/-
Copyright (c) 2026. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Cache.Lean

public section

namespace Cache.Unpacked

open System (FilePath)

/-- One inherited setting enables sharing for all downstream projects. -/
def enabled : IO Bool := do
  match ← IO.getEnv "MATHLIB_CACHE_UNPACKED" with
  | none | some "" | some "0" => return false
  | some "1" =>
    if System.Platform.isWindows then
      throw <| IO.userError "MATHLIB_CACHE_UNPACKED currently requires POSIX symlinks and chmod"
    return true
  | some value =>
    throw <| IO.userError s!"Invalid MATHLIB_CACHE_UNPACKED={value}; expected 0 or 1"

private def run (cmd : String) (args : Array String) : IO String := do
  let out ← IO.Process.output { cmd, args }
  unless out.exitCode == 0 do
    throw <| IO.userError s!"{cmd} failed ({out.exitCode}): {out.stderr}\n{out.stdout}"
  return out.stdout.trimAscii.toString

def entryPath (cache : FilePath) (key : UInt64) : FilePath :=
  cache / "builds-v2" / key.asLTar

/-- Count regular files rather than keeping a per-artifact attachment manifest. -/
private def fileCount (build : FilePath) : IO Nat := do
  let mut count := 0
  for path in ← build.walkDir do
    match (← path.symlinkMetadata).type with
    | .file => count := count + 1
    | .dir => pure ()
    | _ => throw <| IO.userError s!"Unexpected file type in build snapshot: {path}"
  return count

/-- Published directories are immutable. Missing outputs are errors, not cache hits. -/
def readEntry (entry : FilePath) : IO Bool := do
  if !(← entry.pathExists) then return false
  let expected ← IO.FS.readFile (entry / "files")
  let actual ← fileCount (entry / "build")
  unless actual > 0 && expected == toString actual do
    throw <| IO.userError s!"Incomplete build snapshot: {entry}; detach and rebuild privately"
  return true

/-- Removing the reference never traverses the canonical build directory. -/
def detach (base : FilePath) : IO Bool := do
  let build := base / ".lake/build"
  let metadata ← try some <$> build.symlinkMetadata catch
    | .noFileOrDirectory .. => pure none
    | e => throw e
  if metadata.any (·.type == .symlink) then
    let source : FilePath ← run "readlink" #["--", build.toString]
    unless source.fileName == some "build" &&
        (source.parent.bind (·.parent) |>.bind (·.fileName)) == some "builds-v2" do
      return false
    IO.FS.removeFile build
    IO.println s!"Detached {build}; subsequent builds are private"
    return true
  return false

/-- Atomically install one directory reference, replacing only this package's build outputs. -/
def attach (entry base : FilePath) : IO Unit := do
  let source ← IO.FS.realPath (entry / "build")
  let build := base / ".lake/build"
  if ← build.pathExists then
    if (← build.symlinkMetadata).type == .symlink then
      if (← IO.FS.realPath build) == source then return
  IO.FS.createDirAll (base / ".lake")
  let temp : FilePath ← run "mktemp" #["-d", (base / ".lake/.cache-attach.XXXXXXXXXX").toString]
  try
    discard <| run "ln" #["-s", "--", source.toString, (temp / "link").toString]
    try
      IO.FS.rename build (temp / "previous")
    catch
      | .noFileOrDirectory .. => pure ()
      | e => throw e
    IO.FS.rename (temp / "link") build
  finally
    if !(← build.pathExists) && (← (temp / "previous").pathExists) then
      IO.FS.rename (temp / "previous") build
    -- Unlike recursive directory traversal, rm does not follow directory symlinks.
    discard <| run "rm" #["-rf", "--", temp.toString]

/-- Capture a complete prepared build, including its ordinary cache executable. Publication
is a directory rename; failed or concurrent preparations cannot modify a published tree. -/
def prepare (cache : FilePath) (key : UInt64) (build : FilePath) : IO FilePath := do
  let entry := entryPath cache key
  if ← readEntry entry then return entry
  let root := cache / "builds-v2"
  IO.FS.createDirAll root
  let staging : FilePath ← run "mktemp" #["-d", (root / ".tmp.XXXXXXXXXX").toString]
  try
    discard <| run "cp" #["-RL", "--", build.toString, (staging / "build").toString]
    let count ← fileCount (staging / "build")
    unless count > 0 do throw <| IO.userError "Cannot publish an empty build snapshot"
    IO.FS.writeFile (staging / "files") (toString count)
    discard <| run "chmod" #["-R", "a-w", "--", staging.toString]
    try
      IO.FS.rename staging entry
    catch
      | e@(.alreadyExists ..) | e@(.unsatisfiedConstraints ..) =>
        if !(← readEntry entry) then throw e
      | e => throw e
    return entry
  finally
    if ← staging.pathExists then
      discard <| run "chmod" #["-R", "u+w", "--", staging.toString]
      IO.FS.removeDirAll staging

end Cache.Unpacked
