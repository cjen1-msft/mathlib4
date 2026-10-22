/-
Copyright (c) 2026. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Cache.Lean

public section

namespace Cache.Unpacked

open System (FilePath)

/-- Experimental, POSIX-only sharing of unpacked Mathlib dependency outputs. -/
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
    throw <| IO.userError s!"{cmd} failed ({out.exitCode}): {out.stderr}"
  return out.stdout.trimAscii.toString

/-- Versioned separately from the archive format; never used as a mutable build directory. -/
def entryPath (cache : FilePath) (archive : FilePath) : FilePath :=
  cache / "unpacked-v1" / archive.fileStem.getD archive.toString

/-- Only compiler payloads are shared. Traces, hashes, and other bookkeeping remain private. -/
def isPayload (path : FilePath) : Bool :=
  [".olean", ".olean.server", ".olean.private", ".ilean", ".ir", ".ir.sig", ".c"].any
    (path.toString.endsWith ·)

private def safePath (path : FilePath) : Bool :=
  !path.isAbsolute && !(path.components.contains "..") &&
    (path.toString.startsWith ".lake/build/lib/lean/Mathlib/" ||
     path.toString.startsWith ".lake/build/ir/Mathlib/")

/-- An entry is visible only after its complete directory has been atomically published. -/
def readEntry (entry : FilePath) : IO (Option (Array FilePath)) := do
  if !(← entry.pathExists) then return none
  let manifest := entry / "manifest.json"
  let json ← IO.ofExcept <| Lean.Json.parse (← IO.FS.readFile manifest)
  let paths ← IO.ofExcept <| Lean.fromJson? (α := Array String) json
  let paths := paths.map FilePath.mk
  if paths.isEmpty then
    throw <| IO.userError s!"Empty unpacked cache entry: {entry}"
  for path in paths do
    unless safePath path && (← (entry / path).pathExists) do
      throw <| IO.userError s!"Incomplete or invalid unpacked cache entry: {entry / path}"
  return some paths

private def removeTemp (path : FilePath) : IO Unit := do
  try IO.FS.removeFile path
  catch
    | .noFileOrDirectory .. => pure ()
    | e => throw e

private def replace (source target : FilePath) (link : Bool) : IO Unit := do
  if let some parent := target.parent then IO.FS.createDirAll parent
  let temp := target.addExtension s!"cache-{← IO.Process.getPID}.tmp"
  -- A failed prior attachment can leave a temporary symlink, including a dangling one.
  removeTemp temp
  try
    if link then
      discard <| run "ln" #["-s", "--", source.toString, temp.toString]
    else
      IO.FS.writeBinFile temp (← IO.FS.readBinFile source)
    IO.FS.rename temp target
  finally
    removeTemp temp

/-- Attach immutable payloads individually; never link a build directory or mutable metadata.
When `privateCopy` is set, detach payloads as well, before a forced extraction can overwrite them. -/
def attach (entry base : FilePath) (paths : Array FilePath) (privateCopy := false) : IO Unit := do
  unless privateCopy do
    -- A local rebuild or a forced fetch owns its regular payloads. Do not adopt them
    -- into the store or replace them on a later ordinary read.
    for path in paths.filter isPayload do
      if ← (base / path).pathExists then
        if (← (base / path).symlinkMetadata).type == .file then return
  let entry ← IO.FS.realPath entry
  -- Install traces last, so interrupted attachment is not mistaken for a complete restore.
  for path in paths.filter (·.extension != some "trace") do
    replace (entry / path) (base / path) (isPayload path && !privateCopy)
  for path in paths.filter (·.extension == some "trace") do
    replace (entry / path) (base / path) false

/-- Collect only files beneath the supported Mathlib build layout. -/
def extractedPaths (staging : FilePath) : IO (Array FilePath) := do
  let mut paths := #[]
  for path in ← staging.walkDir do
    if (← path.symlinkMetadata).type == .file && safePath (path.withoutParent staging) then
      paths := paths.push (path.withoutParent staging)
  unless paths.any (·.extension == some "trace") &&
      paths.any (·.extension == some "olean") do
    throw <| IO.userError s!"Incomplete Mathlib extraction in {staging}"
  return paths

/-- Prepare privately and publish with directory rename. Concurrent losers reuse the winner;
an interrupted preparation leaves only an ignored `.tmp.*` directory. Existing entries are
never replaced, including by force-fetch. The callback extracts only into the staging directory. -/
def prepare (cache archive : FilePath) (extract : FilePath → IO Unit) :
    IO (FilePath × Array FilePath) := do
  let entry := entryPath cache archive
  if let some paths ← readEntry entry then return (entry, paths)
  let root := cache / "unpacked-v1"
  IO.FS.createDirAll root
  let staging : FilePath ← run "mktemp" #["-d", (root / ".tmp.XXXXXXXXXX").toString]
  try
    extract staging
    let paths ← extractedPaths staging
    for path in paths do
      unless (← (staging / path).metadata).type == .file do
        throw <| IO.userError s!"Unexpected unpacked cache file: {staging / path}"
      if isPayload path then
        discard <| run "chmod" #["a-w", "--", (staging / path).toString]
    IO.FS.writeFile (staging / "manifest.json") (Lean.toJson (paths.map (·.toString))).compress
    try
      IO.FS.rename staging entry
    catch
      | e@(.alreadyExists ..) | e@(.unsatisfiedConstraints ..) =>
        if !(← entry.pathExists) then throw e
        -- rename cannot replace a populated directory. Validate the concurrent winner.
        discard <| readEntry entry
      | e => throw e
    let some paths ← readEntry entry
      | throw <| IO.userError s!"Unpacked cache publication disappeared: {entry}"
    return (entry, paths)
  finally
    if ← staging.pathExists then IO.FS.removeDirAll staging

end Cache.Unpacked
