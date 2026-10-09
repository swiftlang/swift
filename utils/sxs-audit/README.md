# SxS Audit

`sxs-audit` computes the private runtime closure required by groups of Windows
PE files. It can also compare those requirements with authored runtime
features.

The scan root contains package-provided EXEs and DLLs. Private assemblies use
the Windows SxS layout `<root>\<assembly>\<assembly>.dll`. The runtime root is a
complete flat directory of candidate runtime DLLs from which the package
closures are trimmed.

The executable is Windows-only. It uses:

- `FoundationEssentials.URL`, `FileManager`, and mapped `Data` for files;
- `WinSDK` and DbgHelp to inspect PE images in-place;
- `swift-argument-parser` for the command-line interface;
- the Swift standard library for dependency closure and report emission.

## Run

The analyzer groups PE files by an arbitrary package name:

- `--root` is the directory containing the EXEs and DLLs to inspect.
- `--runtime-root` is a flat directory containing every DLL that may be
  selected for the private runtime.
- `--file PACKAGE=FILE` assigns an EXE or DLL from the scan root to `PACKAGE`.
  Repeat it for every file provided by that package. The analyzer combines
  their dependencies into one runtime closure.

For example, the following invocation calculates separate closures for a
compiler package and a debugger package. `compiler.exe` and
`CompilerSupport.dll` both contribute to the `compiler` closure.

```powershell
swift run `
  --package-path C:\path\to\sxs-audit `
  sxs-audit `
  --root C:\path\to\image\bin `
  --runtime-root C:\path\to\runtime\bin `
  --file compiler=compiler.exe `
  --file compiler=CompilerSupport.dll `
  --file debugger=debugger.exe
```

`compiler` and `debugger` are user-defined package names. The file names
identify direct children of the scan root. Both roots are required; the
analyzer makes no assumptions about a toolchain or its installation layout.

To audit installer authoring, `--feature PACKAGE=FEATURE` associates a package
with the name of its runtime feature. Each `--authored PACKAGE=ASSEMBLY` then
declares an assembly already present in that feature:

```powershell
  --feature compiler=CompilerRuntime `
  --authored compiler=Core `
  --authored compiler=Dispatch
```

Assembly values are DLL names without the `.dll` extension. If no
`--feature` or `--authored` options are supplied, the analyzer only reports the
required closures.

Use `--exclude-runtime-dll NAME` to add an exact DLL name or a trailing-`*`
prefix pattern to the default VC runtime and non-shipping test-runtime
exclusions.

## Swift installer adapter

`Invoke-SwiftToolchainSxSAudit.ps1` extracts package provisions and PRT feature
memberships from the Swift installer authoring:

```powershell
C:\path\to\swift\utils\sxs-audit\Invoke-SwiftToolchainSxSAudit.ps1 `
  -ToolchainRoot $toolchain
```

By default, the script finds `swift-installer-scripts` beside the Swift
repository and derives the matching flat runtime from the installed toolchain.
Use `-InstallerRoot` or `-RuntimeRoot` to override either location, or `-Swift`
to select a different `swift` executable.

Configure `SDKROOT` for that Swift installation. Its flat runtime directory
must also be on `PATH` so SwiftPM can run temporary manifest executables.

## Output

The report is CSV with the columns `record`, `package`, `subject`, `category`,
and `value`. File records describe direct runtime roots and their transitive
closures. Package records compare required and authored assemblies. Diagnostic
records identify missing provisions, missing authored features, incomplete flat
runtimes, unowned runtime-dependent files, and unused runtime assemblies. An
empty category produces no rows.
