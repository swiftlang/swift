# Windows Toolchain

## Nomenclature

Different terms are used to reference the different portions of the tree.  These help identify the various uses for the content in the tree.

* **Runtime** (`%ProgramFiles%\Swift\Runtimes\0.0.0\...`)

This is the *runtime* component. It contains the libraries which are needed by the user to execute code written in Swift. These set of libraries must be redistributed with any Swift application or must be provided by the Operating System (e.g. as with macOS). This usually contains the Swift runtime and standard libraries. It may optionally require Foundation if it is used by the application. Only one copy of the runtime component is required, though each application may bundle its own requisite subset.

* **SDK** (`%ProgramFiles%\Platforms\Windows.platform\Developer\SDKs\Windows.sdk\...`)

More than one SDK may be installed at a time. There is no requirement that the platform SDK match the host. However, the host SDK is likely required as dependency on Swift within the Swift compiler grows. The platform SDK may support multiple architectures. This content is used by the developer to build products for the specific platform for any set of architectures it supports.

* **Toolchain** (`%ProgramFiles%\Toolchains\0.0.0+Asserts\...`)

More than one toolchain may be installed at a time. The toolchains are labelled by a SemVer number and a variant (e.g. `+Asserts`, `+NoAsserts`). This allows for installing multiple versions of the toolchain in parallel including variations on a single version. The toolchain contains all the tools required to build C, C++, and Swift code (compiler, assembler, linker) as well as to debug the products (debugger). It also contains binary inspection tools (e.g. `objdump`). This enables a single installation of the toolchain to be sufficient to build most applications and their dependencies.

* **Redistributables** (`%ProgramFiles%\Redistributables\0.0.0\...`)

The redistributables are associated with the SDK and contain MSMs (MSI Merge Modules) that allow a package vendor to integrate the runtime component to be distributed along with the application.

* **Developer Tools** (`%ProgramFiles%\Tools\...`)

Additional tools such as the code formatter are installed into the developer tools to allow for adjunct utilities to be distributed with the toolchain.

## Layout

The Windows Swift Toolset is only supported on 64-bit OSes.  The toolset is installed into `%ProgramFiles%` by default when performing a system-wide installation and into `%LocalAppData%\Programs` when installed per-user.

The various components are installed to the vendor subdirectory (`Swift` in the case of the swift.org releases).

## Environment Variables

- `SDKROOT`

This is the root of the SDK. If available, the value of this environment variable will be used as the default value for the `-sdk` parameter to the Swift compiler. Without this, the compiler will not be able to locate the necessary resources to perform compilation. The user may override the value by specifying `-sdk` manually.

## Packaging

The Swift Tool Kit is distributed as a standalone installer.  The installer comprises of multiple MSI (Microsoft Installer) packages. This allows for a selective installation of components.

- `bld` (Build Tools)

Core tools required to build software. This is primarily the compilers, assemblers, linkers, and compiler resources. It also contains the binary inspection tools.

- `cli` (CLI Tools)

Extra command line utilities that are necessary for building software. This includes things like the Swift Package Manager. It also includes tools which may be useful for CI/CD purposes (e.g. `clang-tidy`).

- `dbg` (Debugging Tools)

This contains the debugger and any related tools.

- `ide` (IDE Integration Tools)

This contains the IDE integration tools (e.g. SourceKit-LSP).

- `sdk` (Platform SDK)

There are multiple SDKs which are built and available. The Windows toolkit includes the SDKs for the Windows platform for the currently supported architectures (x86, x64, ARM64).  It also contains the MSMs to allow the redistribution of the runtime library as part of a software distribution.

- `rtl` (Platform Runtime Library)

There are multiple runtime library packages, but only the runtime library for the current host is installed. The runtime requirement for redistribution is addressed through the redistributable MSM packages distributed through the platform SDK.
