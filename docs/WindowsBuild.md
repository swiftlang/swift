# Building Swift on Windows

Visual Studio 2019 or newer is needed to build Swift on Windows, while VS2022 is recommended and currently used for CI.  The free Community edition is sufficient to build Swift, and we're assuming host and target to be both x64.

The commands below (with the exception of installing Visual Studio) must be entered in the **x64 Native Tools Command Prompt for VS2022** (or VS2019 depending on the Visual Studio that you are using) in the Start Menu. This sets environment variables to locate tools and resources for the correct target platform.

> **NOTE:** This guide is intended for toolchain developers who wants to develop or build Swift on their own machine.  For building and packaging a standard toolchain, please refer to [`build-windows-toolchain.bat`](../utils/build-windows-toolchain.bat).

## Enable Developer Mode

From **Settings** app, go to *Update & Security*.  In the *For developers* tab, set *Install apps from any source, including loose files* to On.  This is required to enable the creation of symbolic links.

## Install build tools and SDK

An easy way to get the tools to build Swift is using the [Visual Studio installer](https://www.visualstudio.com/downloads/). The following command installs all needed Visual Studio components as well as Python, Git, CMake and Ninja:

```cmd
curl.exe -sOL https://aka.ms/vs/17/release/vs_community.exe
vs_community ^
  --add Component.CPython39.x64 ^
  --add Microsoft.VisualStudio.Component.Git ^
  --add Microsoft.VisualStudio.Component.VC.ATL ^
  --add Microsoft.VisualStudio.Component.VC.CMake.Project ^
  --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 ^
  --add Microsoft.VisualStudio.Component.Windows11SDK.22000
del /q vs_community.exe
```

> **NOTE:** For anyone who wants to use Visual Studio 2019, you can simply replace version number `17` with `16` in the installer URL.

> **NOTE:** The following [link](https://docs.microsoft.com/visualstudio/install/workload-component-id-vs-build-tools?view=vs-2022) helps in finding the component name given its ID for Visual Studio 2022.  For anyone using VS installer in GUI, please check it out.

### Set up `vcpkg`

This guide uses `vcpkg` for pulling in external dependencies, including ICU, libcurl, libxml2, SQLite 3 and zlib.

All you have to do is to clone and bootstrap `vcpkg`:

```cmd
git clone https://github.com/microsoft/vcpkg S:\vcpkg
S:\vcpkg\bootstrap-vcpkg.bat
```

> **NOTE:** By default, the dependencies will be downloaded and built on demand. Optionally, you can pre-build the dependencies to speed up configuration:
>
>     S:\vcpkg\vcpkg install curl icu libxml2 sqlite3 zlib --triplet=x64-windows

## Clone the source repositories

This guide assumes your sources live at the root of `S:`.  As long as drive letter `S:` is not assigned, you can create a substitution to map your source directory:

```cmd
subst S: <path to source root>
```

First, clone `apple/swift` (this repository) with Git:

```cmd
git clone -c core.autocrlf=input -c core.symlinks=true https://github.com/apple/swift S:\swift
```

You'll be able to clone and check out the rest of Swift source repositories with `update-checkout` tool:

```cmd
S:\swift\utils\update-checkout.cmd --clone --skip-repository swift
```

> **NOTE:** You can use forked versions of Swift source repositories by manually placing them into `S:\<repo-name>` and appending their names to the `--skip-repository` option of `update-checkout`.  Don't forget to clone them with `-c core.autocrlf=input -c core.symlinks=true` and the correct repository name.

## Set up Visual Studio integration (re-run on Visual Studio upgrades)

Set up the `ucrt`, `visualc`, and `WinSDK` modules by:

- copying `ucrt.modulemap` located at `swift/stdlib/public/Platform/ucrt.modulemap` into
  `${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt` as `module.modulemap`
- copying `vcruntime.modulemap` located at `swift/stdlib/public/Platform/vcruntime.modulemap` into `${VCToolsInstallDir}/include` as `module.modulemap`
- copying `winsdk.modulemap` located at `swift/stdlib/public/Platform/winsdk.modulemap` into `${UniversalCRTSdkDir}/Include/${UCRTVersion}/um`
- and setup the `vcruntime.apinotes` located at `swift/stdlib/public/Platform/vcruntime.apinotes` into `${VCToolsInstallDir}/include` as `vcruntime.apinotes`

You're recommended to create symbolic links to avoid manual copying every time you modified these files.

```cmd
mklink "%UniversalCRTSdkDir%\Include\%UCRTVersion%\ucrt\module.modulemap" S:\swift\stdlib\public\Platform\ucrt.modulemap
mklink "%UniversalCRTSdkDir%\Include\%UCRTVersion%\um\module.modulemap" S:\swift\stdlib\public\Platform\winsdk.modulemap
mklink "%VCToolsInstallDir%\include\module.modulemap" S:\swift\stdlib\public\Platform\vcruntime.modulemap
mklink "%VCToolsInstallDir%\include\vcruntime.apinotes" S:\swift\stdlib\public\Platform\vcruntime.apinotes
```

> **WARNING:** Creating the above links usually requires administrator privileges. The quick and easy way to do this is to open a second developer prompt by right clicking whatever shortcut you used to open the first one, choosing "More > Run As Administrator", and pasting the above commands into the resulting window. You can then close the privileged prompt; this is the only step which requires elevation.

## Build for development

The following guide will get you through the building process of a complete Swift debug toolchain.

Before you kick off the process, unset `%SDKROOT%` if you've already installed Swift:

```cmd
set SDKROOT=
```

If you want to use an existing Swift toolchain to build the core libraries and other parts of the toolchain, make sure you have it in `%Path%`, and strip `S:/b/1/bin/` from every `-D CMAKE_{language}_COMPILER=S:/b/1/bin/{compiler}` option.

Here is a graphical overview of all build steps from this guide.

```
┌──────────────────────────────────────┐      ┌────────────────┐
│                                      │      │                │
│       1. LLVM + Swift compiler       │      │                │
│                                      │      │ Existing Swift │
│    ┌────────────────────────────┐    │  or  │   toolchain    │
│    │ 2. Swift runtime libraries │    │      │                │
│    └────────────────────────────┘    │      │                │
└──────────────────────────────────────┘      └────────────────┘
                    │                                  │
                    └───────────┬──────────────────────┘
                                │
            ┌ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
                          ┌──────────────────────┐ │
            │             │    3. libdispatch    │
                          └──────────────────────┘ │
            │     Core    ┌──────────────────────┐
                          │    4. Foundation     │ │
            │  libraries  └──────────────────────┘
                          ┌──────────────────────┐ │
            │             │      5. XCTest       │
                          └──────────────────────┘ │
            └ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
                                │
                                │
                                │
      ┌───────────────────────────────────────────────────┐
      │                                                   │
      │          6. Developer tool dependencies           │
      │                                                   │
      ├─────────┬──────┬─────────────┬─────┬──────────────┤
      │ LLBuild │ Yams │ SwiftSystem │ ··· │ IndexStoreDB │
      └─────────┴──────┴─────────────┴─────┴──────────────┘
                                │
                                │
                 ┌────────────────────────────┐
                 │           7. TSC           │
                 └────────────────────────────┘
                                │
                                │
                                │
            ┌ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
                          ┌──────────────────────┐ │
            │             │   8. Swift Driver    │
                          └──────────────────────┘ │
            │  Developer  ┌──────────────────────┐
                          │      9. SwiftPM      │ │
            │    tools    └──────────────────────┘
                          ┌──────────────────────┐ │
            │             │  10. SourceKit-LSP   │
                          └──────────────────────┘ │
            └ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
```

### 1. Swift compiler

```cmd
cmake -B S:\b\1 ^
  -C S:\swift\cmake\caches\Windows-x86_64.cmake ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy /source-charset:utf-8 /execution-charset:utf-8" ^
  -D CMAKE_CXX_COMPILER=cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy /utf-8" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D LLVM_DEFAULT_TARGET_TRIPLE=x86_64-unknown-windows-msvc ^
  -D LLVM_ENABLE_PDB=YES ^

  -D LLVM_EXTERNAL_CMARK_SOURCE_DIR=S:\cmark ^
  -D LLVM_EXTERNAL_SWIFT_SOURCE_DIR=S:\swift ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=S:\swift-corelibs-libdispatch ^

  -D SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY=YES ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED=YES ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING=YES ^

  -D SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING=YES ^
  -D EXPERIMENTAL_STRING_PROCESSING_SOURCE_DIR=S:\swift-experimental-string-processing ^

  -G Ninja ^
  -S S:\llvm-project\llvm

cmake --build S:\b\1
```

> **NOTE:** Flag `-D LLVM_ENABLE_PDB=YES` enables LLVM to emit PDB files, which is useful for debugging on Windows.  However, linking with debug information is very memory-intensive and may drastically slow down the linking process.  A single link job is possible to consume upwards of 10 GiB of RAM.  You can append `-D LLVM_PARALLEL_LINK_JOBS=N` and `-D SWIFT_PARALLEL_LINK_JOBS=N` to reduce the number of parallel link operations to `N` which should help reduce the memory pressure.
>
> For host with less than 32 GiB of RAM, you may need to disable PDB support by stripping this option, to avoid running into OOM.

> **NOTE:** By default, we enables all the experimental features in Swift by `-D SWIFT_ENABLE_EXPERIMENTAL_{FEATURE}=YES`.  Notice that `Concurrency` is an accepted language feature that is supposed be enabled for Swift 5.5+.

Test Swift:

```cmd
path %PATH%;%ProgramFiles%\Git\usr\bin
cmake --build S:\b\1 --target check-swift
```

### 2. Swift runtime libraries

```cmd
cmake -B S:\b\2 ^
  -C S:\swift\cmake\caches\Runtime-Windows-x86_64.cmake ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\sdk\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D LLVM_DIR=S:\b\1\lib\cmake\llvm ^
  -D SWIFT_NATIVE_SWIFT_TOOLS_PATH=S:\b\1\bin ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=S:\swift-corelibs-libdispatch ^

  -D SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY=YES ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED=YES ^
  -D SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING=YES ^

  -D SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING=YES ^
  -D EXPERIMENTAL_STRING_PROCESSING_SOURCE_DIR=S:\swift-experimental-string-processing ^

  -G Ninja ^
  -S S:\swift

cmake --build S:\b\2
```

> **NOTE:** Swift runtime libraries are also built along with the compiler.  This step extracts them into a portable SDK where we will install core libraries alongside.

### 3. libdispatch

```cmd
cmake -B S:\b\3 ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\sdk\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D ENABLE_SWIFT=YES ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^

  -D BUILD_TESTING=YES ^

  -G Ninja ^
  -S S:\swift-corelibs-libdispatch

cmake --build S:\b\3
```

Test libdispatch:

```cmd
cmake --build S:\b\3 --target test
```

### 4. Foundation (without tests)

```cmd
cmake -B S:\b\4 ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\sdk\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^

  -D CMAKE_TOOLCHAIN_FILE=S:\vcpkg\scripts\buildsystems\vcpkg.cmake ^

  -G Ninja ^
  -S S:\swift-corelibs-foundation

cmake --build S:\b\4
```

### 5. XCTest

```cmd
cmake -B S:\b\5 ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\sdk\usr ^

  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^

  -D ENABLE_TESTING=YES ^
  -D LLVM_DIR=S:\b\1\lib\cmake\llvm ^
  -D LIT_COMMAND=S:\llvm-project\llvm\utils\lit\lit.py ^
  -D XCTEST_PATH_TO_LIBDISPATCH_SOURCE=S:\swift-corelibs-libdispatch ^
  -D XCTEST_PATH_TO_LIBDISPATCH_BUILD=S:\b\3 ^
  -D XCTEST_PATH_TO_FOUNDATION_BUILD=S:\b\4 ^

  -G Ninja ^
  -S S:\swift-corelibs-xctest

cmake --build S:\b\5
```

Test XCTest:

```cmd
path %PATH%;%ProgramFiles%\Git\usr\bin
cmake --build S:\b\5 --target check-xctest
```

### 4.1 Foundation (with tests)

```cmd
cmake -B S:\b\4 ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\sdk\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^

  -D CMAKE_TOOLCHAIN_FILE=S:\vcpkg\scripts\buildsystems\vcpkg.cmake ^

  -D ENABLE_TESTING=YES ^
  -D XCTest_DIR=S:\b\5\cmake\modules ^

  -G Ninja ^
  -S S:\swift-corelibs-foundation

cmake --build S:\b\4
```

Test Foundation:

```cmd
cmake --build S:\b\4 --target test
```

### 6. Tool dependencies

> **NOTE:** We're building the following libraries without tests here because they're independent packages that are directly or indirectly depended by Swift Driver, SwiftPM or SourceKit-LSP.  For developing these libraries, use SwiftPM instead.

#### LLBuild (Used by Swift Driver, SwiftPM and SourceKit-LSP)

```cmd
cmake -B S:\b\6\LLBuild ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy -Xclang -fno-split-cold-code" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D LLBUILD_SUPPORT_BINDINGS=Swift ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^

  -D CMAKE_TOOLCHAIN_FILE=S:\vcpkg\scripts\buildsystems\vcpkg.cmake ^

  -D LIT_EXECUTABLE=S:\llvm-project\llvm\utils\lit\lit.py ^
  -D FILECHECK_EXECUTABLE=S:\b\1\bin\FileCheck.exe ^

  -G Ninja ^
  -S S:\llbuild

cmake --build S:\b\6\LLBuild
```

#### Yams (used by Swift Driver)

```cmd
cmake -B S:\b\6\Yams ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy /DYAML_DECLARE_EXPORT /DWIN32" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^

  -D BUILD_TESTING=NO ^

  -G Ninja ^
  -S S:\yams

cmake --build S:\b\6\Yams
```

#### Argument Parser (used by Swift Driver and SwiftPM)

```cmd
cmake -B S:\b\6\ArgumentParser ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^

  -D BUILD_EXAMPLES=NO ^
  -D BUILD_TESTING=NO ^

  -G Ninja ^
  -S S:\swift-argument-parser

cmake --build S:\b\6\ArgumentParser
```

#### Swift System (used by Swift Driver and SwiftPM)

```cmd
cmake -B S:\b\6\SwiftSystem ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^

  -G Ninja ^
  -S S:\swift-system

cmake --build S:\b\6\SwiftSystem
```

#### Swift Crypto (used by SwiftPM)

```cmd
cmake -B S:\b\6\SwiftCrypto ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^

  -G Ninja ^
  -S S:\swift-crypto

cmake --build S:\b\6\SwiftCrypto
```

#### Swift Collections (used by SwiftPM)

```cmd
cmake -B S:\b\6\SwiftCollections ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^

  -D BUILD_TESTING=NO ^

  -G Ninja ^
  -S S:\swift-collections

cmake --build S:\b\6\SwiftCollections
```

#### IndexStoreDB (used by SourceKit-LSP)

```cmd
cmake -B S:\b\6\IndexStoreDB ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy -Xclang -fno-split-cold-code" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^

  -D BUILD_TESTING=NO ^

  -G Ninja ^
  -S S:\indexstore-db

cmake --build S:\b\6\IndexStoreDB
```

### 7. TSC (without tests)

```cmd
cmake -B S:\b\7 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^
  -D SwiftSystem_DIR=S:\b\6\SwiftSystem\cmake\modules ^

  -D CMAKE_TOOLCHAIN_FILE=S:\vcpkg\scripts\buildsystems\vcpkg.cmake ^

  -G Ninja ^
  -S S:\swift-tools-support-core

cmake --build S:\b\7
```

### 8. Swift Driver (without tests)

```cmd
cmake -B S:\b\8 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^
  -D LLBuild_DIR=S:\b\6\LLBuild\cmake\modules ^
  -D Yams_DIR=S:\b\6\Yams\cmake\modules ^
  -D SwiftSystem_DIR=S:\b\6\SwiftSystem\cmake\modules ^
  -D ArgumentParser_DIR=S:\b\6\ArgumentParser\cmake\modules ^
  -D TSC_DIR=S:\b\7\cmake\modules ^

  -G Ninja ^
  -S S:\swift-driver

cmake --build S:\b\8
```

### 9. SwiftPM (without tests)

```cmd
cmake -B S:\b\9 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^
  -D LLBuild_DIR=S:\b\6\LLBuild\cmake\modules ^
  -D SwiftSystem_DIR=S:\b\6\SwiftSystem\cmake\modules ^
  -D ArgumentParser_DIR=S:\b\6\ArgumentParser\cmake\modules ^
  -D SwiftCrypto_DIR=S:\b\6\SwiftCrypto\cmake\modules ^
  -D SwiftCollections_DIR=S:\b\6\SwiftCollections\cmake\modules ^
  -D TSC_DIR=S:\b\7\cmake\modules ^
  -D SwiftDriver_DIR=S:\b\8\cmake\modules ^

  -G Ninja ^
  -S S:\swiftpm

cmake --build S:\b\9
```

### 10. SourceKit-LSP (without tests)

```cmd
cmake -B S:\b\10 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\3\cmake\modules ^
  -D Foundation_DIR=S:\b\4\cmake\modules ^
  -D LLBuild_DIR=S:\b\6\LLBuild\cmake\modules ^
  -D SwiftSystem_DIR=S:\b\6\SwiftSystem\cmake\modules ^
  -D ArgumentParser_DIR=S:\b\6\ArgumentParser\cmake\modules ^
  -D SwiftCollections_DIR=S:\b\6\SwiftCollections\cmake\modules ^
  -D IndexStoreDB_DIR=S:\b\6\IndexStoreDB\cmake\modules ^
  -D TSC_DIR=S:\b\7\cmake\modules ^
  -D SwiftPM_DIR=S:\b\9\cmake\modules ^

  -G Ninja ^
  -S S:\sourcekit-lsp

cmake --build S:\b\10
```

## Use the toolchain and SDK

If you want a toolchain for real-world testing, you can use CMake to perform the install step.  However, the built toolchain is not expected to be distributed, mainly because there're plenty of unnecessary or local files that need to be stripped.

### Swift compiler

```cmd
cmake --build S:\b\1 --target install
```

For testing, add the toolchain to path:

```cmd
path S:\b\toolchain\usr\bin;%PATH%
```

> **NOTE**: The built compiler cannot actually compile Swift files without a compatible SDK. You may need to specify `-sdk <sdk>` explicitly.

### Swift Windows SDK (with core libraries)

```cmd
cmake --build S:\b\2 --target install
cmake --build S:\b\3 --target install
cmake --build S:\b\4 --target install
cmake --build S:\b\5 --target install
```

Usually, you also need to bundle external libraries required by `Foundation`:

```cmd
copy /Y S:\b\4\bin\*.dll S:\b\sdk\usr\bin
```

For testing, set `%SDKROOT%` and add runtime libraries to path:

```cmd
set SDKROOT=S:\b\sdk
path S:\b\sdk\usr\bin;%PATH%
```

#### Fix SDK layout

You may notice that the SDK is not functioning correctly. This is caused by the mismatch of expected SDK layout and the build output. Before these issues get addressed, we may need some extra workaround.

1. Compiler expects Swift libraries to live in `\usr\lib\swift\windows\x86_64`, but the build output is `\usr\lib\swift\windows`. This is likely to trigger `unable to load standard library for target 'x86_64-unknown-windows-msvc'` error. Fix by:

```cmd
for /D %m in (S:\b\sdk\usr\lib\swift\windows\*.swiftmodule) do ^
move /Y %m S:\b\sdk\usr\lib\swift\windows\x86_64
```

> **NOTE(stevapple)**: The compiler convention shall be wrong here — Swift libraries can be bundled with multiple architectures, so there's no need to place them in arch-specific directory.

2. Compiler expects underlying modules to be imported from `\usr\include`, but the build output is `\usr\lib\swift`. This is likely to trigger `cannot load underlying module for '<Module>'` error. Fix by:

```cmd
move /Y S:\b\sdk\usr\lib\swift\Block S:\b\sdk\usr\include
move /Y S:\b\sdk\usr\lib\swift\dispatch S:\b\sdk\usr\include
move /Y S:\b\sdk\usr\lib\swift\os S:\b\sdk\usr\include
```

3. Compiler expects Swift runtime import libraries to live in `\usr\lib\swift\windows\x86_64`, but the build output is `\usr\lib\swift\windows`. This is likely to trigger `fatal error LNK1104: cannot open file '<Module>.lib'` error. Fix by:

```cmd
move /Y S:\b\sdk\usr\lib\swift\windows\*.lib S:\b\sdk\usr\lib\swift\windows\x86_64
```

> **NOTE(stevapple)**: The build script shall be wrong here — Swift shared libraries are closely tied to target architecture, so we should place them in arch-specific directory if Universal binary is not available.

### Swift developer tools

```cmd
cmake --build S:\b\6\LLBuild --target install
cmake --build S:\b\6\Yams --target install
cmake --build S:\b\6\ArgumentParser --target install
cmake --build S:\b\6\SwiftSystem --target install
cmake --build S:\b\6\SwiftCrypto --target install
cmake --build S:\b\6\SwiftCollections --target install
cmake --build S:\b\6\IndexStoreDB --target install
cmake --build S:\b\7 --target install
cmake --build S:\b\8 --target install
cmake --build S:\b\9 --target install
cmake --build S:\b\10 --target install
copy /Y S:\b\7\bin\sqlite3.dll S:\b\toolchain\usr\bin
```

To use Swift Driver in place of the old driver (default since Swift 5.7):

```cmd
copy /Y S:\b\toolchain\usr\bin\swift-driver.exe S:\b\toolchain\usr\bin\swift.exe
copy /Y S:\b\toolchain\usr\bin\swift-driver.exe S:\b\toolchain\usr\bin\swiftc.exe
```

You should be able to compile Swift packages without any additional steps.
