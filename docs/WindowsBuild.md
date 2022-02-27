# Building Swift on Windows

Visual Studio 2017 or newer is needed to build Swift on Windows, while VS2019 is recommended and currently used for CI.  The free Community edition is sufficient to build Swift, and we're assuming host and target to be both x64.

The commands below (with the exception of installing Visual Studio) must be entered in the **x64 Native Tools Command Prompt for VS2019** (or VS2017, VS2022 depending on the Visual Studio that you are using) in the Start Menu. This sets environment variables to select the correct target platform.

> **NOTE:** This guide is intended for toolchain developers who wants to develop or build Swift on their own machine.  For building and packaging a standard toolchain, please refer to [`build-windows-toolchain.bat`](../utils/build-windows-toolchain.bat).

## Install dependencies

### Visual Studio

An easy way to get most of the tools to build Swift is using the [Visual Studio installer](https://www.visualstudio.com/downloads/). This command installs all needed Visual Studio components as well as Python, Git, CMake and Ninja:

```
curl.exe -sOL https://aka.ms/vs/16/release/vs_community.exe
vs_community ^
  --add Component.CPython3.x64 ^
  --add Microsoft.VisualStudio.Component.Git ^
  --add Microsoft.VisualStudio.Component.VC.ATL ^
  --add Microsoft.VisualStudio.Component.VC.CMake.Project ^
  --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 ^
  --add Microsoft.VisualStudio.Component.Windows10SDK.19041
del /q vs_community.exe
```

> **NOTE:** For anyone who wants to use Visual Studio 2022 instead:
> - replace `https://aka.ms/vs/16/release/vs_community.exe` with `https://aka.ms/vs/17/release/vs_community.exe`
> - replace `Component.CPython3.x64` with `Component.CPython39.x64`

If you prefer you can install everything by hand, but make sure to include "Programming Languages|Visual C++" and "Windows and Web Development|Universal Windows App Development|Windows SDK" in your installation. The components listed above are required.

The following [link](https://docs.microsoft.com/visualstudio/install/workload-component-id-vs-build-tools?view=vs-2019) helps in finding the component name given its ID for Visual Studio 2019.

### Python

The command above already installs Python 3. Alternatively, in the Visual Studio installation program, under *Individual Components*, install *Python 3 64 bits (3.x.x)*.

If you are building a debug version of Swift, you should also install the Python debug binaries.

1. In the Windows settings, go to *Add and Remove Programs*
2. Select the *Python 3.x.x (64-bit)* entry
3. Click *Modify*, then *Yes*, then *Modify* again and then *Next*
4. Select *Download debug binaries (requires VS 2015 or later)*
5. Click *Install*

## Enable Developer Mode

From the settings application, go to *Update & Security*.  In the *For developers* tab, select *Developer Mode* for *Use Developer Features*.  This is required to enable the creation of symbolic links.

## Clone the repositories

> **NOTE:** This guide assumes your sources live at the root of `S:`. If your sources live elsewhere, you can create a substitution for this:
>
>     subst S: <path to sources>

First, clone `apple/swift` (this repository) with Git:

```cmd
git clone -c core.autocrlf=input -c core.symlinks=true https://github.com/apple/swift S:\swift
```

You'll be able to clone and check out the rest of Swift source repositories with `update-checkout` tool:

```cmd
S:\swift\utils\update-checkout.cmd --clone
```

## Set up `vcpkg`

This guide uses `vcpkg` for pulling in external dependencies, including ICU, libcurl, libxml2, SQLite 3 and zlib.

All you have to do is to clone and bootstrap `vcpkg`:

```cmd
git clone https://github.com/microsoft/vcpkg S:\vcpkg
S:\vcpkg\bootstrap-vcpkg.bat
```

By default, the dependencies will be downloaded and built on demand.  Optionally, you can prebuild the dependencies in advance:

```cmd
S:\vcpkg\vcpkg install curl icu libxml2 sqlite3 zlib --triplet=x64-windows
```

## Set up Visual Studio integration (re-run on Visual Studio upgrades)

Set up the `ucrt`, `visualc`, and `WinSDK` modules by:

- copying `ucrt.modulemap` located at `swift/stdlib/public/Platform/ucrt.modulemap` into
  `${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt` as `module.modulemap`
- copying `visualc.modulemap` located at `swift/stdlib/public/Platform/visualc.modulemap` into `${VCToolsInstallDir}/include` as `module.modulemap`
- copying `winsdk.modulemap` located at `swift/stdlib/public/Platform/winsdk.modulemap` into `${UniversalCRTSdkDir}/Include/${UCRTVersion}/um`
- and setup the `visualc.apinotes` located at `swift/stdlib/public/Platform/visualc.apinotes` into `${VCToolsInstallDir}/include` as `visualc.apinotes`

You're recommended to create symbolic links to avoid manual copying every time you modified these files.

```cmd
mklink "%UniversalCRTSdkDir%\Include\%UCRTVersion%\ucrt\module.modulemap" S:\swift\stdlib\public\Platform\ucrt.modulemap
mklink "%UniversalCRTSdkDir%\Include\%UCRTVersion%\um\module.modulemap" S:\swift\stdlib\public\Platform\winsdk.modulemap
mklink "%VCToolsInstallDir%\include\module.modulemap" S:\swift\stdlib\public\Platform\visualc.modulemap
mklink "%VCToolsInstallDir%\include\visualc.apinotes" S:\swift\stdlib\public\Platform\visualc.apinotes
```

> **WARNING:** Creating the above links usually requires administrator privileges. The quick and easy way to do this is to open a second developer prompt by right clicking whatever shortcut you used to open the first one, choosing "More > Run As Administrator", and pasting the above commands into the resulting window. You can then close the privileged prompt; this is the only step which requires elevation.

## Build a minimal Swift toolchain

A minimal Swift toolchain comes with neither optional features nor SwiftPM stuffs, and is useful for playing with the compiler or language itself.

```cmd
cmake -B S:\b\1 ^
  -C S:\swift\cmake\caches\Windows-x86_64.cmake ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr ^
  -D CMAKE_C_COMPILER=cl ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy /source-charset:utf-8 /execution-charset:utf-8" ^
  -D CMAKE_CXX_COMPILER=cl ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy /utf-8" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D LLVM_DEFAULT_TARGET_TRIPLE=x86_64-unknown-windows-msvc ^
  -D LLVM_APPEND_VC_REV=NO ^
  -D LLVM_EXTERNAL_CMARK_SOURCE_DIR=S:\cmark ^
  -D LLVM_EXTERNAL_SWIFT_SOURCE_DIR=S:\swift ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=S:\swift-corelibs-libdispatch ^
  -G Ninja ^
  -S S:\llvm-project\llvm

cmake --build S:\b\1
cmake --build S:\b\1 --target install
```

## Build a complete toolchain for development

The following guide will get you through the building process of a complete Swift debug toolchain.

Before you kick off the process, unset `SDKROOT` if you've already installed Swift:

```cmd
set SDKROOT=
```

### Swift compiler

```cmd
cmake -B S:\b\1 ^
  -C S:\swift\cmake\caches\Windows-x86_64.cmake ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=S:\b\toolchain\usr ^

  -D CMAKE_C_COMPILER=cl ^
  -D CMAKE_C_FLAGS="/GS- /Oy /Gw /Gy /source-charset:utf-8 /execution-charset:utf-8" ^
  -D CMAKE_CXX_COMPILER=cl ^
  -D CMAKE_CXX_FLAGS="/GS- /Oy /Gw /Gy /utf-8" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_EXE_LINKER_FLAGS="/INCREMENTAL:NO" ^
  -D CMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO" ^

  -D LLVM_DEFAULT_TARGET_TRIPLE=x86_64-unknown-windows-msvc ^

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

> **NOTE:** If you want to profile the Swift compiler, you may need to link it with debug information.  You can enable this by specifying `-D LLVM_ENABLE_PDB=YES`.
>
> Linking with debug information is very memory-intensive and may drastically slow down the linking process.  A single link job is possible to consume upwards of 10 GiB of RAM.  You can append `-D LLVM_PARALLEL_LINK_JOBS=N` to reduce the number of parallel link operations to `N` which should help reduce the memory pressure.

> **NOTE:** By default, we enables all the experimental features in Swift by `-D SWIFT_ENABLE_EXPERIMENTAL_{FEATURE}=YES`.  Notice that `Concurrency` is an accepted language feature that should be enabled for Swift 5.5+.

Test Swift:

```cmd
path %PATH%;%ProgramFiles%\Git\usr\bin
cmake --build S:\b\1 --target check-swift
```

### Swift Standard Library

```cmd
cmake -B S:\b\2 ^
  -C S:\swift\cmake\caches\Runtime-Windows-x86_64.cmake ^
  -D CMAKE_BUILD_TYPE=RelWithDebInfo ^
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

> **NOTE:** Swift Standard Library is also built along with the compiler.  This step extracts it into a portable SDK where we will install other runtime libraries.

### libdispatch

```cmd
cmake -B S:\b\3 ^
  -D CMAKE_BUILD_TYPE=RelWithDebInfo ^
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

### Foundation (without tests)

```cmd
cmake -B S:\b\4 ^
  -D CMAKE_BUILD_TYPE=RelWithDebInfo ^
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

### XCTest

```cmd
cmake -B S:\b\5 ^
  -D CMAKE_BUILD_TYPE=RelWithDebInfo ^
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

### Foundation (with tests)

```cmd
cmake -B S:\b\4 ^
  -D CMAKE_BUILD_TYPE=RelWithDebInfo ^
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

### TSC

```cmd
cmake -B S:\b\5 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=RelWithDebInfo ^
  -D CMAKE_INSTALL_PREFIX=C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\2\cmake\modules ^
  -D Foundation_DIR=S:\b\3\cmake\modules ^
  -D SQLite3_INCLUDE_DIR=S:\Library\sqlite-3.28.0\usr\include ^
  -D SQLite3_LIBRARY=S:\Library\sqlite-3.28.0\usr\lib\SQLite3.lib ^
  -G Ninja ^
  -S S:\swift-tools-support-core

ninja -C S:\b\5
```

### llbuild

```cmd
cmake -B S:\b\6 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=RelWithDebInfo ^
  -D CMAKE_INSTALL_PREFIX=C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr ^
  -D CMAKE_CXX_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_CXX_FLAGS="-Xclang -fno-split-cold-code" ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D LLBUILD_SUPPORT_BINDINGS=Swift ^
  -D dispatch_DIR=S:\b\2\cmake\modules ^
  -D Foundation_DIR=S:\b\3\cmake\modules ^
  -D SQLite3_INCLUDE_DIR=S:\Library\sqlite-3.28.0\usr\include ^
  -D SQLite3_LIBRARY=S:\Library\sqlite-3.28.0\usr\lib\sqlite3.lib ^
  -G Ninja ^
  -S S:\swift-llbuild

ninja -C S:\b\6
```

Add llbuild to your path:

```cmd
path S:\b\6\bin;%PATH%
```

### Yams

```cmd
cmake -B S:\b\7 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\2\cmake\modules ^
  -D Foundation_DIR=S:\b\3\cmake\modules ^
  -D XCTest_DIR=S:\b\4\cmake\modules ^
  -G Ninja ^
  -S S:\Yams

ninja -C S:\b\7
```

### ArgumentParser

```cmd
cmake -B S:\b\8 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\2\cmake\modules ^
  -D Foundation_DIR=S:\b\3\cmake\modules ^
  -D XCTest_DIR=S:\b\4\cmake\modules ^
  -G Ninja ^
  -S S:\swift-argument-parser

ninja -C S:\b\8
```

### SwiftDriver

```cmd
cmake -B S:\b\9 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_INSTALL_PREFIX=C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\2\cmake\modules ^
  -D Foundation_DIR=S:\b\3\cmake\modules ^
  -D TSC_DIR=S:\b\5\cmake\modules ^
  -D LLBuild_DIR=S:\b\6\cmake\modules ^
  -D Yams_DIR=S:\b\7\cmake\modules ^
  -D ArgumentParser_DIR=S:\b\8\cmake\modules ^
  -G Ninja ^
  -S S:\swift-driver

ninja -C S:\b\9
```

### SwiftPM

```cmd
cmake -B S:\b\10 ^
  -D BUILD_SHARED_LIBS=YES ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D CMAKE_C_COMPILER=S:/b/1/bin/clang-cl.exe ^
  -D CMAKE_INSTALL_PREFIX=C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr ^
  -D CMAKE_MT=mt ^
  -D CMAKE_Swift_COMPILER=S:/b/1/bin/swiftc.exe ^
  -D dispatch_DIR=S:\b\2\cmake\modules ^
  -D Foundation_DIR=S:\b\3\cmake\modules ^
  -D TSC_DIR=S:\b\5\cmake\modules ^
  -D LLBuild_DIR=S:\b\6\cmake\modules ^
  -D Yams_DIR=S:\b\7\cmake\modules ^
  -D ArgumentParser_DIR=S:\b\8\cmake\modules ^
  -D SwiftDriver_DIR=S:\b\9\cmake\modules ^
  -G Ninja ^
  -S S:\swiftpm

ninja -C S:\b\10
```

Indicate to SwiftPM where to find `PackageDescription` before installation:

```cmd
set SWIFTPM_PD_LIBS=S:\b\10\pm
```

## Gather the toolchain and SDK

If you want a toolchain for real-world testing or distribution, you can use CMake to perform the install step.

### Swift compiler and standard library

```cmd
cmake --build S:\b\1 --target install
```

For testing, add the target to path:

```cmd
path S:\b\toolchain\usr\bin:%PATH%
```

## Swift Windows SDK (with core libraries)

```cmd
cmake --build S:\b\2 --target install
cmake --build S:\b\3 --target install
cmake --build S:\b\4 --target install
```

For testing, set `SDKROOT`:

```cmd
set SDKROOT=S:\b\sdk
```
