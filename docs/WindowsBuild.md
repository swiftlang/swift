# Building Swift on Windows

Visual Studio 2017 or newer is needed to build Swift on Windows. The free Community edition is sufficient to build Swift.

The commands below (with the exception of installing Visual Studio) must be entered in the "**x64 Native** Tools Command Prompt for VS2017" (or VS2019, VS2019 Preview depending on the Visual Studio that you are using) in the Start Menu. This sets environment variables to select the correct target platform.

## Install dependencies

### Visual Studio

An easy way to get most of the tools to build Swift is using the [Visual Studio installer](https://www.visualstudio.com/downloads/). This command installs all needed Visual Studio components as well as Python and Git:

```
vs_community ^
  --add Component.CPython2.x86 ^
  --add Component.CPython3.x64 ^
  --add Microsoft.VisualStudio.Component.Git ^
  --add Microsoft.VisualStudio.Component.VC.ATL ^
  --add Microsoft.VisualStudio.Component.VC.CMake.Project ^
  --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 ^
  --add Microsoft.VisualStudio.Component.Windows10SDK ^
  --add Microsoft.VisualStudio.Component.Windows10SDK.17763
```

If you prefer you can install everything by hand, but make sure to include "Programming Languages|Visual C++" and "Windows and Web Development|Universal Windows App Development|Windows SDK" in your installation. The components listed above are required.

The following [link](https://docs.microsoft.com/visualstudio/install/workload-component-id-vs-build-tools?view=vs-2019) helps in finding the component name given its ID for Visual Studio 2019.

### Python

The command above already installs Python 2 and 3. Alternatively, in the Visual Studio installation program, under *Individual Components*

1. Install *Python 2*, either the 32-bit version (C:\Python27\\) or the 64-bit version (C:\Python27amd64\\)

   **Note:** If you install the 64-bit version only, you will need to adjust `PYTHON_EXECUTABLE` below to `C:\Python27amd64\python.exe`

2. Install *Python 3 64 bits (3.7.x)*

If you are building a debug version of Swift, you should also install the Python debug binaries.

1. In the Windows settings, go to *Add and Remove Programs*
2. Select the *Python 3.7.x (64-bit)* entry
3. Click *Modify*, then *Yes*, then *Modify* again and then *Next*
4. Select *Download debug binaries (requires VS 2015 or later)*
5. Click *Install*

## Enable Developer Mode

From the settings application, go to `Update & Security`.  In the `For developers` tab, select `Developer Mode` for `Use Developer Features`.  This is required to enable the creation of symbolic links.

## Clone the repositories

1. Clone `apple/llvm-project` into a directory for the toolchain
2. Clone `apple/swift-cmark`, `apple/swift`, `apple/swift-corelibs-libdispatch`, `apple/swift-corelibs-foundation`, `apple/swift-corelibs-xctest`, `apple/swift-llbuild`, `apple/swift-package-manager` into the toolchain directory

- Currently, other repositories in the Swift project have not been tested and may not be supported.

This guide assumes your sources live at the root of `S:`. If your sources live elsewhere, you can create a substitution for this:

```cmd
subst S: <path to sources>
```

```cmd
S:
git clone https://github.com/apple/llvm-project --branch swift/master llvm-project
git clone -c core.autocrlf=input -c core.symlinks=true https://github.com/apple/swift swift
git clone https://github.com/apple/swift-cmark cmark
git clone https://github.com/apple/swift-corelibs-libdispatch swift-corelibs-libdispatch
git clone https://github.com/apple/swift-corelibs-foundation swift-corelibs-foundation
git clone https://github.com/apple/swift-corelibs-xctest swift-corelibs-xctest
git clone https://github.com/apple/swift-llbuild llbuild
git clone https://github.com/apple/swift-tools-support-core swift-tools-support-core
git clone -c core.autocrlf=input https://github.com/apple/swift-package-manager swiftpm
```

## Dependencies (ICU, SQLite3, curl, libxml2 and zlib)

The instructions assume that the dependencies are in `S:/Library`. The directory
structure should resemble:

```
/Library
  ┝ icu-67
  │   ┕ usr/...
  ├ libcurl-development
  │   ┕ usr/...
  ├ libxml2-development
  │   ┕ usr/...
  ├ sqlite-3.28.0
  │   ┕ usr/...
  ┕ zlib-1.2.11
      ┕ usr/...
```

Note that only ICU is required for building the toolchain, and SQLite is only
needed for building llbuild and onwards.  The ICU project provides binaries,
alternatively, see the ICU project for details on building ICU from source.

## One-time Setup (re-run on Visual Studio upgrades)

Set up the `ucrt`, `visualc`, and `WinSDK` modules by:

- copying `ucrt.modulemap` located at `swift/stdlib/public/Platform/ucrt.modulemap` into
  `${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt` as `module.modulemap`
- copying `visualc.modulemap` located at `swift/stdlib/public/Platform/visualc.modulemap` into `${VCToolsInstallDir}/include` as `module.modulemap`
- copying `winsdk.modulemap` located at `swift/stdlib/public/Platform/winsdk.modulemap` into `${UniversalCRTSdkDir}/Include/${UCRTVersion}/um`
- and setup the `visualc.apinotes` located at `swift/stdlib/public/Platform/visualc.apinotes` into `${VCToolsInstallDir}/include` as `visualc.apinotes`

```cmd
mklink "%UniversalCRTSdkDir%\Include\%UCRTVersion%\ucrt\module.modulemap" S:\swift\stdlib\public\Platform\ucrt.modulemap
mklink "%UniversalCRTSdkDir%\Include\%UCRTVersion%\um\module.modulemap" S:\swift\stdlib\public\Platform\winsdk.modulemap
mklink "%VCToolsInstallDir%\include\module.modulemap" S:\swift\stdlib\public\Platform\visualc.modulemap
mklink "%VCToolsInstallDir%\include\visualc.apinotes" S:\swift\stdlib\public\Platform\visualc.apinotes
```

Warning: Creating the above links usually requires administrator privileges. The quick and easy way to do this is to open a second developer prompt by right clicking whatever shortcut you used to open the first one, choosing Run As Administrator, and pasting the above commands into the resulting window. You can then close the privileged prompt; this is the only step which requires elevation.

## Build the toolchain

```cmd
cmake -B "S:\b\toolchain" ^
  -C S:\swift\cmake\caches\Windows-x86_64.cmake ^
  -D CMAKE_BUILD_TYPE=Release ^
  -D SWIFT_PATH_TO_LIBDISPATCH_SOURCE=S:\swift-corelibs-libdispatch ^
  -D LLVM_ENABLE_PDB=YES ^
  -D LLVM_EXTERNAL_SWIFT_SOURCE_DIR=S:\swift ^
  -D LLVM_EXTERNAL_CMARK_SOURCE_DIR=S:\cmark ^
  -D SWIFT_WINDOWS_x86_64_ICU_UC_INCLUDE=S:\Library\icu-67\usr\include ^
  -D SWIFT_WINDOWS_x86_64_ICU_UC=S:\Library\icu-67\usr\lib\icuuc67.lib ^
  -D SWIFT_WINDOWS_x86_64_ICU_I18N_INCLUDE=S:\Library\icu-67\usr\include ^
  -D SWIFT_WINDOWS_x86_64_ICU_I18N=S:\Library\icu-67\usr\lib\icuin67.lib ^
  -D CMAKE_INSTALL_PREFIX=C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr ^
  -G Ninja ^
  -S S:\llvm-project\llvm

ninja -C S:\b\toolchain
```

**Note:** If you installed only the 64-bit version of Python, you will need to adjust `PYTHON_EXECUTABLE` argument to `C:\Python27amd64\python.exe`


## Running Swift tests on Windows

```cmd
path S:\Library\icu-67\usr\bin;S:\b\toolchain\bin;S:\b\toolchain\tools\swift\libdispatch-prefix\bin;%PATH%;%ProgramFiles%\Git\usr\bin
ninja -C S:\b\toolchain check-swift
```

## Build swift-corelibs-libdispatch

```cmd
cmake -B S:\b\libdispatch -D CMAKE_BUILD_TYPE=RelWithDebInfo -D CMAKE_C_COMPILER=S:/b/toolchain/bin/clang-cl.exe -D CMAKE_CXX_COMPILER=S:/b/toolchain/bin/clang-cl.exe -D CMAKE_Swift_COMPILER=S:/b/toolchain/bin/swiftc.exe -D ENABLE_SWIFT=YES -G Ninja -S S:\swift-corelibs-libdispatch
ninja -C S:\b\libdispatch
```

## Test swift-corelibs-libdispatch

```cmd
ninja -C S:\b\libdispatch check
```

## Build swift-corelibs-foundation

```cmd
cmake -B S:\b\foundation -D CMAKE_BUILD_TYPE=RelWithDebInfo -D CMAKE_C_COMPILER=S:/b/toolchain/bin/clang-cl.exe -D CMAKE_Swift_COMPILER=S:/b/toolchain/bin/swiftc.exe -D CURL_LIBRARY="S:/Library/libcurl-development/usr/lib/libcurl.lib" -D CURL_INCLUDE_DIR="S:/Library/libcurl-development/usr/include" -D ICU_ROOT="S:/Library/icu-67" -D ICU_INCLUDE_DIR=S:/Library/icu-67/usr/include -D LIBXML2_LIBRARY="S:/Library/libxml2-development/usr/lib/libxml2s.lib" -D LIBXML2_INCLUDE_DIR="S:/Library/libxml2-development/usr/include/libxml2" -D ENABLE_TESTING=NO -D dispatch_DIR=S:/b/libdispatch/cmake/modules -G Ninja -S S:\swift-corelibs-foundation
ninja -C S:\b\foundation
```

- Add Foundation to your path:

```cmd
path S:\b\foundation\Foundation;%PATH%
```

## Build swift-corelibs-xctest

```cmd
cmake -B S:\b\xctest -D CMAKE_BUILD_TYPE=RelWithDebInfo -D CMAKE_Swift_COMPILER=S:/b/toolchain/bin/swiftc.exe -D dispatch_DIR=S:\b\dispatch\cmake\modules -D Foundation_DIR=S:\b\foundation\cmake\modules -D LIT_COMMAND=S:\toolchain\llvm\utils\lit\lit.py -D PYTHON_EXECUTABLE=C:\Python27\python.exe -G Ninja -S S:\swift-corelibs-xctest
ninja -C S:\b\xctest
```

- Add XCTest to your path:

```cmd
path S:\b\xctest;%PATH%
```

## Test XCTest

```cmd
ninja -C S:\b\xctest check-xctest
```

## Rebuild Foundation

```cmd
cmake -B S:\b\foundation -D CMAKE_BUILD_TYPE=RelWithDebInfo -D CMAKE_C_COMPILER=S:/b/toolchain/bin/clang-cl.exe -D CMAKE_Swift_COMPILER=S:/b/toolchain/bin/swiftc.exe -D CURL_LIBRARY="S:/Library/libcurl-development/usr/lib/libcurl.lib" -D CURL_INCLUDE_DIR="S:/Library/libcurl-development/usr/include" -D ICU_ROOT="S:/Library/icu-67" -D LIBXML2_LIBRARY="S:/Library/libxml2-development/usr/lib/libxml2.lib" -D LIBXML2_INCLUDE_DIR="S:/Library/libxml2-development/usr/include" -D ENABLE_TESTING=YES -D dispatch_DIR=S:/b/libdispatch/cmake/modules -D XCTest_DIR=S:/b/xctest/cmake/modules -G Ninja -S S:\swift-corelibs-foundation
ninja -C S:\b\foundation
```

## Test Foundation

```cmd
cmake --build S:\b\foundation
ninja -C S:\b\foundation test
```

## Build llbuild

```cmd
set AR=llvm-ar
cmake -B S:\b\llbuild -D CMAKE_BUILD_TYPE=RelWithDebInfo -D CMAKE_CXX_COMPILER=cl -D CMAKE_Swift_COMPILER=S:/b/toolchain/bin/swiftc.exe -D Foundation_DIR=S:/b/foundation/cmake/modules -D dispatch_DIR=S:/b/libdispatch/cmake/modules -D SQLite3_INCLUDE_DIR=S:\Library\sqlite-3.28.0\usr\include -D SQLite3_LIBRARY=S:\Library\sqlite-3.28.0\usr\lib\sqlite3.lib -D LLBUILD_SUPPORT_BINDINGS=Swift -G Ninja -S S:\llbuild
ninja -C S:\b\llbuild
```

- Add llbuild to your path:

```cmd
path S:\b\llbuild\bin;%PATH%
```

## Build swift-tools-core-support

```cmd
cmake -B S:\b\tsc -D CMAKE_BUILD_TYPE=RelWithDebInfo -D CMAKE_C_COMPILER=cl -D CMAKE_Swift_COMPILER=S:/b/toolchain/bin/swiftc.exe -D Foundation_DIR=S:/b/foundation/cmake/modules -D dispatch_DIR=S:/b/libdispatch/cmake/modules -G Ninja -S S:\swift-tools-support-core
ninja -C S:\b\tsc
```

## Build swift-package-manager

```cmd
cmake -B S:\b\spm -D CMAKE_BUILD_TYPE=RelWithDebInfo -D CMAKE_C_COMPILER=S:/b/toolchain/bin/clang-cl.exe -D CMAKE_CXX_COMPILER=S:/b/toolchain/bin/clang-cl.exe -D CMAKE_Swift_COMPILER=S:/b/toolchain/bin/swiftc.exe -D USE_VENDORED_TSC=YES -D Foundation_DIR=S:/b/foundation/cmake/modules -D dispatch_DIR=S:/b/libdispatch/cmake/modules -D LLBuild_DIR=S:/b/llbuild/cmake/modules -G Ninja -S S:\swiftpm
ninja -C S:\b\spm
```

## Install the Swift toolchain on Windows

- Run ninja install:

```cmd
ninja -C S:\b\toolchain install
```

- Add the Swift on Windows binaries path (`C:\Library\Developer\Toolchains\unknown-Asserts-development.xctoolchain\usr\bin`) to the `PATH` environment variable.
