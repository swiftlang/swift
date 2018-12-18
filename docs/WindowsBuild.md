# Building Swift on Windows

This document describes how to build Swift for Windows natively. See [the
Windows doc](./Windows.md) for more about what is possible with Swift on
Windows.

There are two supported ways to build Swift on Windows, they are

1. Using [`clang-cl`](https://clang.llvm.org/docs/UsersManual.html#clang-cl)
1. Using the Microsoft Visual C++ compiler (MSVC)

`clang-cl` is recommended over MSVC for building Swift on Windows.

Although it is possible to build the compiler and the standard library with
MSVC and to use those built products to compile a Swift program, it won't be
possible to run the binary without separately obtaining the Swift runtime. On
the other hand, `clang-cl` is able to build the runtime, which makes it
possible to build and run all the components required for Swift natively on
Windows.

clang should be 7.0 or newer. Visual Studio 2017 is needed in all cases as
it provides some of the needed headers and libraries.

## `clang-cl`

### 1. Install dependencies
1. Latest version of [Visual Studio](https://www.visualstudio.com/downloads/)
- Make sure to include "Programming Languages|Visual C++" and "Windows and Web
  Development|Universal Windows App Development|Windows SDK" in your
  installation.

### 2. Clone the repositories
1. Create a folder to contain all the Swift repositories
1. Clone `apple/swift-cmark` into a folder named `cmark`
1. Clone `apple/swift-clang` into a folder named `clang`
1. Clone `apple/swift-llvm` into a folder named `llvm`
1. Clone `apple/swift-compiler-rt` into a folder named `compiler-rt`
1. Clone `apple/swift` into a folder named `swift`
1. Clone `apple/swift-corelibs-libdispatch` into a folder named `swift-corelibs-libdispatch`
- Currently, other repositories in the Swift project have not been tested and
  may not be supported.

### 3. Acquire ICU
1. Download ICU from [ICU Project](http://site.icu-project.org) for Windows x64 and extract the binaries.
1. Add the `bin64` folder to your `Path` environment variable.

### 4. Get ready
- From within a **developer** command prompt (not PowerShell nor cmd, but [the
  Visual Studio Developer Command
  Prompt](https://msdn.microsoft.com/en-us/library/f35ctcxw.aspx)), execute the
  following command if you have an x64 PC.
```cmd
VsDevCmd -arch=amd64
```
If instead you're compiling for a 32-bit Windows target, adapt the `arch`
argument to `x86` and run
```cmd
VsDevCmd -arch=x86
```

- We will use the assumption that the sources are on the `S` drive.  Replace it with the path to the checkout.  Make sure to use forward slashes (`/`) instead of backslashes (`\`) as the path separators. `clang` breaks with backslashed paths.

- Decide whether you want to build a release or debug version of Swift on Windows and 
  replace the `CMAKE_BUILD_TYPE` parameter in the build steps below with the correct value 
  (`Debug`, `RelWithDebInfoAssert` or `Release`) to avoid conflicts between the debug and 
  non-debug version of the MSCRT library.

- Set up the `ucrt`, `visualc`, and `WinSDK` modules by copying  `ucrt.modulemap` located at
  `swift/stdlib/public/Platform/ucrt.modulemap` into
  `${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt` as `module.modulemap`, copying `visualc.modulemap` located at `swift/stdlib/public/Platform/visualc.modulemap` into `${VCToolsInstallDir}/include` as `module.modulemap`, and copying `winsdk.modulemap` located at `swift/stdlib/public/Platform/winsdk.modulemap` into `${UniversalCRTSdkDir}/Include/${UCRTVersion}/um` and setup the `visualc.apinotes` located at `swift/stdlib/public/Platform/visualc.apinotes` into `${VCToolsInstallDir}/include` as `visualc.apinotes`

```cmd
cd %UniversalCRTSdkDir%\Include\%UCRTVersion%\ucrt
mklink module.modulemap S:\swift\stdlib\public\Platform\ucrt.modulemap
cd %VCToolsInstallDir%\include
mklink module.modulemap S:\swift\stdlib\public\Platform\visualc.modulemap
mklink visualc.apinotes S:\swift\stdlib\public\Platform\visualc.apinotes
cd %UniversalCRTSdkDir\Include\%UCRTVersion%\um
mklink module.modulemap S:\swift\stdlib\public\Platform\winsdk.modulemap
```

### 5. Build CMark
- This must be done from within a developer command prompt. CMark is a fairly
  small project and should only take a few minutes to build.
```cmd
mkdir S:/build/Ninja-DebugAssert/cmark-windows-amd64"
pushd S:/build/Ninja-DebugAssert/cmark-windows-amd64" "S:/%swift_source_dir%/cmark"
popd
cmake --build "S:/build/Ninja-DebugAssert/cmark-windows-amd64/"
```

### 6. Build LLVM/Clang
- This must be done from within a developer command prompt. LLVM and Clang are
  large projects, so building might take a few hours. Make sure that the build
  type (e.g. `Debug`, `Release`, `RelWithDebInfoAssert`) for LLVM/Clang matches the
  build type for Swift.
```cmd
mkdir "S:/build/Ninja-DebugAssert/llvm-windows-amd64"
pushd "S:/build/Ninja-DebugAssert/llvm-windows-amd64"
cmake -G "Ninja"^
 -DLLVM_ENABLE_ASSERTIONS=TRUE^
 -DCMAKE_BUILD_TYPE=Debug^
 -DLLVM_ENABLE_PROJECTS=clang^
 -DLLVM_TARGETS_TO_BUILD=X86^
 -DLLVM_DEFAULT_TARGET_TRIPLE=x86_64-unknown-windows-msvc^
 "S:/llvm"
popd
cmake --build "S:/build/Ninja-DebugAssert/llvm-windows-amd64"
```

### 7. Build Swift
- This must be done from within a developer command prompt and could take hours 
  depending on your system.
- You may need to adjust the `SWIFT_WINDOWS_LIB_DIRECTORY` parameter depending on
  your target platform or Windows SDK version.
```cmd
mkdir "S:/build/Ninja-DebugAssert/swift-windows-amd64"
pushd "S:/build/Ninja-DebugAssert/swift-windows-amd64"
cmake -G "Ninja"^
 -DCMAKE_BUILD_TYPE=Debug^
 -DCMAKE_C_COMPILER="S:/build/Ninja-DebugAssert/llvm-windows-amd64/clang-cl.exe"^
 -DCMAKE_CXX_COMPILER="S:/build/Ninja-DebugAssert/llvm-windows-amd64/clang-cl.exe"^
 -DSWIFT_PATH_TO_CMARK_SOURCE="S:/cmark"^
 -DCMAKE_CXX_FLAGS="-Wno-c++98-compat -Wno-c++98-compat-pedantic"^
 -DCMAKE_EXE_LINKER_FLAGS:STRING="/INCREMENTAL:NO"^
 -DCMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO"^
 -DSWIFT_INCLUDE_DOCS=NO^
 -DSWIFT_PATH_TO_LLVM_SOURCE="S:/llvm"^
 -DSWIFT_PATH_TO_CLANG_SOURCE="S:/clang"^
 -DSWIFT_PATH_TO_LIBDISPATCH_SOURCE="S:/swift-corelibs-libdispatch"^
 -DSWIFT_PATH_TO_LLVM_BUILD="S:/build/Ninja-DebugAssert/llvm-windows-amd64"^
 -DSWIFT_PATH_TO_CLANG_BUILD="S:/build/Ninja-DebugAssert/llvm-windows-amd64"^
 -DSWIFT_PATH_TO_CMARK_BUILD="S:/build/Ninja-DebugAssert/cmark-windows-amd64"^
 -DSWIFT_WINDOWS_x86_64_ICU_UC_INCLUDE="S:/icu/include"^
 -DSWIFT_WINDOWS_x86_64_ICU_UC="S:/icu/lib64/icuuc.lib"^
 -DSWIFT_WINDOWS_x86_64_ICU_I18N_INCLUDE="S:/icu/include"^
 -DSWIFT_WINDOWS_x86_64_ICU_I18N="S:/icu/lib64/icuin.lib"^
 -DCMAKE_INSTALL_PREFIX="C:/Program Files (x86)/Swift"^
 "S:/swift"
popd
cmake --build "S:/build/Ninja-DebugAssert/swift-windows-amd64"
```

- To create a Visual Studio project, you'll need to change the generator and,
  if you have a 64 bit processor, specify the generator platform. Note that you
  may get multiple build errors compiling the `swift` project due to an MSBuild
  limitation that file paths cannot exceed 260 characters. These can be
  ignored, as they occur after the build when writing the last build status to
  a file.

```cmd
cmake -G "Visual Studio 2017" "%swift_source_dir%/swift"^
 -DCMAKE_GENERATOR_PLATFORM="x64"^
 ...
```

### 8. Build lldb
- This must be done from within a developer command prompt and could take hours
  depending on your system.
```cmd
mkdir "S:/build/Ninja-DebugAssert/lldb-windows-amd64"
pushd "S:/build/Ninja-DebugAssert/lldb-windows-amd64"
cmake -G "Ninja" "S:/lldb"^
  -DCMAKE_BUILD_TYPE=Debug^
  -DLLDB_PATH_TO_CMARK_SOURCE="S:/cmark"^
  -DLLDB_PATH_TO_LLVM_SOURCE="S:/llvm"^
  -DLLDB_PATH_TO_CLANG_SOURCE="S:/clang"^
  -DLLDB_PATH_TO_SWIFT_SOURCE="S:/swift"^
  -DLLDB_PATH_TO_CMARK_BUILD="S:/build/Ninja-DebugAssert/cmark-windows-amd64"^
  -DLLDB_PATH_TO_CLANG_BUILD="S:/build/Ninja-DebugAssert/llvm-windows-amd64"^
  -DLLDB_PATH_TO_LLVM_BUILD="S:/build/Ninja-DebugAssert/llvm-windows-amd64"^
  -DLLDB_PATH_TO_SWIFT_BUILD="S:/build/Ninja-DebugAssert/swift-windows-amd64"^
  -DLLVM_ENABLE_ASSERTIONS=YES
popd
cmake --build "S:/build/Ninja-RelWithDebInfoAssert/lldb-windows-amd64"
```

### 9. Running tests on Windows

Running the testsuite on Windows has additional external dependencies.  You must have a subset of the GNUWin32 programs installed and available in your path.  The following packages are currently required:

  1. coreutils
  2. diffutils
  3. grep
  4. sed
  
```cmd
ninja -C "S:/build/Ninja-DebugAssert/swift-windows-amd64" check-swift
```

### 10. Build swift-corelibs-foundation

```cmd
mkdir "S:/build/Ninja-DebugAssert/swift-corelibs-foundation-windows-amd64"
pushd "S:/build/Ninja-DebugAssert/swift-corelibs-foundation-windows-amd64"
cmake -G "Ninja"^
  -DCMAKE_BUILD_TYPE=Debug^
  -DCMAKE_C_COMPILER="S:/build/Ninja-DebugAssert/llvm-windows-amd64/bin/clang-cl.exe"^
  -DCURL_LIBRARY="S:/curl/builds/libcurl-VS15-x64-release-static-ipv6-sspi-winssl/lib/libcurl_a.lib"^
  -DCURL_INCLUDE_DIR="S:/curl/builds/libcurl-VS15-x64-release-static-ipv6-sspi-winssl/include"^
  -DICU_ROOT="S:/thirdparty/icu4c-63_1-Win64-MSVC2017"^
  -DLIBXML2_LIBRARY="S:/libxml2/win32/bin.msvc/libxml2_a.lib"^
  -DLIBXML2_INCLUDE_DIR="S:/libxml2/include"^
  -DFOUNDATION_PATH_TO_LIBDISPATCH_SOURCE="S:/swift-corelibs-libdispatch"^
   "S:/swift-corelibs-foundation"
```

### 11. Install Swift on Windows

- Run ninja install:
```cmd 
ninja -C "S:/build/Ninja-DebugAssert/swift-windows-amd64" install
```
- Add the Swift on Windows binaries path (`C:\Program Files (x86)\Swift\bin`)  to the 
  `Path` environment variable.
- Add the Swift on Windows library path (`C:\Program Files (x86)\Swift\lib\swift\windows`) 
  to the `Path` environment variable.

## MSVC

To use `cl` instead, just remove the `-DCMAKE_C_COMPILER` and `-DCMAKE_CXX_COMPILER` parameters to the `cmake` invocations.
