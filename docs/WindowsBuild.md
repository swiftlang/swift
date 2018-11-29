# Building Swift on Windows

This document describes how to build Swift for Windows natively. See [the
Windows doc](./Windows.md) for more about what is possible with Swift on
Windows.

There are two supported ways to build Swift on Windows, they are

1. Using [`clang-cl`](https://clang.llvm.org/docs/UsersManual.html#clang-cl)
1. Using the Microsoft Visual C++ compiler (MSVC)

`clang-cl` is recommended over MSVC for building Swift on Windows.
Although it is possible to build the compiler and the standard library with
MSVC, and use those built products to compile a Swift program, it won't be
possible to run the binary without seperately obtaining the Swift runtime. On
the other hand, `clang-cl` is able to build the runtime, which makes it
possible to build and run all the components required for Swift natively on
Windows.

clang should be 7.0 or newer.  Visual Studio 2017 is needed in all cases as
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

### 3. Build ICU
1. Download and extract the [ICU source
code](http://site.icu-project.org/download) to a folder named `icu` in the same
directory as the other Swift project repositories (tested with ICU versions 55.1 and 59.1).
1. Open `src/win32/allinone.sln` in Visual Studio.
1. Make sure to select the correct architecture from the drop-down in Visual
Studio.
1. Right click on the solution in the Solution Explorer window and select
"Build Solution".
1. When this is done, add the `<icu-source>/bin` folder to your `Path`
environment variable.

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

- Then adapt the following command and run it. Make sure to use forward slashes 
  (`/`) instead of backslashes (`\`) as the path separators. `clang` breaks 
  with backslashed paths.
```cmd
set swift_source_dir=path-to-directory-containing-all-cloned-repositories
```

- Decide whether you want to build a release or debug version of Swift on Windows and 
  replace the `CMAKE_BUILD_TYPE` parameter in the build steps below with the correct value 
  (`Debug`, `RelWithDebInfoAssert` or `Release`) to avoid conflicts between the debug and 
  non-debug version of the MSCRT library.

- Set up the `ucrt`, `visualc`, and `WinSDK` modules by copying  `ucrt.modulemap` located at
  `swift/stdlib/public/Platform/ucrt.modulemap` into
  `${UniversalCRTSdkDir}/Include/${UCRTVersion}/ucrt` as `module.modulemap`, copying `visualc.modulemap` located at `swift/stdlib/public/Platform/visualc.modulemap` into `${VCToolsInstallDir}/include` as `module.modulemap`, and copying `winsdk.modulemap` located at `swift/stdlib/public/Platform/winsdk.modulemap` into `${UniversalCRTSdkDir}/Include/10.0.107663/um`

### 5. Build CMark
- This must be done from within a developer command prompt. CMark is a fairly
  small project and should only take a few minutes to build.
```cmd
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64"
cmake -G "Ninja" -DCMAKE_BUILD_TYPE=Debug "%swift_source_dir%/cmark"
popd
cmake --build "%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64/"
```

### 6. Build LLVM/Clang
- This must be done from within a developer command prompt. LLVM and Clang are
  large projects, so building might take a few hours. Make sure that the build
  type (e.g. `Debug`, `Release`, `RelWithDebInfoAssert`) for LLVM/Clang matches the
  build type for Swift.
```cmd
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"
cmake -G "Ninja"^
 -DLLVM_ENABLE_ASSERTIONS=TRUE^
 -DCMAKE_BUILD_TYPE=Debug^
 -DLLVM_TOOL_SWIFT_BUILD=NO^
 -DLLVM_INCLUDE_DOCS=TRUE^
 -DLLVM_ENABLE_PROJECTS=clang^
 -DLLVM_LIT_ARGS=-sv^
 -DLLVM_TARGETS_TO_BUILD=X86^
 -DLLVM_DEFAULT_TARGET_TRIPLE=x86_64-unknown-windows-msvc^
 "%swift_source_dir%/llvm"
popd
cmake --build "%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"
```
- Store the LLVM `bin` directory in an environment variable so it can be used
  to build Swift. Assuming you followed the instructions exactly, the path
  below is correct, but it may be different based on your build variant and
  platform, so double check.
```cmd
set llvm_bin_dir="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64/bin"
```

### 7. Build Swift
- This must be done from within a developer command prompt and could take hours 
  depending on your system.
- You may need to adjust the `SWIFT_WINDOWS_LIB_DIRECTORY` parameter depending on
  your target platform or Windows SDK version.
```cmd
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
cmake -G "Ninja"^
 -DCMAKE_BUILD_TYPE=Debug^
 -DSWIFT_PATH_TO_CMARK_SOURCE="%swift_source_dir%/cmark"^
 -DSWIFT_PATH_TO_CMARK_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64"^
 -DSWIFT_CMARK_LIBRARY_DIR="%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64/src"^
 -DSWIFT_PATH_TO_LLVM_SOURCE="%swift_source_dir%/llvm"^
 -DSWIFT_PATH_TO_LLVM_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"^
 -DSWIFT_PATH_TO_CLANG_SOURCE="%swift_source_dir%/llvm/tools/clang"^
 -DSWIFT_PATH_TO_CLANG_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"^
 -DICU_WINDOWS_x86_64_UC_INCLUDE="%swift_source_dir%/icu/include"^
 -DICU_WINDOWS_x86_64_UC_LIBRARY="%swift_source_dir%/icu/lib64/icuuc.lib"^
 -DICU_WINDOWS_x86_64_I18N_INCLUDE="%swift_source_dir%/icu/include"^
 -DICU_WINDOWS_x86_64_I18N_LIBRARY="%swift_source_dir%/icu/lib64/icuin.lib"^
 -DSWIFT_INCLUDE_DOCS=FALSE^
 -DSWIFT_INCLUDE_TESTS=FALSE^
 -DCMAKE_C_COMPILER="%llvm_bin_dir%/clang-cl.exe"^
 -DCMAKE_CXX_COMPILER="%llvm_bin_dir%/clang-cl.exe"^
 -DCMAKE_C_FLAGS="/Z7"^
 -DCMAKE_CXX_FLAGS="/Z7 -Wno-c++98-compat -Wno-c++98-compat-pedantic"^
 -DCMAKE_EXE_LINKER_FLAGS:STRING="/INCREMENTAL:NO"^
 -DCMAKE_MODULE_LINKER_FLAGS="/INCREMENTAL:NO"^
 -DCMAKE_SHARED_LINKER_FLAGS="/INCREMENTAL:NO"^
 -DCMAKE_STATIC_LINKER_FLAGS="/INCREMENTAL:NO"^
 -DCMAKE_INSTALL_PREFIX="C:/Program Files (x86)/Swift"^
  "%swift_source_dir%/swift"
popd
cmake --build "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
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
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/lldb-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/lldb-windows-amd64"
cmake -G "Ninja" "%swift_source_dir%/lldb"^
  -DCMAKE_BUILD_TYPE=Debug^
  -DLLDB_PATH_TO_CMARK_SOURCE="%swift_source_dir%/cmark"^
  -DLLDB_PATH_TO_CMARK_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64"^
  -DLLDB_PATH_TO_LLVM_SOURCE="%swift_source_dir%/llvm"^
  -DLLDB_PATH_TO_LLVM_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"^
  -DLLDB_PATH_TO_CLANG_SOURCE="%swift_source_dir%/clang"^
  -DLLDB_PATH_TO_CLANG_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"^
  -DLLDB_PATH_TO_SWIFT_SOURCE="%swift_source_dir%/swift"^
  -DLLDB_PATH_TO_SWIFT_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"^
  -DCMAKE_C_COMPILER="%llvm_bin_dir%/clang-cl.exe"^
  -DCMAKE_CXX_COMPILER="%llvm_bin_dir%/clang-cl.exe"^
  -DCMAKE_C_FLAGS="/Z7"^
  -DCMAKE_CXX_FLAGS="/Z7 -Wno-c++98-compat"^
  -DLLVM_ENABLE_ASSERTIONS=YES
popd
cmake --build "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/lldb-windows-amd64"
```

### 9. Install Swift on Windows

- Run ninja install:
```cmd 
pushd "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
ninja install
popd
```
- Add the Swift on Windows binaries path (`C:\Program Files (x86)\Swift\bin`)  to the 
  `Path` environment variable.
- Add the Swift on Windows library path (`C:\Program Files (x86)\Swift\lib\swift\windows`) 
  to the `Path` environment variable.

## MSVC

Follow instructions 1-6 for `clang-cl`, but run the following instead to build Swift

```cmd
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
cmake -G "Ninja"^
 -DCMAKE_BUILD_TYPE=Debug^
 -DSWIFT_PATH_TO_CMARK_SOURCE="%swift_source_dir%/cmark"^
 -DSWIFT_PATH_TO_CMARK_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64"^
 -DSWIFT_CMARK_LIBRARY_DIR="%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64/src"^
 -DSWIFT_PATH_TO_LLVM_SOURCE="%swift_source_dir%/llvm"^
 -DSWIFT_PATH_TO_LLVM_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"^
 -DSWIFT_PATH_TO_CLANG_SOURCE="%swift_source_dir%/llvm/tools/clang"^
 -DSWIFT_PATH_TO_CLANG_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"^
 -DICU_WINDOWS_x86_64_UC_INCLUDE="%swift_source_dir%/icu/include"^
 -DICU_WINDOWS_x86_64_UC_LIBRARY="%swift_source_dir%/icu/lib64/icuuc.lib"^
 -DICU_WINDOWS_x86_64_I18N_INCLUDE="%swift_source_dir%/icu/include"^
 -DICU_WINDOWS_x86_64_I18N_LIBRARY="%swift_source_dir%/icu/lib64/icuin.lib"^
 -DSWIFT_INCLUDE_DOCS=FALSE^
 -DSWIFT_INCLUDE_TESTS=FALSE^
 -DSWIFT_BUILD_DYNAMIC_SDK_OVERLAY=FALSE^
 -DSWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER=FALSE^
  "%swift_source_dir%/swift"
popd
cmake --build "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
```
