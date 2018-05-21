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

## `clang-cl`
- Windows doesn't currently have a build script. You'll need to run commands
  manually to build Swift on Windows.
- Windows support for Swift is a work in progress and may not work on your
  system, but it has been tested.
- Using the latest Visual Studio version is recommended (tested with Visual
  Studio 2017 - Version 15.5.5). Swift may fail to build with older C++ 
  compilers.

### 1. Install dependencies
1. Latest version (2.7.12 tested) of [Python
   2](https://www.python.org/downloads/)
1. Latest version (3.7.0-rc3 tested) of [CMake](https://cmake.org/download/)
1. Latest version (1.7.1 tested) of
   [Ninja](https://github.com/ninja-build/ninja/releases/latest)
1. Latest version (2015 Update 3 tested) of [Visual
   Studio](https://www.visualstudio.com/downloads/)
- Make sure to include "Programming Languages|Visual C++" and "Windows and Web
  Development|Universal Windows App Development|Windows SDK" in your
  installation.
- Make sure to add Python, CMake and Ninja to your `Path` environment variable

### 2. Clone the repositories
1. Create a folder to contain all the Swift repositories
1. Clone `apple/swift-cmark` into a folder named `cmark`
1. Clone `apple/swift-clang` into a folder named `clang`
1. Clone `apple/swift-llvm` into a folder named `llvm`
1. Clone `apple/swift` into a folder named `swift`
- Currently, other repositories in the Swift project have not been tested and
  may not be supported.

### 3. Build ICU
1. Download and extract the [ICU source
code](http://site.icu-project.org/download) to a folder named `icu` in the same
directory as the other Swift project repositories.
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

### 5. Build CMark
- This must be done from within a developer command prompt. CMark is a fairly
  small project and should only take a few minutes to build.
```cmd
mkdir "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/cmark-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/cmark-windows-amd64"
cmake -G "Ninja" "%swift_source_dir%/cmark"
popd
cmake --build "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/cmark-windows-amd64/"
```

### 6. Build LLVM/Clang/Compiler-RT
- This must be done from within a developer command prompt. LLVM and Clang are
  large projects, so building might take a few hours. Make sure that the build
  type (e.g. Debug, Release, RelWithDebInfoAssert) for LLVM/Clang matches the
  build type for Swift.
- Optionally, you can omit building compiler-rt by removing all lines referring
  to `compiler-rt` below, which should give faster build times.
```cmd
mklink /J "%swift_source_dir%/llvm/tools/clang" "%swift_source_dir%/clang"
mklink /J "%swift_source_dir%/llvm/tools/compiler-rt" "%swift_source_dir%/compiler-rt"
mkdir "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"
cmake -G "Ninja"^
 -DLLVM_ENABLE_ASSERTIONS=TRUE^
 -DCMAKE_BUILD_TYPE=RelWithDebInfo^
 -DLLVM_TOOL_SWIFT_BUILD=NO^
 -DLLVM_INCLUDE_DOCS=TRUE^
 -DLLVM_TOOL_COMPILER_RT_BUILD=TRUE^
 -DLLVM_BUILD_EXTERNAL_COMPILER_RT=TRUE^
 -DLLVM_LIT_ARGS=-sv^
 -DLLVM_TARGETS_TO_BUILD=X86^
 "%swift_source_dir%/llvm"
popd
cmake --build "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"
```
- Store the LLVM `bin` directory in an environment variable so it can be used
  to build Swift. Assuming you followed the instructions exactly, the path
  below is correct, but it may be different based on your build variant and
  platform, so double check.
```cmd
set llvm_bin_dir="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64/bin"
```

### 7. Build Swift
- This must be done from within a developer command prompt and could take hours 
  depending on your system.
- You may need to adjust the `SWIFT_WINDOWS_LIB_DIRECTORY` parameter depending on
  your target platform or Windows SDK version.
```cmd
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
cmake -G "Ninja" "%swift_source_dir%/swift"^
 -DCMAKE_BUILD_TYPE=Debug^
 -DSWIFT_PATH_TO_CMARK_SOURCE="%swift_source_dir%/cmark"^
 -DSWIFT_PATH_TO_CMARK_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/cmark-windows-amd64"^
 -DSWIFT_CMARK_LIBRARY_DIR="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/cmark-windows-amd64/src"^
 -DSWIFT_PATH_TO_LLVM_SOURCE="%swift_source_dir%/llvm"^
 -DSWIFT_PATH_TO_LLVM_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"^
 -DSWIFT_PATH_TO_CLANG_SOURCE="%swift_source_dir%/llvm/tools/clang"^
 -DSWIFT_PATH_TO_CLANG_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"^
 -DICU_UC_INCLUDE_DIR="%swift_source_dir%/icu/include"^
 -DICU_UC_LIBRARY_DIRS="%swift_source_dir%/icu/lib64"^
 -DICU_I18N_INCLUDE_DIR="%swift_source_dir%/icu/include"^
 -DICU_I18N_LIBRARY_DIRS="%swift_source_dir%/icu/lib64"^
 -DICU_UC_LIB_NAME="icuuc"^
 -DICU_I18N_LIB_NAME="icuin"^
 -DSWIFT_INCLUDE_DOCS=FALSE^
 -DSWIFT_INCLUDE_TESTS=FALSE^
 -DCMAKE_C_COMPILER="%llvm_bin_dir%/clang-cl.exe"^
 -DCMAKE_CXX_COMPILER="%llvm_bin_dir%/clang-cl.exe"^
 -DCMAKE_C_FLAGS="-fms-compatibility-version=19.00 /Z7"^
 -DCMAKE_CXX_FLAGS="-fms-compatibility-version=19.00 -Z7" ^
 -DSWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER=FALSE
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
cmake -G "Visual Studio 15" "%swift_source_dir%/swift"^
 -DCMAKE_GENERATOR_PLATFORM="x64"^
 ...
```

### 8. Build lldb
- This must be done from within a developer command prompt and could take hours
  depending on your system.
```cmd
mkdir "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/lldb-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/lldb-windows-amd64"
cmake -G "Ninja" "%swift_source_dir%/lldb"^
  -DCMAKE_BUILD_TYPE=RelWithDebInfo^
  -DLLDB_PATH_TO_CMARK_SOURCE="%swift_source_dir%/cmark"^
  -DLLDB_PATH_TO_CMARK_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/cmark-windows-amd64"^
  -DLLDB_PATH_TO_LLVM_SOURCE="%swift_source_dir%/llvm"^
  -DLLDB_PATH_TO_LLVM_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"^
  -DLLDB_PATH_TO_CLANG_SOURCE="%swift_source_dir%/clang"^
  -DLLDB_PATH_TO_CLANG_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"^
  -DLLVM_ENABLE_ASSERTIONS=YES
popd
cmake --build "%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/lldb-windows-amd64"
```

## MSVC

Follow instructions 1-6 for `clang-cl`, but run the following instead to build Swift

```cmd
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
cmake -G "Ninja" "%swift_source_dir%/swift"^
 -DCMAKE_BUILD_TYPE=Debug^
 -DSWIFT_PATH_TO_CMARK_SOURCE="%swift_source_dir%/cmark"^
 -DSWIFT_PATH_TO_CMARK_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/cmark-windows-amd64"^
 -DSWIFT_CMARK_LIBRARY_DIR="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/cmark-windows-amd64/src"^
 -DSWIFT_PATH_TO_LLVM_SOURCE="%swift_source_dir%/llvm"^
 -DSWIFT_PATH_TO_LLVM_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"^
 -DSWIFT_PATH_TO_CLANG_SOURCE="%swift_source_dir%/llvm/tools/clang"^
 -DSWIFT_PATH_TO_CLANG_BUILD="%swift_source_dir%/build/Ninja-RelWithDebInfoAssert/llvm-windows-amd64"^
 -DICU_UC_INCLUDE_DIRS="%swift_source_dir%/icu/include"^
 -DICU_UC_LIBRARY_DIRS="%swift_source_dir%/icu/lib64"^
 -DICU_I18N_INCLUDE_DIRS="%swift_source_dir%/icu/include"^
 -DICU_I18N_LIBRARY_DIRS="%swift_source_dir%/icu/lib64"^
 -DICU_UC_LIB_NAME="icuuc"^
 -DICU_I18N_LIB_NAME="icuin"^
 -DSWIFT_INCLUDE_DOCS=FALSE^
 -DSWIFT_INCLUDE_TESTS=FALSE^
 -DSWIFT_BUILD_DYNAMIC_SDK_OVERLAY=FALSE^
 -DSWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER=FALSE
popd
cmake --build "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64"
```
