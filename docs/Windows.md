# Getting Started with Swift on Windows

## clang (cross-compiling)

### 1. Setup Visual Studio Environment Variables
Building for Windows requires that the Visual Studio environment variables are
setup similar to the values on Windows.  The following assumes that
`WINKIT_ROOT` points to the path where the Windows 10 SDK is available and that
`VC_ROOT` points to the path where the Visual Studio VC headers and libraries
are available.  Currently, the runtime has been tested to build against the
Windows 10 SDK at revision 10.10.586.

```
export WINKIT_ROOT=".../Windows Kits/10"
export VC_ROOT=".../Microsoft Visual Studio 14.0/VC"
export INCLUDE='${VC_ROOT}/include;${WINKIT_ROOT}/Include/10.0.10586.0/ucrt;${WINKIT_ROOT}/Include/10.0.10586.0/um;${WINKIT_ROOT}/Include/10.0.10586.0/shared'
export LIB='${VC_ROOT}/lib;${WINKIT_ROOT}/Lib/10.0.10586.0/ucrt/x86;${WINKIT_ROOT}/Lib/10.0.10586.0/um/x86'
```

### 2. Setup `visualc` and `ucrt` modules
The `visualc.modulemap` located at
`swift/stdlib/public/Platform/visualc.modulemap` needs to be copied into
`${VC_ROOT}/include`.  The `ucrt.modulemap` located at
`swift/stdlib/public/Platform/ucrt.modulemap` needs to be copied into
`${WINKIT_ROOT}/Include/10.0.10586.0/ucrt`.

### 3. Configure the runtime to be built with the just built clang
Ensure that we use the tools from the just built LLVM and clang tools to build
the Windows SDK.  You will need to pass a few extra options to cmake via the
`build-script` invocation to achieve this.  You will need to expand out the
path where llvm-ar and llvm-ranlib are built.  These are needed to correctly
build the static libraries.  Note that cross-compiling will require the use of
lld.  Ensure that lld-link.exe (lld-link) is available to clang via your path.
Additionally, the ICU headers and libraries need to be provided for the build.

```
--extra-cmake-options=-DSWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER=FALSE,-DCMAKE_AR=<path to llvm-ar>,-DCMAKE_RANLIB=<path to llvm-ranlib>,-DSWIFT_SDKS=WINDOWS,-DSWIFT_WINDOWS_ICU_I18N_INCLUDE=<path to ICU i18n includes>,-DSWIFT_WINDOWS_ICU_UC_INCLUDE=<path to ICU UC includes>,-DSWIFT_WINDOWS_ICU_I18N_LIB=<path to ICU i18n lib>,-DSWIFT_WINDOWS_ICU_UC_LIB=<path to ICU UC lib>
```

## MSVC
- Windows doesn't currently have a build script. You'll need to run commands manually to build Swift on Windows.
- Release/RelWithDebInfo modes have not been tested and may not be supported.
- Windows support for Swift is very much a WIP, and may not work on your system.
- Using the latest Visual Studio version is recommended. Swift may fail to build with older C++ compilers.

### 1. Install dependencies
- Make sure to add Python, CMake and Ninja to your `Path` environment variable
1. Latest version (2.7.12 tested) of [Python 2](https://www.python.org/downloads/)
2. Latest version (3.7.0-rc3 tested) of [CMake](https://cmake.org/download/)
3. Latest version (1.7.1 tested) of [Ninja](https://github.com/ninja-build/ninja/releases/latest)
4. Latest version (2015 Update 3 tested) of [Visual Studio](https://www.visualstudio.com/downloads/)
- Make sure to include `Programming Languages|Visual C++`, and `Windows and Web Development|Universal Windows App Development|Windows SDK` in your installation.
- Windows SDK 10.0.10240 was tested. Some later versions (e.g. 10.0.14393) are known not to work, as they are not supported by `compiler-rt`.

### 2. Clone the repositories
1. Create a folder to contain all the Swift repositories
2. `apple/swift-cmark` into a folder named `cmark`
3. `apple/swift-clang` into a folder named `clang`
5. `apple/swift-llvm` into a folder named `llvm`
5. `apple/swift` into a folder named `swift`
- Currently, other repositories in the Swift project have not been tested, and may not be supported.

### 3. Build ICU
1. Download and extract the [ICU source code](http://site.icu-project.org/download) to a folder named `icu` in the same directory as the other Swift project repositories.
2. Open `src/win32/allinone.sln` in Visual Studio.
3. Make sure to select the correct architecture from the drop-down in Visual Studio.
4. Right click on the solution in the Solution Explorer window and select `Build Solution`.
5. When this is done, add the `<icu-source>/bin` folder to your `Path` environment variable.

### 4. Get ready
- From within a **developer** command prompt, execute the following command if you have an x64 PC.
```
VsDevCmd -arch=amd64
```
- Then adapt the following command, and run it.
```
set swift_source_dir=path-to-directory-containing-all-cloned-repositories
```

### 5. Build CMark
- This must be done from within a developer command prompt, and could take up to 10 minutes.
```
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64"
cmake -G "Ninja" "%swift_source_dir%/cmark"
popd
cmake --build "%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64/"
```

### 6. Build LLVM/Clang/Compiler-RT
- This must be done from within a developer command prompt, and could take up to 5 hours.
```
mklink /J "%swift_source_dir%/llvm/tools/clang" "%swift_source_dir%my-swift/clang"
mklink /J "%swift_source_dir%/llvm/tools/compiler-rt" "%swift_source_dir%/compiler-rt"
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"
cmake -G "Ninja"^
 -DLLVM_ENABLE_ASSERTIONS=TRUE^
 -DCMAKE_BUILD_TYPE=Debug^
 -DLLVM_TOOL_SWIFT_BUILD=NO^
 -DLLVM_INCLUDE_DOCS=TRUE^
 -DLLVM_TOOL_COMPILER_RT_BUILD=TRUE^
 -DLLVM_BUILD_EXTERNAL_COMPILER_RT=TRUE^
 -DLLVM_LIT_ARGS=-sv^
 "%swift_source_dir%/llvm"
popd
cmake --build "%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"
```

### 7. Build Swift
- This must be done from within a developer command prompt, and could take up to 2 hours.
- You may need to adjust the SWIFT_WINDOWS_LIB_DIRECTORY parameter depending on your target platform or Windows SDK version.
```
mkdir "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64/ninja"
pushd "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64/ninja"
cmake -G "Ninja" "%swift_source_dir%/swift"^
 -DCMAKE_BUILD_TYPE=Debug^
 -DSWIFT_PATH_TO_CMARK_SOURCE="%swift_source_dir%/cmark"^
 -DSWIFT_PATH_TO_CMARK_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64"^
 -DSWIFT_CMARK_LIBRARY_DIR="%swift_source_dir%/build/Ninja-DebugAssert/cmark-windows-amd64/src"^
 -DSWIFT_PATH_TO_LLVM_SOURCE="%swift_source_dir%/llvm"^
 -DSWIFT_PATH_TO_LLVM_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"^
 -DSWIFT_PATH_TO_CLANG_SOURCE="%swift_source_dir%/llvm/tools/clang"^
 -DSWIFT_PATH_TO_CLANG_BUILD="%swift_source_dir%/build/Ninja-DebugAssert/llvm-windows-amd64"^
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
cmake --build "%swift_source_dir%/build/Ninja-DebugAssert/swift-windows-amd64/ninja"
```

- To create a VisualStudio project, you'll need to change the generator and, if you have a 64 bit processor, specify the generator platform. Note that you may get multiple build errors compiling the `swift` project due to an MSBuild limitation that file paths cannot exceed 260 characters. These can be ignored, as they occur after the build, writing the last build status to a file.
```
cmake -G "Visual Studio 15" "%swift_source_dir%/swift"^
 -DCMAKE_GENERATOR_PLATFORM="x64"^
 ...
```

## Clang-cl

Follow the instructions for MSVC, but add the following lines to each CMake configuration command. We need to use LLVM's `lld-link.exe` linker, as MSVC's `link.exe` crashes due to corrupt PDB files using `clang-cl`. `Clang-cl` 3.9.0 has been tested. You can remove the `SWIFT_BUILD_DYNAMIC_SDK_OVERLAY=FALSE` definition, as overlays are supported with `clang-cl`, as it supports modules. 

```
 -DCMAKE_C_COMPILER="<path-to-llvm-bin>/clang-cl.exe"^
 -DCMAKE_CXX_COMPILER="<path-to-llvm-bin>/bin/clang-cl.exe"^
 -DCMAKE_LINKER="<path-to-llvm-bin>/lld-link.exe"^
```

## Windows Subsystem for Linux (WSL)
- Note that all compiled Swift binaries are only executable within Bash on Windows and are Ubuntu, not Windows, executables.
- Make sure to run all commands from Bash, or the project won't compile.

###  1. Install WSL
Install and run the latest version of [Bash on Ubuntu on Windows](https://msdn.microsoft.com/en-gb/commandline/wsl/about) installed on your PC.
```
bash
```

### 2. Install dependencies
Install the developer dependencies needed to compile the Swift project. These are identical to the Ubuntu dependencies, with the addition of `make`.
```bash
sudo apt-get install git make cmake ninja-build clang python uuid-dev libicu-dev icu-devtools libbsd-dev libedit-dev libxml2-dev libsqlite3-dev swig libpython-dev libncurses5-dev pkg-config libblocksruntime-dev libcurl4-openssl-dev
```

### 3. Upgrade clang
Install a version of clang with C++ 14 support - the default version of clang on WSL results in linker errors during compilation.
```bash
sudo apt-get install clang-3.6
sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-3.6 100
sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.6 100
```

### 4. Upgrade CMake
Install the latest version of CMake - Swift uses new CMake features such as `IN_LIST` and won't build without these features.
```bash
wget http://www.cmake.org/files/v3.6/cmake-3.6.2.tar.gz
tar xf cmake-3.6.2.tar.gz
cd cmake-3.6.2
./configure
make
sudo make install
sudo update-alternatives --install /usr/bin/cmake cmake /usr/local/bin/cmake 1 --force
cmake --version # This should print 3.6.2
```

### 6. Clone and build the Swift project
```bash
mkdir swift-source
cd swift-source
git clone https://github.com/apple/swift.git
./swift/utils/update-checkout --clone
./swift/utils/build-script -r
```
### 7. Hello, Windows (Subsystem for Linux)
```bash
cd ./build/Ninja-RelWithDebInfoAssert/swift-linux-x86_64/bin # This path may depend on your build configuration
echo 'print("Hello, Windows")' >> test.swift
swiftc test.swift
./test # Hello, Windows
```
