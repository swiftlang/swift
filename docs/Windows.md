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
`build-script` invocation to acheive this.  You will need to expand out the
path where llvm-ar and llvm-ranlib are built.  These are needed to correctly
build the static libraries.  Note that cross-compiling will require the use of
lld.  Ensure that lld-link.exe (lld-link) is available to clang via your path.
Additionally, the ICU headers and libraries need to be provided for the build.

```
--extra-cmake-options=-DSWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER=FALSE,-DCMAKE_AR=<path to llvm-ar>,-DCMAKE_RANLIB=<path to llvm-ranlib>,-DSWIFT_SDKS=WINDOWS,-DSWIFT_WINDOWS_ICU_I18N_INCLUDE=<path to ICU i18n includes>,-DSWIFT_WINDOWS_ICU_UC_INCLUDE=<path to ICU UC includes>,-DSWIFT_WINDOWS_ICU_I18N_LIB=<path to ICU i18n lib>,-DSWIFT_WINDOWS_ICU_UC_LIB=<path to ICU UC lib>
```

## MSVC
To be filled in.

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
wget http://www.cmake.org/files/v3.5/cmake-3.6.2.tar.gz
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
