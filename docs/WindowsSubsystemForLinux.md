# Swift on the Windows Subsystem for Linux (WSL)

- Note that all compiled Swift binaries are only executable within Bash on
  Windows and are Ubuntu, not Windows, executables (WSL can natively run Ubuntu
  executables).
- Make sure to run all commands from `bash`, or the project won't compile.

###  1. Install WSL
Install and run the latest version of [Bash on Ubuntu on
Windows](https://docs.microsoft.com/en-us/windows/wsl/about) installed on
your PC.
```
bash
```

### 2. Install dependencies
Install the developer dependencies needed to compile the Swift project. These
are identical to the Ubuntu dependencies, with the addition of `make`.
```bash
sudo apt-get install git make cmake ninja-build clang python uuid-dev libicu-dev icu-devtools libbsd-dev libedit-dev libxml2-dev libsqlite3-dev swig libpython-dev libncurses5-dev pkg-config libblocksruntime-dev libcurl4-openssl-dev
```

### 3. Upgrade `clang`
Install a version of `clang` with C++ 14 support; the default version of `clang`
on WSL results in linker errors during compilation.
```bash
sudo apt-get install clang-3.6
sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-3.6 100
sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.6 100
```

### 4. Upgrade CMake
Install the latest version of CMake; Swift uses new CMake features such as
`IN_LIST` and won't build without these features.
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
