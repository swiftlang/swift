# Getting Started with Swift on Windows

## Windows Subsystem for Linux (WSL)
- Note that all compiled Swift binaries are only executable within Bash on Windows and are Ubunutu, not Windows, executables.
- Make sure to run all commands from Bash, or the project won't compile.

###  1. Install WSL
Install and run the latest version of [Bash on Ubuntu on Windows](https://msdn.microsoft.com/en-gb/commandline/wsl/about) installed on your PC.
```
bash
```

### 2. Install dependencies
Install the developer dependencies needed to compile the Swift project. These are identical to the Ubuntu dependencies, with the addition of `make`
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
```bash
wget http://www.cmake.org/files/v3.5/cmake-3.5.2.tar.gz
tar xf cmake-3.5.2.tar.gz
cd cmake-3.5.2
./configure
make
sudo make install
sudo update-alternatives --install /usr/bin/cmake cmake /usr/local/bin/cmake 1 --force
cmake --version # This should print 3.5.2
```

### 6. Clone and build the Swift project
```bash
mkdir swift-source
cd swift-source
git clone https://github.com/apple/swift.git
./swift/utils/update-checkout --clone
./swift/utils/build-script -r
```
6. Hello, Windows (Subsystem for Linux)
```bash
cd ./build/Ninja-RelWithDebInfoAssert/swift-linux-x86_64\bin # This path may depend on your build configuration
vim test.swift
print("Hello, Windows");
:wq
swiftc test.swift
./test # Hello, Windows
```
