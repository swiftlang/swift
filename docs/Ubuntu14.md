# Getting Started with Swift on Ubuntu 14.04

## Upgrade Clang
You'll need to upgrade your clang compiler for C++14 support and create a symlink. The minimum required version of clang may change, and may not be available on Ubuntu 14.04 in the future.
```bash
sudo apt-get install clang-3.6
sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-3.6 100
sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.6 100
```

## Upgrade CMake
You'll need to upgrade your CMake toolchain to a supported version by building a local copy. The minimum required version of CMake may change, and may not be available on Ubuntu 14.04 in the future.
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
