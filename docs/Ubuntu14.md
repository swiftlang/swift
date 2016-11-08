# Getting Started with Swift on Ubuntu 14.04

The Swift project no longer officially supports Ubuntu 14.04 as a build platform.
It is highly recommended to upgrade to Ubuntu 16.04 or higher in order to have the best experience building and running the project.

If you cannot upgrade Ubuntu to a more up to date version, you should follow these steps before checking out and building the Swift source code. These are not guaranteed to work in the future, and may break or be unreliable.

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
