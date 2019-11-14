#/bin/bash

sudo apt update
sudo apt install \
  git ninja-build clang python \
  uuid-dev libicu-dev icu-devtools libbsd-dev \
  libedit-dev libxml2-dev libsqlite3-dev swig \
  libpython-dev libncurses5-dev pkg-config \
  libblocksruntime-dev libcurl4-openssl-dev \
  systemtap-sdt-dev tzdata rsync wget

export current_sha=`git rev-parse HEAD`
./utils/update-checkout --clone --scheme wasm
git checkout $current_sha
export sourcedir=$PWD/..
cd $sourcedir

wget -O install_cmake.sh "https://github.com/Kitware/CMake/releases/download/v3.15.3/cmake-3.15.3-Linux-x86_64.sh"
chmod +x install_cmake.sh
sudo mkdir -p /opt/cmake
sudo ./install_cmake.sh --skip-license --prefix=/opt/cmake
sudo ln -sf /opt/cmake/bin/* /usr/local/bin
cmake --version

wget -O wasi-sdk.tar.gz https://github.com/swiftwasm/wasi-sdk/releases/download/20191022.1/wasi-sdk-4.39g3025a5f47c04-linux.tar.gz
tar xfz wasi-sdk.tar.gz
mv wasi-sdk-4.39g3025a5f47c04 ./wasi-sdk
mv wasi-sdk/share/wasi-sysroot wasi-sdk/share/sysroot

wget -O icu.tar.xz "https://github.com/swiftwasm/icu4c-wasi/releases/download/20190421.3/icu4c-wasi.tar.xz"
tar xf icu.tar.xz

cd swift
./build-linux.sh
