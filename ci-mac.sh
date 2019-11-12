#/bin/bash

brew install cmake ninja llvm
export current_sha=`git rev-parse HEAD`
./utils/update-checkout --clone --scheme wasm
git checkout $current_sha
export sourcedir=$PWD/..
cd $sourcedir
wget -O wasi-sdk.tar.gz https://github.com/swiftwasm/wasi-sdk/releases/download/20190421.6/wasi-sdk-3.19gefb17cb478f9.m-linux.tar.gz
tar xfz wasi-sdk.tar.gz
mv wasi-sdk-3.19gefb17cb478f9+m/opt/wasi-sdk ./wasi-sdk
# Link sysroot/usr/include to sysroot/include because Darwin sysroot doesn't 
# find header files in sysroot/include but sysroot/usr/include
mkdir wasi-sdk/share/sysroot/usr/
ln -s ../include wasi-sdk/share/sysroot/usr/include
wget -O icu.tar.xz "https://github.com/swiftwasm/icu4c-wasi/releases/download/20190421.3/icu4c-wasi.tar.xz"
tar xf icu.tar.xz

cd swift
./build-mac.sh
