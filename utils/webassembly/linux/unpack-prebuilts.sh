#!/bin/bash
set -e
rm -rf swiftwasm-sdk compiler
mkdir swiftwasm-sdk
ln -s swiftwasm-sdk compiler
cd compiler
untar="../../prebuilt/wasi-sdk-"*"-linux.tar.gz
../../prebuilt/swiftwasm-linux.tar.gz
../../prebuilt/icu4c-wasi.tar.xz"
for i in $untar
do
	echo $i
	tar xf $i
done
cd ..
mv "compiler/wasi-sdk-"* "compiler/wasi-sdk"
mv compiler/wasi-sdk/share/wasi-sysroot compiler/wasi-sdk/share/sysroot
../remove-swift-extra-files.sh || true
../remove-wasi-extra-files.sh || true
../copy-shared-files.sh || true
