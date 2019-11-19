#!/bin/bash
set -e
rm -rf swiftwasm-sdk compiler tmpdir
mkdir swiftwasm-sdk tmpdir
ln -s swiftwasm-sdk compiler
cd compiler
untar="../../prebuilt/wasi-sdk-"*"-linux.tar.gz
../../prebuilt/swiftwasm-macos.tar.gz
../../prebuilt/icu4c-wasi.tar.xz"
for i in $untar
do
	echo $i
	tar xf $i
done
# Mac: unpack the Linux one and copy stdlibs
cd ..
cd tmpdir
tar xf ../../prebuilt/clang+llvm-*-x86_64-darwin-apple.tar.xz
tar xf ../../prebuilt/swiftwasm-linux.tar.gz
cd ..
mv "compiler/wasi-sdk-"* "compiler/wasi-sdk"
mv compiler/wasi-sdk/share/wasi-sysroot compiler/wasi-sdk/share/sysroot
../remove-swift-extra-files.sh || true
rm -r compiler/wasi-sdk/bin
mkdir compiler/wasi-sdk/bin
cp tmpdir/clang+llvm-*-x86_64-darwin-apple/bin/wasm-ld compiler/wasi-sdk/bin
cp -a tmpdir/opt/swiftwasm-sdk/lib/swift/wasm compiler/opt/swiftwasm-sdk/lib/swift/wasm
cp -a tmpdir/opt/swiftwasm-sdk/lib/swift_static compiler/opt/swiftwasm-sdk/lib/swift_static
# ok, finally copy over the shared files
../copy-shared-files.sh || true
