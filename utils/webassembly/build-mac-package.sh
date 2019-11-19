#!/bin/bash

echo "Unpacking macOS prebuilts"
mkdir -p output
cd macos
./unpack-prebuilts.sh
echo "Compressing macOS package"
tar cJf ../output/swiftwasm-sdk-macos.tar.xz swiftwasm-sdk
cd ..
