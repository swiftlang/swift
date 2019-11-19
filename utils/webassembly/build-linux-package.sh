#!/bin/bash

echo "Unpacking Linux prebuilts"
mkdir -p output
cd linux
./unpack-prebuilts.sh
echo "Compressing"
tar cJf ../output/swiftwasm-sdk-linux.tar.xz swiftwasm-sdk
cd ..
