#!/bin/bash
set -e
mkdir -p prebuilt
cd prebuilt
wget -O swiftwasm-linux.tar.gz https://github.com/swiftwasm/swiftwasm-sdk/releases/download/20191112.1.linux/swiftwasm.tar.gz
# Mac specific
wget -O swiftwasm-macos.tar.gz https://github.com/swiftwasm/swiftwasm-sdk/releases/download/20191112.1.mac/swiftwasm-mac.tar.gz
