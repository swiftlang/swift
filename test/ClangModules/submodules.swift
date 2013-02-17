// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s
// RUN: ls -lR %t/clang-module-cache | grep ctypes.pcm

import ctypes
import ctypes.bits

// From bits submodule
var x : DWORD = 123
var y : CInt = x
