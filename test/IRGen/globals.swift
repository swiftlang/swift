// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: target triple = "x86_64-apple-darwin10"

import swift

var g0 : int = 1
