// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle -enable-sil-ownership %s | %FileCheck %s
// REQUIRES: EnableSILOpaqueValues
