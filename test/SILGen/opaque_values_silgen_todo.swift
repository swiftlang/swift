// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle %s | %FileCheck %s
// REQUIRES: EnableSILOpaqueValues
