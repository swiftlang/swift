// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sorted-sil -Xllvm -sil-full-demangle -emit-silgen %s | %FileCheck %s
// REQUIRES: EnableSILOpaqueValues
