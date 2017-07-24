// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen -sil-serialize-witness-tables %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// CHECK-LABEL: sil hidden @_T06shared0A9_argumentySih1x_SSh1ytF 
func shared_argument(x : __shared Int, y : __shared String) {}
