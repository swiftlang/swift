// RUN: %target-swift-emit-silgen -parse-stdlib -enable-sil-ownership %s | %FileCheck %s

// CHECK: [_semantics "123"] [_semantics "223"] @func1
@_semantics("223") @_semantics("123")
@_silgen_name("func1") func func1() { }
