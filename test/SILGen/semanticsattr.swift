// RUN: %target-swift-emit-silgen -parse-stdlib %s | %FileCheck %s

// CHECK: [_semantics "123"] [_semantics "223"] [ossa] @func1
@_semantics("223") @_semantics("123")
@_silgen_name("func1") func func1() { }
