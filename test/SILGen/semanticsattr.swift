// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s

// We currently we take the last @_semantics value.
//
// CHECK: [_semantics "123"] @func1
@_semantics("223") @_semantics("123")
@_silgen_name("func1") func func1() { }
