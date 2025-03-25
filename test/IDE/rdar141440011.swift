public protocol MyProto {}
public struct MyStruct: MyProto {}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/swift_mod.swiftmodule %s -parse-as-library -experimental-skip-all-function-bodies -experimental-skip-non-exportable-decls -experimental-lazy-typecheck
// RUN: %target-swift-synthesize-interface -module-name swift_mod -I %t -o - | %FileCheck %s

// CHECK: public struct MyStruct : swift_mod.MyProto
