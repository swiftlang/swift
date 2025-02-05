public protocol MyProto {}
public struct MyStruct: MyProto {}

// RUN: %empty-directory(%t)
// RUN: %swift -emit-module -o %t/swift_mod.swiftmodule %s -parse-as-library -experimental-skip-all-function-bodies -experimental-skip-non-exportable-decls -experimental-lazy-typecheck -target %target-triple
// RUN: %target-swift-synthesize-interface -module-name swift_mod -I %t -o - -target %target-triple | %FileCheck %s

// CHECK: public struct MyStruct : swift_mod.MyProto
