// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -o %t %S/Inputs/has_generic_subscript.swift %S/Inputs/has_generic_subscript_proto.swift -module-name has_generic_subscript
// RUN: llvm-bcanalyzer %t/has_generic_subscript.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -I %t %s -o /dev/null

// CHECK-NOT: UnknownCode

import has_generic_subscript

var sillyDict = GenericSubscript()
_ = sillyDict as GenericSubscriptProto
var value: Int = sillyDict["beer"]
