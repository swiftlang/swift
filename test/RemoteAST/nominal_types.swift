// RUN: %target-swift-remoteast-test %s | FileCheck %s

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

printType(Int.self)
// CHECK: Int
