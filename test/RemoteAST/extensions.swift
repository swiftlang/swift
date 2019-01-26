// RUN: %target-swift-remoteast-test %s | %FileCheck %s

// REQUIRES: swift-remoteast-test

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

extension Int {
  struct Inner { }
}

// CHECK: Int.Inner
printType(Int.Inner.self)
