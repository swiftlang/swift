// RUN: %target-swift-remoteast-test %s | FileCheck %s

// REQUIRES: PTRSIZE=64

@_silgen_name("printTypeMemberOffset")
func printTypeMemberOffset(_: Any.Type, _: StaticString)

@_silgen_name("printTypeMetadataMemberOffset")
func printTypeMetadataMemberOffset(_: Any.Type, _: StaticString)

printTypeMemberOffset((Int,Bool,Float,Bool,Int16).self, "0")
// CHECK: found offset: 0

printTypeMemberOffset((Int,Bool,Float,Bool,Int16).self, "1")
// CHECK: found offset: 8

printTypeMemberOffset((Int,Bool,Float,Bool,Int16).self, "2")
// CHECK: found offset: 12

printTypeMemberOffset((Int,Bool,Float,Bool,Int16).self, "3")
// CHECK: found offset: 16

printTypeMemberOffset((Int,Bool,Float,Bool,Int16).self, "4")
// CHECK: found offset: 18

printTypeMemberOffset((Int,Bool,Float,Bool,Int16).self, "5")
// CHECK: type has no member named '5'
