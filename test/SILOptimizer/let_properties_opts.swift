// RUN: %target-swift-frontend %s -O -emit-sil | FileCheck -check-prefix=CHECK-WMO %s
// RUN: %target-swift-frontend -primary-file %s -O -emit-sil | FileCheck %s

// Test propagation of non-static let properties with compile-time constant values.

// TODO: Once this optimization can remove the propagated private/internal let propeties or
// mark them as ones without a storage, new tests should be added here to check for this
// functionality.

// FIXME: This test is written in Swift instead of SIL, because there are some problems
// with SIL deserialization (rdar://22636911)

// Check that initializers do not contain a code to initialize private or
// internal (if used with WMO) properties, because their values are propagated into
// their uses and they cannot be accessed from other modules. Therefore the
// initialization code could be removed.
// Specifically, the initialization code for Prop1, Prop2 and Prop3 can be removed.

// CHECK-WMO-LABEL: sil @_TFC19let_properties_opts3Fooc{{.*}} : $@convention(method) (Int32, @owned Foo) -> @owned Foo
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop0
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK-WMO: return

// CHECK-WMO-LABEL: sil @_TFC19let_properties_opts3Fooc{{.*}} : $@convention(method) (Int64, @owned Foo) -> @owned Foo 
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop0
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK-WMO: return

// Check that initializers do not contain a code to initialize private properties, 
// because their values are propagated into their uses and they cannot be accessed
// from other modules. Therefore the initialization code could be removed.
// Specifically, the initialization code for Prop2 can be removed.

// CHECK-LABEL: sil @_TFC19let_properties_opts3Fooc{{.*}} : $@convention(method) (Int32, @owned Foo) -> @owned Foo
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK: return

// CHECK-LABEL: sil @_TFC19let_properties_opts3Fooc{{.*}} : $@convention(method) (Int64, @owned Foo) -> @owned Foo
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK: return

public class Foo {
  public let Prop0: Int32 = 1
  let Prop1: Int32 = 1 + 4/2 + 8
  private let Prop2: Int32 = 3*7
  internal let Prop3: Int32  = 4*8
  public init(i:Int32) {}  
  public init(i:Int64) {}
}

public class Foo1 {
  let Prop1: Int32
  private let Prop2: Int32 = 3*7
  internal let Prop3: Int32 = 4*8
  public init(i:Int32) {
    Prop1  = 11
  }

  public init(i:Int64) {
    Prop1  = 1111
  }
}

public struct Boo {
  public let Prop0: Int32 = 1
  let Prop1: Int32 = 1 + 4/2 + 8  
  private let Prop2: Int32 = 3*7
  internal let Prop3: Int32 = 4*8
  public init(i:Int32) {}
  public init(i:Int64) {}
}

// Check that Foo1.Prop1 is not constant-folded, because its value is unkown, since it is initialized differently
// by Foo1 initializers.

// CHECK-LABEL: sil @_TF19let_properties_opts13testClassLet1FCS_4Foo1Vs5Int32 : $@convention(thin) (@owned Foo1) -> Int32
// bb0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop1 
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop2
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop3
// CHECK: return
public func testClassLet1(f: Foo1) -> Int32 {
  return f.Prop1 + f.Prop2 + f.Prop3
}

// Check that Foo1.Prop1 is not constant-folded, because its value is unkown, since it is initialized differently
// by Foo1 initializers.

// CHECK-LABEL: sil @_TF19let_properties_opts13testClassLet1FRCS_4Foo1Vs5Int32 : $@convention(thin) (@inout Foo1) -> Int32 
// bb0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop1 
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop2
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop3
// CHECK: return
public func testClassLet1(inout f: Foo1) -> Int32 {
  return f.Prop1 + f.Prop2 + f.Prop3
}

// Check that return expressions in all subsequent functions can be constant folded, because the values of let properties
// are known to be constants of simple types.

// CHECK: sil @_TF19let_properties_opts12testClassLetFCS_3FooVs5Int32 : $@convention(thin) (@owned Foo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 75
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: strong_release
// CHECK-NEXT: return
public func testClassLet(f: Foo) -> Int32 {
  return f.Prop1 + f.Prop1 + f.Prop2 + f.Prop3
}

// CHECK-LABEL: sil @_TF19let_properties_opts12testClassLetFRCS_3FooVs5Int32 : $@convention(thin) (@inout Foo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 75
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testClassLet(inout f: Foo) -> Int32 {
  return f.Prop1 + f.Prop1 + f.Prop2 + f.Prop3
}

// CHECK-LABEL: sil @_TF19let_properties_opts18testClassPublicLetFCS_3FooVs5Int32 : $@convention(thin) (@owned Foo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 1
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: strong_release
// CHECK-NEXT: return
public func testClassPublicLet(f: Foo) -> Int32 {
  return f.Prop0
}

// CHECK-LABEL: sil @_TF19let_properties_opts13testStructLetFVS_3BooVs5Int32 : $@convention(thin) (Boo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 75
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testStructLet(b: Boo) -> Int32 {
  return b.Prop1 + b.Prop1 + b.Prop2 + b.Prop3
}

// CHECK-LABEL: sil @_TF19let_properties_opts13testStructLetFRVS_3BooVs5Int32 : $@convention(thin) (@inout Boo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 75
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testStructLet(inout b: Boo) -> Int32 {
  return b.Prop1 + b.Prop1 + b.Prop2 + b.Prop3
}

// CHECK-LABEL: sil @_TF19let_properties_opts19testStructPublicLetFVS_3BooVs5Int32 : $@convention(thin) (Boo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 1
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testStructPublicLet(b: Boo) -> Int32 {
  return b.Prop0
}
