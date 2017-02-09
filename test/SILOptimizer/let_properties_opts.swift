// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -O -emit-sil | %FileCheck -check-prefix=CHECK-WMO %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -primary-file %s -O -emit-sil | %FileCheck %s

// Test propagation of non-static let properties with compile-time constant values.

// TODO: Once this optimization can remove the propagated fileprivate/internal let properties or
// mark them as ones without a storage, new tests should be added here to check for this
// functionality.

// FIXME: This test is written in Swift instead of SIL, because there are some problems
// with SIL deserialization (rdar://22636911)

// Check that initializers do not contain a code to initialize fileprivate or
// internal (if used with WMO) properties, because their values are propagated into
// their uses and they cannot be accessed from other modules. Therefore the
// initialization code could be removed.
// Specifically, the initialization code for Prop1, Prop2 and Prop3 can be removed.

// CHECK-WMO-LABEL: sil @_T019let_properties_opts3FooC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (Int32, @owned Foo) -> @owned Foo
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop0
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK-WMO: return

// CHECK-WMO-LABEL: sil @_T019let_properties_opts3FooC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (Int64, @owned Foo) -> @owned Foo 
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK-WMO-NOT: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop0
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK-WMO: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK-WMO: return

// Check that initializers do not contain a code to initialize fileprivate properties, 
// because their values are propagated into their uses and they cannot be accessed
// from other modules. Therefore the initialization code could be removed.
// Specifically, the initialization code for Prop2 can be removed.

// CHECK-LABEL: sil @_T019let_properties_opts3FooC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (Int32, @owned Foo) -> @owned Foo
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK: return

// CHECK-LABEL: sil @_T019let_properties_opts3FooC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (Int64, @owned Foo) -> @owned Foo
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop1
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop2
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo, #Foo.Prop3
// CHECK: return

public class Foo {
  public let Prop0: Int32 = 1
  let Prop1: Int32 = 1 + 4/2 + 8
  fileprivate let Prop2: Int32 = 3*7
  internal let Prop3: Int32  = 4*8
  public init(i:Int32) {}  
  public init(i:Int64) {}
}

public class Foo1 {
  let Prop1: Int32
  fileprivate let Prop2: Int32 = 3*7
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
  fileprivate let Prop2: Int32 = 3*7
  internal let Prop3: Int32 = 4*8
  public init(i:Int32) {}
  public init(i:Int64) {}
}

public class Foo2 {
  internal let x: Int32
  @inline(never)
  init(count: Int32) {
    if count < 2 {
      x = 5
    } else {
      x = 10
    }
  }
}

public class C {}

struct Boo3 {
  //public 
  let Prop0: Int32
  let Prop1: Int32
  fileprivate let Prop2: Int32
  internal let Prop3: Int32

  @inline(__always)
  init(_ f1: C, _ f2: C) {
    self.Prop0 = 0
    self.Prop1 = 1
    self.Prop2 = 2
    self.Prop3 = 3
  }

  init(_ v: C) {
    self.Prop0 = 10
    self.Prop1 = 11
    self.Prop2 = 12
    self.Prop3 = 13
  }
}

// The initializer of this struct can be defined elsewhere,
// e.g. in an extension of this struct in a different module.
public struct StructWithOnlyPublicLetProperties {
  public let Prop0: Int32
  public let Prop1: Int32

  init(_ v: Int32, _ u: Int32) {
    Prop0 = 10
    Prop1 = 11
  }
}

// The initializer of this struct cannot be defined outside
// of the current module, because it contains an internal stored
// property, which is impossible to initialize outside of this module.
public struct StructWithPublicAndInternalLetProperties {
  public let Prop0: Int32
  internal let Prop1: Int32

  init(_ v: Int32, _ u: Int32) {
    Prop0 = 10
    Prop1 = 11
  }
}

// The initializer of this struct cannot be defined elsewhere,
// because it contains a fileprivate stored property, which is
// impossible to initialize outside of this file.
public struct StructWithPublicAndInternalAndPrivateLetProperties {
  public let Prop0: Int32
  internal let Prop1: Int32
  fileprivate let Prop2: Int32

  init(_ v: Int32, _ u: Int32) {
    Prop0 = 10
    Prop1 = 11
    Prop2 = 12
  }
}


// Check that Foo1.Prop1 is not constant-folded, because its value is unknown, since it is initialized differently
// by Foo1 initializers.

// CHECK-LABEL: sil [thunk] [always_inline] @_T019let_properties_opts13testClassLet1s5Int32VAA4Foo1CF : $@convention(thin) (@owned Foo1) -> Int32
// bb0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop1 
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop2
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop3
// CHECK: return
public func testClassLet1(_ f: Foo1) -> Int32 {
  return f.Prop1 + f.Prop2 + f.Prop3
}

// Check that Foo1.Prop1 is not constant-folded, because its value is unknown, since it is initialized differently
// by Foo1 initializers.

// CHECK-LABEL: sil @_T019let_properties_opts13testClassLet1s5Int32VAA4Foo1CzF : $@convention(thin) (@inout Foo1) -> Int32 
// bb0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop1 
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop2
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo1, #Foo1.Prop3
// CHECK: return
public func testClassLet1(_ f: inout Foo1) -> Int32 {
  return f.Prop1 + f.Prop2 + f.Prop3
}

// Check that return expressions in all subsequent functions can be constant folded, because the values of let properties
// are known to be constants of simple types.

// CHECK: sil [thunk] [always_inline] @_T019let_properties_opts12testClassLets5Int32VAA3FooCF : $@convention(thin) (@owned Foo) -> Int32
// CHECK: bb0
// CHECK-NEXT: strong_release
// CHECK: integer_literal $Builtin.Int32, 75
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testClassLet(_ f: Foo) -> Int32 {
  return f.Prop1 + f.Prop1 + f.Prop2 + f.Prop3
}

// CHECK-LABEL: sil @_T019let_properties_opts12testClassLets5Int32VAA3FooCzF : $@convention(thin) (@inout Foo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 75
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testClassLet(_ f: inout Foo) -> Int32 {
  return f.Prop1 + f.Prop1 + f.Prop2 + f.Prop3
}

// CHECK-LABEL: sil [thunk] [always_inline] @_T019let_properties_opts18testClassPublicLets5Int32VAA3FooCF : $@convention(thin) (@owned Foo) -> Int32
// CHECK: bb0
// CHECK-NEXT: strong_release
// CHECK: integer_literal $Builtin.Int32, 1
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testClassPublicLet(_ f: Foo) -> Int32 {
  return f.Prop0
}

// CHECK-LABEL: sil @_T019let_properties_opts13testStructLets5Int32VAA3BooVF : $@convention(thin) (Boo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 75
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testStructLet(_ b: Boo) -> Int32 {
  return b.Prop1 + b.Prop1 + b.Prop2 + b.Prop3
}

// CHECK-LABEL: sil @_T019let_properties_opts13testStructLets5Int32VAA3BooVzF : $@convention(thin) (@inout Boo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 75
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testStructLet(_ b: inout Boo) -> Int32 {
  return b.Prop1 + b.Prop1 + b.Prop2 + b.Prop3
}

// CHECK-LABEL: sil @_T019let_properties_opts19testStructPublicLets5Int32VAA3BooVF : $@convention(thin) (Boo) -> Int32
// CHECK: bb0
// CHECK: integer_literal $Builtin.Int32, 1
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return
public func testStructPublicLet(_ b: Boo) -> Int32 {
  return b.Prop0
}

// Check that f.x is not constant folded, because the initializer of Foo2 has multiple
// assignments to the property x with different values.
// CHECK-LABEL: sil [thunk] [always_inline] @_T019let_properties_opts13testClassLet2s5Int32VAA4Foo2CF : $@convention(thin) (@owned Foo2) -> Int32
// bb0
// CHECK: ref_element_addr %{{[0-9]+}} : $Foo2, #Foo2.x
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo2, #Foo2.x
// CHECK-NOT: ref_element_addr %{{[0-9]+}} : $Foo2, #Foo2.x
// CHECK: return
public func testClassLet2(_ f: Foo2) -> Int32 {
  return f.x + f.x
}

// Check that the sum of properties is not folded into a constant.
// CHECK-WMO-LABEL: sil hidden [noinline] @_T019let_properties_opts27testStructWithMultipleInitss5Int32VAA4Boo3V_AFtF : $@convention(thin) (Boo3, Boo3) -> Int32
// CHECK-WMO: bb0
// No constant folding should have been performed.
// CHECK-WMO-NOT: integer_literal $Builtin.Int32, 92
// CHECK-WMO: struct_extract
// CHECK-WMO: }
@inline(never)
func testStructWithMultipleInits( _ boos1: Boo3, _ boos2: Boo3) -> Int32 {
  let count1 =  boos1.Prop0 + boos1.Prop1 + boos1.Prop2 + boos1.Prop3
  let count2 =  boos2.Prop0 + boos2.Prop1 + boos2.Prop2 + boos2.Prop3
  return count1 + count2
}

public func testStructWithMultipleInitsAndInlinedInitializer() {
  let things = [C()]
  // This line results in inlining of the initializer Boo3(C, C) and later
  // removal of this initializer by the dead function elimination pass.
  // As a result, only one initializer, Boo3(C) is seen by the Let Properties Propagation
  // pass. This pass may think that there is only one initializer and take the
  // values of let properties assigned there as constants and try to propagate
  // those values into uses. But this is wrong! The pass should be clever enough
  // to detect all stores to the let properties, including those outside of
  // initializers, e.g. inside inlined initializers. And if it detects all such
  // stores it should understand that values of let properties in Boo3 are not
  // statically known constant initializers with the same value and thus
  // cannot be propagated.
  let boos1 = things.map { Boo3($0, C()) }
  let boos2 = things.map(Boo3.init)
  print(testStructWithMultipleInits(boos1[0], boos2[0]))
}

// Since all properties are public, they can be initialized in a
// different module.
// Their values are not known and cannot be propagated.

// CHECK-LABEL: sil @_T019let_properties_opts31testStructPropertyAccessibilitys5Int32VAA0E27WithOnlyPublicLetPropertiesVF 
// CHECK: struct_extract %0 : $StructWithOnlyPublicLetProperties, #StructWithOnlyPublicLetProperties.Prop0
// CHECK: return

// CHECK-WMO-LABEL: sil @_T019let_properties_opts31testStructPropertyAccessibilitys5Int32VAA0E27WithOnlyPublicLetPropertiesVF
// CHECK-WMO: struct_extract %0 : $StructWithOnlyPublicLetProperties, #StructWithOnlyPublicLetProperties.Prop0
// CHECK-WMO: return
public func testStructPropertyAccessibility(_ b: StructWithOnlyPublicLetProperties) -> Int32 {
  return b.Prop0 + b.Prop1
}

// Properties can be initialized in a different file in the same module.
// Their values are not known and cannot be propagated,
// unless it is a WMO compilation.

// CHECK-LABEL: sil @_T019let_properties_opts31testStructPropertyAccessibilitys5Int32VAA0E34WithPublicAndInternalLetPropertiesVF
// CHECK: struct_extract %0 : $StructWithPublicAndInternalLetProperties, #StructWithPublicAndInternalLetProperties.Prop0
// CHECK-NOT: integer_literal $Builtin.Int32, 21
// CHECK: return

// CHECK-WMO-LABEL: sil @_T019let_properties_opts31testStructPropertyAccessibilitys5Int32VAA0E34WithPublicAndInternalLetPropertiesVF
// CHECK-WMO: integer_literal $Builtin.Int32, 21
// CHECK-WMO-NEXT: struct $Int32
// CHECK-WMO-NEXT: return
public func testStructPropertyAccessibility(_ b: StructWithPublicAndInternalLetProperties) -> Int32 {
  return b.Prop0 + b.Prop1
}

// Properties can be initialized only in this file, because one of the
// properties is fileprivate.
// Therefore their values are known and can be propagated.

// CHECK: sil @_T019let_properties_opts31testStructPropertyAccessibilitys5Int32VAA0e21WithPublicAndInternalK20PrivateLetPropertiesVF
// CHECK: integer_literal $Builtin.Int32, 33
// CHECK-NEXT: struct $Int32
// CHECK-NEXT: return

// CHECK-WMO-LABEL: sil @_T019let_properties_opts31testStructPropertyAccessibilitys5Int32VAA0e21WithPublicAndInternalK20PrivateLetPropertiesVF
// CHECK-WMO: integer_literal $Builtin.Int32, 33
// CHECK-WMO-NEXT: struct $Int32
// CHECK-WMO-NEXT: return
public func testStructPropertyAccessibility(_ b: StructWithPublicAndInternalAndPrivateLetProperties) -> Int32 {
  return b.Prop0 + b.Prop1 + b.Prop2
}

// Force use of initializers, otherwise they got removed by the dead-function-elimination pass
// and the values of let properties cannot be determined.
public func useInitializers() -> StructWithOnlyPublicLetProperties {
  return StructWithOnlyPublicLetProperties(1, 1)
}

public func useInitializers() -> StructWithPublicAndInternalLetProperties {
  return StructWithPublicAndInternalLetProperties(1, 1)
}

public func useInitializers() -> StructWithPublicAndInternalAndPrivateLetProperties {
  return StructWithPublicAndInternalAndPrivateLetProperties(1, 1)
}
