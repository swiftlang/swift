// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %s -emit-silgen -emit-verbose-sil -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import  | FileCheck %s

import Foundation

class ClassWithOutlets {
  // CHECK: sil {{.*}}ClassWithOutlets{{.*}}String1{{.*}} : $@cc(objc_method) @thin (ClassWithOutlets) -> @autoreleased ImplicitlyUnwrappedOptional<NSString>

  // CHECK-LABEL: sil @_TFC15objc_properties16ClassWithOutletscfMS0_FT_S0_ : $@cc(method) @thin (@owned ClassWithOutlets) -> @owned ClassWithOutlets
  // CHECK: string_literal utf16 "Hello"
  // CHECK-NOT: return
  // CHECK: string_literal utf16 "World"
  // CHECK-NOT: return
  // CHECK: function_ref @_TFVSs4_Nil12__conversionfS_U__FT_GSQQ__
  // CHECK: ref_element_addr [[SELF:%[0-9]+]] : $ClassWithOutlets, #ClassWithOutlets.String3
  @IBOutlet var String1 = "Hello"
  @IBOutlet weak var String2: String! = "World"
  @IBOutlet var String3: String
}


class A {
  @objc var prop: Int
  @objc var computedProp: Int {
    get {
      return 5
    }
    set {}
  }

  // Regular methods go through the @objc property accessors.
  // CHECK-LABEL: sil @_TFC15objc_properties1A6method
  // CHECK: class_method {{.*}} #A.prop
  func method(x: Int) {
    prop = x
    method(prop)
  }

  // Initializers and destructors always directly access stored properties, even
  // when they are @objc.
  // CHECK-LABEL: sil @_TFC15objc_properties1AcfMS0_FT_S0_ : $@cc(method) @thin (@owned A) -> @owned A {
  // CHECK-NOT: class_method {{.*}} #A.prop
  init() {
    prop = 5
    method(prop)
    prop = 6
  }
  
  // rdar://15858869 - However, direct access only applies to (implicit or
  // explicit) 'self' ivar references, not ALL ivar refs.
  // CHECK-LABEL: sil @_TFC15objc_properties1AcfMS0_FT5otherS0_1xSi_S0_
  // CHECK-NEXT: bb0(%0 : $A, %1 : $Int, %2 : $A):
  // CHECK: [[SELF:%[0-9]+]] = mark_uninitialized [rootself] %2 : $A
  init(other : A, x : Int) {
    // CHECK: [[SELF_A:%[0-9]+]] = ref_element_addr [[SELF]] : $A, #A.prop
    // CHECK: assign %1 to [[SELF_A]]
    prop = x

    // CHECK: class_method
    // CHECK: apply
    other.prop = x
  }

  // CHECK-LABEL: sil @_TFC15objc_properties1Ad : $@cc(method) @thin (@owned A) -> @owned Builtin.NativeObject
  // CHECK-NOT: class_method {{.*}} #A.prop
  deinit {
    prop = 7
    method(prop)
  }

}

// CHECK-LABEL: sil @_TF15objc_properties11testPropGet
func testPropGet(a: A) -> Int {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.prop!getter.1.foreign : A -> () -> Int , $@cc(objc_method) @thin (A) -> Int
  return a.prop
}

// CHECK-LABEL: sil @_TF15objc_properties11testPropSet
func testPropSet(a: A, i: Int) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.prop!setter.1.foreign : A -> (Int) -> () , $@cc(objc_method) @thin (Int, A) -> ()
  a.prop = i
}

// CHECK-LABEL: sil @_TF15objc_properties19testComputedPropGet
func testComputedPropGet(a: A) -> Int {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.computedProp!getter.1.foreign : A -> () -> Int , $@cc(objc_method) @thin (A) -> Int
  return a.computedProp
}

// CHECK-LABEL: sil @_TF15objc_properties19testComputedPropSet
func testComputedPropSet(a: A, i: Int) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.computedProp!setter.1.foreign : A -> (Int) -> () , $@cc(objc_method) @thin (Int, A) -> ()
  a.computedProp = i
}

// 'super' property references.
class B : A {
  @objc override var computedProp: Int {
    // CHECK-LABEL: sil @_TFC15objc_properties1Bg12computedPropSi : $@cc(method) @thin (@owned B) -> Int
    get {
      // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $B, #A.computedProp!getter.1.foreign : A -> () -> Int , $@cc(objc_method) @thin (A) -> Int
      return super.computedProp
    }
    // CHECK-LABEL: sil @_TFC15objc_properties1Bs12computedPropSi : $@cc(method) @thin (Int, @owned B) -> ()
    set(value) {
      // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $B, #A.computedProp!setter.1.foreign : A -> (Int) -> () , $@cc(objc_method) @thin (Int, A) -> ()
      super.computedProp = value
    }
  }
}


// Test the @NSCopying attribute.
class TestNSCopying {
  // CHECK: // objc_properties.TestNSCopying.property.setter : ObjectiveC.NSString
  // CHECK-NEXT: sil [transparent] @_TFC15objc_properties13TestNSCopyings8propertyCSo8NSString : $@cc(method) @thin (@owned NSString, @owned TestNSCopying) -> ()
  // CHECK-NEXT: bb0(%0 : $NSString, %1 : $TestNSCopying):
  // CHECK:  class_method [volatile] %0 : $NSString, #NSString.copyWithZone!1.foreign
  @NSCopying var property : NSString

  @NSCopying var optionalProperty : NSString?
  @NSCopying var uncheckedOptionalProperty : NSString!

  @NSCopying weak var weakProperty : NSString? = nil
//  @NSCopying unowned var unownedProperty : NSString? = nil

  init(s : NSString) { property = s }
}


