// RUN: %target-swift-frontend %s -emit-silgen -emit-verbose-sil -sdk %S/Inputs -I %S/Inputs -enable-source-import | %FileCheck %s

// REQUIRES: objc_interop

import Foundation


class A {
  dynamic var prop: Int
  dynamic var computedProp: Int {
    get {
      return 5
    }
    set {}
  }

  // Regular methods go through the @objc property accessors.
  // CHECK-LABEL: sil hidden @_TFC15objc_properties1A6method
  // CHECK: class_method {{.*}} #A.prop
  func method(_ x: Int) {
    prop = x
    method(prop)
  }

  // Initializers and destructors always directly access stored properties, even
  // when they are @objc.
  // CHECK-LABEL: sil hidden @_TFC15objc_properties1Ac
  // CHECK-NOT: class_method {{.*}} #A.prop
  init() {
    prop = 5
    method(prop)
    prop = 6
  }

  // rdar://15858869 - However, direct access only applies to (implicit or
  // explicit) 'self' ivar references, not ALL ivar refs.
  // CHECK-LABEL: sil hidden @_TFC15objc_properties1Ac
  // CHECK: bb0(%0 : $A, %1 : $Int, %2 : $A):
  // CHECK: [[SELF:%[0-9]+]] = mark_uninitialized [rootself] %2 : $A
  init(other : A, x : Int) {
    // CHECK: [[SELF_A:%[0-9]+]] = ref_element_addr [[SELF]] : $A, #A.prop
    // CHECK: assign %1 to [[SELF_A]]
    prop = x

    // CHECK: class_method
    // CHECK: apply
    other.prop = x
  }

  // CHECK-LABEL: sil hidden @_TFC15objc_properties1Ad : $@convention(method) (@guaranteed A) -> @owned Builtin.NativeObject {
  // CHECK-NOT:     class_method {{.*}} #A.prop
  // CHECK:       }
  deinit {
    prop = 7
    method(prop)
  }

}

// CHECK-LABEL: sil hidden @_TF15objc_properties11testPropGet
func testPropGet(_ a: A) -> Int {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.prop!getter.1.foreign : (A) -> () -> Int , $@convention(objc_method) (A) -> Int
  return a.prop
}

// CHECK-LABEL: sil hidden @_TF15objc_properties11testPropSet
func testPropSet(_ a: A, i: Int) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.prop!setter.1.foreign : (A) -> (Int) -> () , $@convention(objc_method) (Int, A) -> ()
  a.prop = i
}

// CHECK-LABEL: sil hidden @_TF15objc_properties19testComputedPropGet
func testComputedPropGet(_ a: A) -> Int {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.computedProp!getter.1.foreign : (A) -> () -> Int , $@convention(objc_method) (A) -> Int
  return a.computedProp
}

// CHECK-LABEL: sil hidden @_TF15objc_properties19testComputedPropSet
func testComputedPropSet(_ a: A, i: Int) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.computedProp!setter.1.foreign : (A) -> (Int) -> () , $@convention(objc_method) (Int, A) -> ()
  a.computedProp = i
}

// 'super' property references.
class B : A {
  @objc override var computedProp: Int {
    // CHECK-LABEL: sil hidden @_TFC15objc_properties1Bg12computedPropSi : $@convention(method) (@guaranteed B) -> Int
    get {
      // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $B, #A.computedProp!getter.1.foreign : (A) -> () -> Int , $@convention(objc_method) (A) -> Int
      return super.computedProp
    }
    // CHECK-LABEL: sil hidden @_TFC15objc_properties1Bs12computedPropSi : $@convention(method) (Int, @guaranteed B) -> ()
    set(value) {
      // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $B, #A.computedProp!setter.1.foreign : (A) -> (Int) -> () , $@convention(objc_method) (Int, A) -> ()
      super.computedProp = value
    }
  }
}


// Test the @NSCopying attribute.
class TestNSCopying {
  // CHECK-LABEL: sil hidden [transparent] @_TFC15objc_properties13TestNSCopyings8propertyCSo8NSString : $@convention(method) (@owned NSString, @guaranteed TestNSCopying) -> ()
  // CHECK: bb0(%0 : $NSString, %1 : $TestNSCopying):
  // CHECK:  class_method [volatile] %0 : $NSString, #NSString.copy!1.foreign
  @NSCopying var property : NSString

  @NSCopying var optionalProperty : NSString?
  @NSCopying var uncheckedOptionalProperty : NSString!

  @NSCopying weak var weakProperty : NSString? = nil
//  @NSCopying unowned var unownedProperty : NSString? = nil

  init(s : NSString) { property = s }
}


// <rdar://problem/16663515> IBOutlet not adjusting getter/setter when making a property implicit unchecked optional
@objc
class TestComputedOutlet : NSObject {
  var _disclosedView : TestComputedOutlet! = .none

  @IBOutlet var disclosedView : TestComputedOutlet! {
  get { return _disclosedView }
  set { _disclosedView = newValue }
  }

  func foo() {
    _disclosedView != nil ? () : self.disclosedView.foo()
  }
}

class Singleton : NSObject {
  // CHECK-DAG: sil hidden @_TZFC15objc_properties9Singletong14sharedInstanceS0_ : $@convention(method) (@thick Singleton.Type) -> @owned Singleton
  // CHECK-DAG: sil hidden [thunk] @_TToZFC15objc_properties9Singletong14sharedInstanceS0_ : $@convention(objc_method) (@objc_metatype Singleton.Type) -> @autoreleased Singleton {
  static let sharedInstance = Singleton()

  // CHECK-DAG: sil hidden @_TZFC15objc_properties9Singletong1iSi : $@convention(method) (@thick Singleton.Type) -> Int
  // CHECK-DAG: sil hidden [thunk] @_TToZFC15objc_properties9Singletong1iSi : $@convention(objc_method) (@objc_metatype Singleton.Type) -> Int
  static let i = 2

  // CHECK-DAG: sil hidden @_TZFC15objc_properties9Singletong1jSS : $@convention(method) (@thick Singleton.Type) -> @owned String
  // CHECK-DAG: sil hidden [thunk] @_TToZFC15objc_properties9Singletong1jSS : $@convention(objc_method) (@objc_metatype Singleton.Type) -> @autoreleased NSString
  // CHECK-DAG: sil hidden @_TZFC15objc_properties9Singletons1jSS : $@convention(method) (@owned String, @thick Singleton.Type) -> ()
  // CHECK-DAG: sil hidden [thunk] @_TToZFC15objc_properties9Singletons1jSS : $@convention(objc_method) (NSString, @objc_metatype Singleton.Type) -> ()
  static var j = "Hello"

  // CHECK-DAG: sil hidden [thunk] @_TToZFC15objc_properties9Singletong1kSd : $@convention(objc_method) (@objc_metatype Singleton.Type) -> Double
  // CHECK-DAG: sil hidden @_TZFC15objc_properties9Singletong1kSd : $@convention(method) (@thick Singleton.Type) -> Double
  static var k: Double {
    return 7.7
  }
}

class HasUnmanaged : NSObject {
  // CHECK-LABEL: sil hidden [thunk] @_TToFC15objc_properties12HasUnmanagedg3refGSqGVs9UnmanagedPs9AnyObject___
  // CHECK: bb0([[CLS:%.*]] : $HasUnmanaged):
  // CHECK:     [[CLS_COPY:%.*]] = copy_value [[CLS]]
  // CHECK:     [[NATIVE:%.+]] = function_ref @_TFC15objc_properties12HasUnmanagedg3refGSqGVs9UnmanagedPs9AnyObject___
  // CHECK:     [[RESULT:%.+]] = apply [[NATIVE]]([[CLS_COPY]])
  // CHECK-NOT: {{(retain|release)}}
  // CHECK:     destroy_value [[CLS_COPY]] : $HasUnmanaged
  // CHECK-NOT: {{(retain|release)}}
  // CHECK:     return [[RESULT]] : $Optional<Unmanaged<AnyObject>>
  // CHECK: } // end sil function '_TToFC15objc_properties12HasUnmanagedg3refGSqGVs9UnmanagedPs9AnyObject___'

  // CHECK-LABEL: sil hidden [thunk] @_TToFC15objc_properties12HasUnmanageds3refGSqGVs9UnmanagedPs9AnyObject___
  // CHECK: bb0([[NEW_VALUE:%.*]] : $Optional<Unmanaged<AnyObject>>, [[SELF:%.*]] : $HasUnmanaged):
  // CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $HasUnmanaged
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[NATIVE:%.+]] = function_ref @_TFC15objc_properties12HasUnmanageds3refGSqGVs9UnmanagedPs9AnyObject___
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[NATIVE]]([[NEW_VALUE]], [[SELF_COPY]])
  // CHECK-NEXT: destroy_value [[SELF_COPY]] : $HasUnmanaged
  // CHECK-NEXT: return [[RESULT:%.*]]
  // CHECK: } // end sil function '_TToFC15objc_properties12HasUnmanageds3refGSqGVs9UnmanagedPs9AnyObject___'
  var ref: Unmanaged<AnyObject>?
}


// <rdar://problem/21544588> crash when overriding non-@objc property with @objc property.
class NonObjCBaseClass : NSObject {
  @nonobjc var property: Int {
    get { return 0 }
    set {}
  }
}

@objc class ObjCSubclass : NonObjCBaseClass {
  @objc override var property: Int {
    get { return 1 }
    set {}
  }
}

// CHECK-LABEL: sil hidden [thunk] @_TToFC15objc_properties12ObjCSubclassg8propertySi
// CHECK-LABEL: sil hidden [thunk] @_TToFC15objc_properties12ObjCSubclasss8propertySi
