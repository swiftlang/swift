// RUN: %target-swift-emit-silgen -enable-sil-ownership %s -emit-verbose-sil -sdk %S/Inputs -I %S/Inputs -enable-source-import | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class A {
  @objc dynamic var prop: Int
  @objc dynamic var computedProp: Int {
    get {
      return 5
    }
    set {}
  }

  // Regular methods go through the @objc property accessors.
  // CHECK-LABEL: sil hidden @$S15objc_properties1AC6method{{[_0-9a-zA-Z]*}}F
  // CHECK: objc_method {{.*}} #A.prop
  func method(_ x: Int) {
    prop = x
    method(prop)
  }

  // Initializers and destructors always directly access stored properties, even
  // when they are @objc.
  // CHECK-LABEL: sil hidden @$S15objc_properties1AC{{[_0-9a-zA-Z]*}}fc
  // CHECK-NOT: class_method {{.*}} #A.prop
  init() {
    prop = 5
    method(prop)
    prop = 6
  }

  // rdar://15858869 - However, direct access only applies to (implicit or
  // explicit) 'self' ivar references, not ALL ivar refs.
  // CHECK-LABEL: sil hidden @$S15objc_properties1AC{{[_0-9a-zA-Z]*}}fc
  // CHECK: bb0(%0 : @owned $A, %1 : @trivial $Int, [[OLD_SELF:%.*]] : @owned $A):
  // CHECK: [[SELF:%[0-9]+]] = mark_uninitialized [rootself] [[OLD_SELF]] : $A
  init(other : A, x : Int) {
    // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
    // CHECK: [[SELF_A:%[0-9]+]] = ref_element_addr [[BORROWED_SELF]] : $A, #A.prop
    // CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[SELF_A]] : $*Int
    // CHECK: assign %1 to [[WRITE]]
    // CHECK: end_access [[WRITE]] : $*Int
    // CHECK: end_borrow [[BORROWED_SELF]] from [[SELF]]
    prop = x

    // CHECK: objc_method
    // CHECK: apply
    other.prop = x
  }

  // CHECK-LABEL: sil hidden @$S15objc_properties1ACfd : $@convention(method) (@guaranteed A) -> @owned Builtin.NativeObject {
  // CHECK-NOT:     class_method {{.*}} #A.prop
  // CHECK:       }
  deinit {
    prop = 7
    method(prop)
  }

}

// CHECK-LABEL: sil hidden @$S15objc_properties11testPropGet{{[_0-9a-zA-Z]*}}F
func testPropGet(_ a: A) -> Int {
  // CHECK: objc_method [[OBJ:%[0-9]+]] : $A, #A.prop!getter.1.foreign : (A) -> () -> Int, $@convention(objc_method) (A) -> Int
  return a.prop
}

// CHECK-LABEL: sil hidden @$S15objc_properties11testPropSet{{[_0-9a-zA-Z]*}}F
func testPropSet(_ a: A, i: Int) {
  // CHECK: objc_method [[OBJ:%[0-9]+]] : $A, #A.prop!setter.1.foreign : (A) -> (Int) -> (), $@convention(objc_method) (Int, A) -> ()
  a.prop = i
}

// CHECK-LABEL: sil hidden @$S15objc_properties19testComputedPropGet{{[_0-9a-zA-Z]*}}F
func testComputedPropGet(_ a: A) -> Int {
  // CHECK: objc_method [[OBJ:%[0-9]+]] : $A, #A.computedProp!getter.1.foreign : (A) -> () -> Int, $@convention(objc_method) (A) -> Int
  return a.computedProp
}

// CHECK-LABEL: sil hidden @$S15objc_properties19testComputedPropSet{{[_0-9a-zA-Z]*}}F
func testComputedPropSet(_ a: A, i: Int) {
  // CHECK: objc_method [[OBJ:%[0-9]+]] : $A, #A.computedProp!setter.1.foreign : (A) -> (Int) -> (), $@convention(objc_method) (Int, A) -> ()
  a.computedProp = i
}

// 'super' property references.
class B : A {
  @objc override var computedProp: Int {
    // CHECK-LABEL: sil hidden @$S15objc_properties1BC12computedPropSivg : $@convention(method) (@guaranteed B) -> Int
    get {
      // CHECK: objc_super_method [[SELF:%[0-9]+]] : $B, #A.computedProp!getter.1.foreign : (A) -> () -> Int, $@convention(objc_method) (A) -> Int
      return super.computedProp
    }
    // CHECK-LABEL: sil hidden @$S15objc_properties1BC12computedPropSivs : $@convention(method) (Int, @guaranteed B) -> ()
    set(value) {
      // CHECK: objc_super_method [[SELF:%[0-9]+]] : $B, #A.computedProp!setter.1.foreign : (A) -> (Int) -> (), $@convention(objc_method) (Int, A) -> ()
      super.computedProp = value
    }
  }
}


// Test the @NSCopying attribute.
class TestNSCopying {
  // CHECK-LABEL: sil hidden [transparent] @$S15objc_properties13TestNSCopyingC8propertySo8NSStringCvs : $@convention(method) (@owned NSString, @guaranteed TestNSCopying) -> ()
  // CHECK: bb0([[ARG0:%.*]] : @owned $NSString, [[ARG1:%.*]] : @guaranteed $TestNSCopying):
  // CHECK:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK:   objc_method [[BORROWED_ARG0]] : $NSString, #NSString.copy!1.foreign
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
  // CHECK-DAG: sil hidden @$S15objc_properties9SingletonC14sharedInstanceACvgZ : $@convention(method) (@thick Singleton.Type) -> @owned Singleton
  // CHECK-DAG: sil hidden [thunk] @$S15objc_properties9SingletonC14sharedInstanceACvgZTo : $@convention(objc_method) (@objc_metatype Singleton.Type) -> @autoreleased Singleton {
  @objc static let sharedInstance = Singleton()

  // CHECK-DAG: sil hidden @$S15objc_properties9SingletonC1iSivgZ : $@convention(method) (@thick Singleton.Type) -> Int
  // CHECK-DAG: sil hidden [thunk] @$S15objc_properties9SingletonC1iSivgZTo : $@convention(objc_method) (@objc_metatype Singleton.Type) -> Int
  @objc static let i = 2

  // CHECK-DAG: sil hidden @$S15objc_properties9SingletonC1jSSvgZ : $@convention(method) (@thick Singleton.Type) -> @owned String
  // CHECK-DAG: sil hidden [thunk] @$S15objc_properties9SingletonC1jSSvgZTo : $@convention(objc_method) (@objc_metatype Singleton.Type) -> @autoreleased NSString
  // CHECK-DAG: sil hidden @$S15objc_properties9SingletonC1jSSvsZ : $@convention(method) (@owned String, @thick Singleton.Type) -> ()
  // CHECK-DAG: sil hidden [thunk] @$S15objc_properties9SingletonC1jSSvsZTo : $@convention(objc_method) (NSString, @objc_metatype Singleton.Type) -> ()
  @objc static var j = "Hello"

  // CHECK-DAG: sil hidden [thunk] @$S15objc_properties9SingletonC1kSdvgZTo : $@convention(objc_method) (@objc_metatype Singleton.Type) -> Double
  // CHECK-DAG: sil hidden @$S15objc_properties9SingletonC1kSdvgZ : $@convention(method) (@thick Singleton.Type) -> Double
  @objc static var k: Double {
    return 7.7
  }
}

class HasUnmanaged : NSObject {
  // CHECK-LABEL: sil hidden [thunk] @$S15objc_properties12HasUnmanagedC3refs0D0VyyXlGSgvgTo
  // CHECK: bb0([[CLS:%.*]] : @unowned $HasUnmanaged):
  // CHECK:     [[CLS_COPY:%.*]] = copy_value [[CLS]]
  // CHECK:     [[BORROWED_CLS_COPY:%.*]] = begin_borrow [[CLS_COPY]]
  // CHECK:     [[NATIVE:%.+]] = function_ref @$S15objc_properties12HasUnmanagedC3refs0D0VyyXlGSgvg
  // CHECK:     [[RESULT:%.+]] = apply [[NATIVE]]([[BORROWED_CLS_COPY]])
  // CHECK:     end_borrow [[BORROWED_CLS_COPY]] from [[CLS_COPY]]
  // CHECK-NOT: {{(retain|release)}}
  // CHECK:     destroy_value [[CLS_COPY]] : $HasUnmanaged
  // CHECK-NOT: {{(retain|release)}}
  // CHECK:     return [[RESULT]] : $Optional<Unmanaged<AnyObject>>
  // CHECK: } // end sil function '$S15objc_properties12HasUnmanagedC3refs0D0VyyXlGSgvgTo'

  // CHECK-LABEL: sil hidden [thunk] @$S15objc_properties12HasUnmanagedC3refs0D0VyyXlGSgvsTo
  // CHECK: bb0([[NEW_VALUE:%.*]] : @trivial $Optional<Unmanaged<AnyObject>>, [[SELF:%.*]] : @unowned $HasUnmanaged):
  // CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $HasUnmanaged
  // CHECK-NEXT: [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[NATIVE:%.+]] = function_ref @$S15objc_properties12HasUnmanagedC3refs0D0VyyXlGSgvs
  // CHECK-NEXT: [[RESULT:%.*]] = apply [[NATIVE]]([[NEW_VALUE]], [[BORROWED_SELF_COPY]])
  // CHECK-NEXT: end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT: destroy_value [[SELF_COPY]] : $HasUnmanaged
  // CHECK-NEXT: return [[RESULT:%.*]]
  // CHECK: } // end sil function '$S15objc_properties12HasUnmanagedC3refs0D0VyyXlGSgvsTo'
  @objc var ref: Unmanaged<AnyObject>?
}

@_silgen_name("autoreleasing_user")
func useAutoreleasingUnsafeMutablePointer(_ a: AutoreleasingUnsafeMutablePointer<NSObject>)

class NonObjCClassWithObjCProperty {
  var property: NSObject

  init(_ newValue: NSObject) {
    property = newValue
  }

  // CHECK-LABEL: sil hidden @$S15objc_properties016NonObjCClassWithD9CPropertyC11usePropertyyyF : $@convention(method) (@guaranteed NonObjCClassWithObjCProperty) -> () {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $NonObjCClassWithObjCProperty):
  // CHECK: [[MATERIALIZE_FOR_SET:%.*]] = class_method [[ARG]] : $NonObjCClassWithObjCProperty, #NonObjCClassWithObjCProperty.property!materializeForSet.1
  // CHECK: [[TUPLE:%.*]] = apply [[MATERIALIZE_FOR_SET]]({{.*}}, {{.*}}, [[ARG]])
  // CHECK: [[RAW_POINTER:%.*]] = tuple_extract [[TUPLE]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>), 0
  // CHECK: [[OBJECT:%.*]] = pointer_to_address [[RAW_POINTER]] : $Builtin.RawPointer to [strict] $*NSObject
  // CHECK: [[OBJECT_DEP:%.*]] = mark_dependence [[OBJECT]] : $*NSObject on [[ARG]]
  // CHECK: [[OBJECT_ACCESS:%.*]] = begin_access [modify] [unsafe] [[OBJECT_DEP]] : $*NSObject
  // CHECK: [[LOADED_OBJECT:%.*]] = load_borrow [[OBJECT_ACCESS]]
  // CHECK: [[UNMANAGED_OBJECT:%.*]] = ref_to_unmanaged [[LOADED_OBJECT]] : $NSObject to $@sil_unmanaged NSObject
  // CHECK: end_borrow [[LOADED_OBJECT]] from [[OBJECT_ACCESS]]
  func useProperty() {
    useAutoreleasingUnsafeMutablePointer(&property)
  }
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

// CHECK-LABEL: sil hidden [thunk] @$S15objc_properties12ObjCSubclassC8propertySivgTo
// CHECK-LABEL: sil hidden [thunk] @$S15objc_properties12ObjCSubclassC8propertySivsTo

// Make sure lazy properties that witness @objc protocol requirements are
// correctly formed
//
// <https://bugs.swift.org/browse/SR-1825>

@objc protocol HasProperty {
    @objc var window: NSObject? { get set }
}

class HasLazyProperty : NSObject, HasProperty {
  func instanceMethod() -> NSObject? {
    return nil
  }

  lazy var window = self.instanceMethod()
}

// CHECK-LABEL: sil hidden @$S15objc_properties15HasLazyPropertyC6windowSo8NSObjectCSgvg : $@convention(method) (@guaranteed HasLazyProperty) -> @owned Optional<NSObject> {
// CHECK: class_method %0 : $HasLazyProperty, #HasLazyProperty.instanceMethod!1 : (HasLazyProperty) -> () -> NSObject?
// CHECK: return

//   The way we import this setter splits the name into the parameter list,
//   which can cause fits for SILGenApply the way it's currently implemented.
// CHECK-LABEL: sil hidden @$S15objc_properties26testPropSetWithPreposition
func testPropSetWithPreposition(object: ObjectWithSplitProperty?) {
  // CHECK: #ObjectWithSplitProperty.flagForSomething!setter.1.foreign : (ObjectWithSplitProperty) -> (Bool) -> (), $@convention(objc_method) ({{Bool|ObjCBool}}, ObjectWithSplitProperty) -> ()
  object?.flagForSomething = false
}
