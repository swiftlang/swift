// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/keypaths_objc.h %s | %FileCheck %s
// RUN: %target-swift-emit-ir(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/keypaths_objc.h %s
// REQUIRES: objc_interop

import Foundation

struct NonObjC {
  var x: Int
  var y: NSObject
}

class Foo: NSObject {
  @objc var int: Int { fatalError() }
  @objc var bar: Bar { fatalError() }
  var nonobjc: NonObjC { fatalError() }
  @objc(thisIsADifferentName) var differentName: Bar { fatalError() }

  @objc subscript(x: Int) -> Foo { return self }
  @objc subscript(x: Bar) -> Foo { return self }

  @objc dynamic var dyn: String { fatalError() }
}

class Bar: NSObject {
  @objc var foo: Foo { fatalError() }
}

// CHECK-LABEL: sil hidden [ossa] @$s13keypaths_objc0B8KeypathsyyF
func objcKeypaths() {
  // CHECK: keypath $WritableKeyPath<NonObjC, Int>, (root
  _ = \NonObjC.x
  // CHECK: keypath $WritableKeyPath<NonObjC, NSObject>, (root
  _ = \NonObjC.y
  // CHECK: keypath $KeyPath<Foo, Int>, (objc "int"
  _ = \Foo.int
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "bar"
  _ = \Foo.bar
  // CHECK: keypath $KeyPath<Foo, Foo>, (objc "bar.foo"
  _ = \Foo.bar.foo
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "bar.foo.bar"
  _ = \Foo.bar.foo.bar
  // CHECK: keypath $KeyPath<Foo, NonObjC>, (root
  _ = \Foo.nonobjc
  // CHECK: keypath $KeyPath<Foo, NSObject>, (root
  _ = \Foo.bar.foo.nonobjc.y
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "thisIsADifferentName"
  _ = \Foo.differentName
}

// CHECK-LABEL: sil hidden [ossa] @$s13keypaths_objc0B18KeypathIdentifiersyyF
func objcKeypathIdentifiers() {
  // CHECK: keypath $KeyPath<ObjCFoo, String>, (objc "objcProp"; {{.*}} id #ObjCFoo.objcProp!getter.foreign
  _ = \ObjCFoo.objcProp
  // CHECK: keypath $KeyPath<Foo, String>, (objc "dyn"; {{.*}} id #Foo.dyn!getter.foreign
  _ = \Foo.dyn
  // CHECK: keypath $KeyPath<Foo, Int>, (objc "int"; {{.*}} id #Foo.int!getter :
  _ = \Foo.int
}

struct X {}

extension NSObject {
    var x: X { return X() }
    @objc var objc: Int { return 0 }
    @objc dynamic var dynamic: Int { return 0 }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}nonobjcExtensionOfObjCClass
func nonobjcExtensionOfObjCClass() {
  // Should be treated as a statically-dispatch property
  // CHECK: keypath $KeyPath<NSObject, X>, ({{.*}} id @
  _ = \NSObject.x
  // CHECK: keypath $KeyPath<NSObject, Int>, ({{.*}} id #NSObject.objc!getter.foreign
  _ = \NSObject.objc
  // CHECK: keypath $KeyPath<NSObject, Int>, ({{.*}} id #NSObject.dynamic!getter.foreign
  _ = \NSObject.dynamic

}

@objc protocol ObjCProto {
  var objcRequirement: Int { get set }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}ProtocolRequirement
func objcProtocolRequirement<T: ObjCProto>(_: T) {
  // CHECK: keypath {{.*}} id #ObjCProto.objcRequirement!getter.foreign
  _ = \T.objcRequirement
  // CHECK: keypath {{.*}} id #ObjCProto.objcRequirement!getter.foreign
  _ = \ObjCProto.objcRequirement
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}externalObjCProperty
func externalObjCProperty() {
  // Pure ObjC-dispatched properties do not have external descriptors.
  // CHECK: keypath $KeyPath<NSObject, String>, 
  // CHECK-NOT: external #NSObject.description
  _ = \NSObject.description
}

func sharedCProperty() {
  // CHECK:  keypath $WritableKeyPath<c_union, some_struct>
  // CHECK-NOT: external #c_union.some_field
  let dataKeyPath: WritableKeyPath<c_union, some_struct>? = \c_union.some_field
}

class OverrideFrameworkObjCProperty: A {
  override var counter: Int32 {
    get { return 0 }
    set { }
  }
}

func overrideFrameworkObjCProperty() {
  let _ = \OverrideFrameworkObjCProperty.counter
}

@dynamicMemberLookup
class DynamicClass<Root> {
    init() {}
    subscript<T>(dynamicMember member: KeyPath<Root, T>) -> DynamicClass<T> {
        fatalError()
    }
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}dynamicMemberLookupSimple
func dynamicMemberLookupSimple(foo: DynamicClass<Foo>, nonobjc: DynamicClass<NonObjC>) {
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "bar"
  _ = foo.bar
  // CHECK: keypath $KeyPath<Foo, Int>, (objc "int"
  _ = foo.int
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "bar"
  // CHECK: keypath $KeyPath<Bar, Foo>, (objc "foo"
  _ = foo.bar.foo
  // CHECK: keypath $KeyPath<Foo, NonObjC>, (root
  _ = foo.nonobjc
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "thisIsADifferentName"
  _ = foo.differentName
  // CHECK: keypath $KeyPath<NonObjC, Int>, (root
  _ = nonobjc.x
  // CHECK: keypath $KeyPath<NonObjC, NSObject>, (root
   _ = nonobjc.y
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}dynamicMemberLookupNestedKeypaths
func dynamicMemberLookupNestedKeypaths(foo: DynamicClass<Foo>) {
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "bar" 
  // CHECK: keypath $KeyPath<Bar, Foo>, (objc "foo" 
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "bar" 
  _ = foo.bar.foo.bar
}

// CHECK-LABEL: sil hidden [ossa] @{{.*}}dynamicMemberLookupMixedKeypaths
func dynamicMemberLookupMixedKeypaths(foo: DynamicClass<Foo>) {
  // CHECK: keypath $KeyPath<Foo, Bar>, (objc "bar"
  // CHECK: keypath $KeyPath<Bar, Foo>, (objc "foo"
  // CHECK: keypath $KeyPath<Foo, NonObjC>, (root
  // CHECK: keypath $KeyPath<NonObjC, NSObject>, (root
  _ = foo.bar.foo.nonobjc.y 
}