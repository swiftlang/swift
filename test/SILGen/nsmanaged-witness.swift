// RUN: %target-swift-emit-silgen -enable-sil-ownership -sdk %S/Inputs %s -I %S/Inputs -enable-source-import | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

@objc protocol ObjCReadOnly {
  var name: String { get }
}

@objc protocol ObjCReadWrite {
  var name: String { get set }
}

class SomeObject: NSObject, ObjCReadOnly, ObjCReadWrite {
  @NSManaged var name: String
}

// We should not emit references to native Swift accessors for @NSManaged
// properties.
// CHECK-NOT: hidden_external {{.*}}main{{.*}}SomeObject{{.*}}name

protocol NativeReadWrite {
  var name: String { get set }
}

protocol AnotherNativeReadWrite {
  var name: String { get set }
}

class SomeOtherObject: NSObject, NativeReadWrite {
  @NSManaged var name: String
}

// CHECK-NOT: hidden_external {{.*}}main{{.*}}SomeOtherObject{{.*}}name

class DynamicSubObject: DynamicObject, AnotherNativeReadWrite {
  override var name: String {
    get { return "" }
    set {}
  }
}

// CHECK-NOT: hidden_external {{.*}}main{{.*}}DynamicSubObject{{.*}}name

class DynamicObject: NativeReadWrite {
  @objc dynamic var name: String = ""
}

// CHECK-NOT: hidden_external {{.*}}main{{.*}}DynamicObject{{.*}}name

protocol NativeIntProperty {
  var intProperty: Int32 { get set }
}

// Foo is defined in ObjC with an 'intProperty' property
extension Foo: NativeIntProperty {}

// CHECK-NOT: hidden_external {{.*}}Foo{{.*}}intProperty

// TODO: We can't emit a vtable entry for modify for ObjC types.
// CHECK-NOT: class_method {{.*}}Foo{{.*}}intProperty{{.*}}modify

// CHECK-LABEL: sil shared [serializable] @$SSo3FooC11intPropertys5Int32VvM

