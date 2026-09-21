// A generic method of a class is dispatched statically in Embedded Swift and
// kept out of the vtable, because there is no unspecialized implementation to
// put in a vtable slot. A non-generic sibling keeps its entry and stays
// overridable.
//
// RUN: %target-swift-emit-sil %s -enable-experimental-feature Embedded -parse-stdlib -wmo -module-name cgv -o - | %FileCheck %s

// REQUIRES: swift_feature_Embedded

public class Base {
  public func generic<T>(_: T) { }
  public func nonGeneric() { }
  public final func finalNonGeneric() { }
}

public class Derived: Base {
  public override func nonGeneric() { }
}

// A generic class: a method that is generic only over the class's own
// parameters is *not* more generic than the class, so it keeps its entry.
public class GenericBase<T> {
  public func usesClassParam(_: T) { }
  public func alsoGeneric<U>(_: U) { }
}

// CHECK-LABEL: sil_vtable Base {
// CHECK-NOT:     #Base.generic
// CHECK-DAG:     #Base.nonGeneric
// CHECK-NOT:     #Base.generic
// CHECK:       }

// The `final` method is excluded for the usual reason, not this one.
// CHECK-LABEL: sil_vtable Derived {
// CHECK-NOT:     #Base.generic
// CHECK-DAG:     #Base.nonGeneric{{.*}}override
// CHECK-NOT:     #Base.generic
// CHECK:       }

// CHECK-LABEL: sil_vtable GenericBase {
// CHECK-NOT:     #GenericBase.alsoGeneric
// CHECK-DAG:     #GenericBase.usesClassParam
// CHECK-NOT:     #GenericBase.alsoGeneric
// CHECK:       }
