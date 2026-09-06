// Verifies that a `@cxx @implementation` of a C++ virtual method is emitted
// under the method's own mangled symbol for a value record (including an
// accepted override) and for a foreign reference type, that Swift does not
// emit any vtable (the C++ translation unit defining the key function does),
// and that Swift-side calls keep their dispatch: static (the plain method
// symbol) for a value record, dynamic (the importer's synthesized thunk) for
// a foreign reference type.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-availability-checking \
// RUN:   -I %S/Inputs \
// RUN:   %s -o %t.ll
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi < %t.ll
// RUN: %FileCheck %s --check-prefix=NOVTABLE < %t.ll

// REQUIRES: swift_feature_CxxImplementation

import Virtual


extension Shape {
  // virtual int Shape::area() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK5Shape4areaEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?area@Shape@@UEBAHXZ"(ptr %0)
  @cxx @implementation
  public func area() -> Int32 { return sides * sides }

  // virtual void Shape::scale(int factor);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN5Shape5scaleEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} void @"?scale@Shape@@UEAAXH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public mutating func scale(_ factor: Int32) { sides *= factor }
}

extension SimpleBase {
  // virtual int SimpleBase::simple() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK10SimpleBase6simpleEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?simple@SimpleBase@@UEBAHXZ"(ptr %0)
  @cxx @implementation
  public func simple() -> Int32 { return stored }
}

// An accepted override (single, non-virtual inheritance, unchanged return
// type) is emitted under its own symbol, which SimpleDerived's vtable slot
// names.
extension SimpleDerived {
  // int SimpleDerived::simple() const override;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK13SimpleDerived6simpleEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?simple@SimpleDerived@@UEBAHXZ"(ptr %0)
  @cxx @implementation
  public func simple() -> Int32 { return stored * 2 }
}

extension Engine {
  // virtual int Engine::status() const; `self` is the reference, i.e. `this`.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK6Engine6statusEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?status@Engine@@UEBAHXZ"(ptr %0)
  // CHECK: getelementptr inbounds{{.*}} %TSo6EngineV, ptr %0
  @cxx @implementation
  public func status() -> Int32 { return rpm }

  // virtual void Engine::boost(int amount);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN6Engine5boostEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} void @"?boost@Engine@@UEAAXH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public func boost(_ amount: Int32) { rpm += amount }
}


// Swift-side calls: a value record's virtual method dispatches statically to
// the method symbol itself; a foreign reference type's dispatches dynamically
// through the importer's synthesized thunk.

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}16callVirtualFuncsys5Int32VSo5ShapeVz_So6EngineVtF"
// CHECK-SYSV:   invoke void @_ZN5Shape5scaleEi(ptr %0, i32 2)
// CHECK-SYSV:   invoke void @_ZN6Engine30__synthesizedVirtualCall_boostEi(ptr %1, i32 3)
// CHECK-SYSV:   invoke i32 @_ZNK5Shape4areaEv(ptr %0)
// CHECK-SYSV:   invoke i32 @_ZNK6Engine31__synthesizedVirtualCall_statusEv(ptr %1)
public func callVirtualFuncs(_ s: inout Shape, _ e: Engine) -> Int32 {
  s.scale(2)
  e.boost(3)
  return s.area() + e.status()
}

// CHECK-SYSV: define linkonce_odr{{.*}} void @_ZN6Engine30__synthesizedVirtualCall_boostEi
// CHECK-SYSV: define linkonce_odr{{.*}} i32 @_ZNK6Engine31__synthesizedVirtualCall_statusEv

// Swift must not emit any vtable: the C++ translation unit that defines the
// key function does.
// NOVTABLE-NOT: @_ZTV
// NOVTABLE-NOT: @"??_7
