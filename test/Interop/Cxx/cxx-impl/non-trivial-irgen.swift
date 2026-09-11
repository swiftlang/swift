// Verifies that a `@cxx @implementation` function taking or returning a
// non-trivial C++ class, or a method taking or returning one, is emitted under
// the mangled symbol of the C++ function it implements with the class passed
// indirectly (by-value parameters as the address of the caller's temporary,
// results through `sret`), and that Swift-side calls target the same foreign
// entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import NonTrivial


// int takesTracked(Tracked t);
// The parameter is the address of the caller's temporary, which the callee
// destroys under the Microsoft ABI and the caller under the Itanium ABI.
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z12takesTracked7Tracked(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?takesTracked@@YAHUTracked@@@Z"(ptr %0)
// CHECK: getelementptr inbounds{{.*}} %TSo7TrackedV, ptr %0
// CHECK-SYSV-NOT: call {{.*}}@_ZN7TrackedD1Ev
// CHECK-WIN: call {{.*}}@"??1Tracked@@QEAA@XZ"(ptr %0)
// CHECK: ret i32
@cxx @implementation
public func takesTracked(_ t: Tracked) -> Int32 { return t.value }

// int takesTwoTracked(Tracked a, Tracked b);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z15takesTwoTracked7TrackedS_(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?takesTwoTracked@@YAHUTracked@@0@Z"(ptr %0, ptr %1)
@cxx @implementation
public func takesTwoTracked(_ a: Tracked, _ b: Tracked) -> Int32 { return a.value + b.value }

// int copiesTracked(Tracked t);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z13copiesTracked7Tracked(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?copiesTracked@@YAHUTracked@@@Z"(ptr %0)
@cxx @implementation
public func copiesTracked(_ t: Tracked) -> Int32 {
  var copy = t
  copy.value += 100
  return copy.value
}

// Tracked returnsTracked(int v);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z14returnsTrackedi(ptr noalias sret(%TSo7TrackedV) %0, i32 %1)
// CHECK-WIN-LABEL: define{{.*}} @"?returnsTracked@@YA?AUTracked@@H@Z"(ptr {{[^,]*}}sret{{[^,]*}} %0, i32 %1)
@cxx @implementation
public func returnsTracked(_ v: Int32) -> Tracked { return Tracked(v) }

// Tracked passesThroughTracked(Tracked t);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z20passesThroughTracked7Tracked(ptr noalias sret(%TSo7TrackedV) %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} @"?passesThroughTracked@@YA?AUTracked@@U1@@Z"(ptr {{[^,]*}}sret{{[^,]*}} %0, ptr %1)
@cxx @implementation
public func passesThroughTracked(_ t: Tracked) -> Tracked { return t }

// int takesMovable(Movable m);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z12takesMovable7Movable(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?takesMovable@@YAHUMovable@@@Z"(ptr %0)
@cxx @implementation
public func takesMovable(_ m: Movable) -> Int32 { return m.value }

// Movable returnsMovable(int v);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z14returnsMovablei(ptr noalias sret(%TSo7MovableV) %0, i32 %1)
// CHECK-WIN-LABEL: define{{.*}} @"?returnsMovable@@YA?AUMovable@@H@Z"(ptr {{[^,]*}}sret{{[^,]*}} %0, i32 %1)
@cxx @implementation
public func returnsMovable(_ v: Int32) -> Movable { return Movable(v) }

// MoveOnly returnsMoveOnly(int v);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z15returnsMoveOnlyi(ptr noalias sret(%TSo8MoveOnlyV) %0, i32 %1)
// CHECK-WIN-LABEL: define{{.*}} @"?returnsMoveOnly@@YA?AUMoveOnly@@H@Z"(ptr {{[^,]*}}sret{{[^,]*}} %0, i32 %1)
@cxx @implementation
public func returnsMoveOnly(_ v: Int32) -> MoveOnly { return MoveOnly(v) }

// int takesPolymorphic(Polymorphic p);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z16takesPolymorphic11Polymorphic(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?takesPolymorphic@@YAHUPolymorphic@@@Z"(ptr %0)
@cxx @implementation
public func takesPolymorphic(_ p: Polymorphic) -> Int32 { return p.value + p.tag() }


// Methods

extension Box {
  // int Box::take(Tracked t) const;
  // `this` first, then the address of the argument temporary.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK3Box4takeE7Tracked(ptr %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?take@Box@@QEBAHUTracked@@@Z"(ptr %0, ptr %1)
  // CHECK: getelementptr inbounds{{.*}} %TSo3BoxV, ptr %0
  // CHECK: getelementptr inbounds{{.*}} %TSo7TrackedV, ptr %1
  @cxx @implementation
  public func take(_ t: Tracked) -> Int32 { return base + t.value }

  // int Box::add(Tracked t);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN3Box3addE7Tracked(ptr %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?add@Box@@QEAAHUTracked@@@Z"(ptr %0, ptr %1)
  @cxx @implementation
  public mutating func add(_ t: Tracked) -> Int32 {
    base += t.value
    return base
  }

  // Tracked Box::produce() const;
  // The Microsoft ABI passes `this` before the `sret` pointer.
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZNK3Box7produceEv(ptr noalias sret(%TSo7TrackedV) %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} @"?produce@Box@@QEBA?AUTracked@@XZ"(ptr %0, ptr {{[^,]*}}sret
  @cxx @implementation
  public func produce() -> Tracked { return Tracked(base) }

  // static Tracked Box::wrap(int v);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN3Box4wrapEi(ptr noalias sret(%TSo7TrackedV) %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} @"?wrap@Box@@SA?AUTracked@@H@Z"(ptr {{[^,]*}}sret{{[^,]*}} %0, i32 %1)
  @cxx @implementation
  public static func wrap(_ v: Int32) -> Tracked { return Tracked(v) }
}


// References

// int readTracked(const Tracked &t);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z11readTrackedRK7Tracked(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?readTracked@@YAHAEBUTracked@@@Z"(ptr %0)
// CHECK: getelementptr inbounds{{.*}} %TSo7TrackedV, ptr %0
@cxx @implementation
public func readTracked(_ t: UnsafePointer<Tracked>) -> Int32 { return t.pointee.value }

// void bumpTracked(Tracked &t);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z11bumpTrackedR7Tracked(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?bumpTracked@@YAXAEAUTracked@@@Z"(ptr %0)
@cxx @implementation
public func bumpTracked(_ t: UnsafeMutablePointer<Tracked>) { t.pointee.value += 1 }

// void assignTracked(Tracked &dst, const Tracked &src);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z13assignTrackedR7TrackedRKS_(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} void @"?assignTracked@@YAXAEAUTracked@@AEBU1@@Z"(ptr %0, ptr %1)
@cxx @implementation
public func assignTracked(_ dst: UnsafeMutablePointer<Tracked>, _ src: UnsafePointer<Tracked>) {
  dst.pointee = src.pointee
}


// Swift-side calls

// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}19callNonTrivialFuncsyyF"
// CHECK-SYSV:   invoke i32 @_Z12takesTracked7Tracked
// CHECK-SYSV:   invoke i32 @_Z15takesTwoTracked7TrackedS_
// CHECK-SYSV:   invoke void @_Z14returnsTrackedi
// CHECK-SYSV:   invoke void @_Z20passesThroughTracked7Tracked
// CHECK-SYSV:   invoke i32 @_Z12takesMovable7Movable
// CHECK-SYSV:   invoke i32 @_Z16takesPolymorphic11Polymorphic
// CHECK-SYSV:   invoke i32 @_ZNK3Box4takeE7Tracked
// CHECK-SYSV:   invoke i32 @_ZN3Box3addE7Tracked
// CHECK-SYSV:   invoke void @_ZNK3Box7produceEv
// CHECK-SYSV:   invoke void @_ZN3Box4wrapEi
// CHECK-SYSV:   invoke i32 @_Z11readTrackedRK7Tracked
// CHECK-SYSV:   invoke void @_Z11bumpTrackedR7Tracked
// CHECK-SYSV:   invoke void @_Z13assignTrackedR7TrackedRKS_
public func callNonTrivialFuncs() {
  var t = Tracked(1)
  _ = takesTracked(t)
  _ = takesTwoTracked(t, t)
  _ = returnsTracked(2)
  _ = passesThroughTracked(t)
  _ = takesMovable(Movable(3))
  _ = takesPolymorphic(Polymorphic(4))

  var box = Box(base: 10)
  _ = box.take(t)
  _ = box.add(t)
  _ = box.produce()
  _ = Box.wrap(5)

  let u = Tracked(6)
  _ = readTracked(t)
  bumpTracked(&t)
  assignTracked(&t, u)
}
