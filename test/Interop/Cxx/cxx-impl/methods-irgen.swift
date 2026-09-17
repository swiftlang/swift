// Verifies that a `@cxx @implementation` method in an extension of an imported
// C++ struct is emitted under the mangled symbol of the C++ method it
// implements, with the C++ method ABI (`this` first, after an indirect result
// on the Itanium ABI and before it on the Microsoft ABI), and that Swift-side
// calls target the same foreign entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import Methods


extension Counter {
  // static Counter Counter::make(int v);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN7Counter4makeEi(i32 %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?make@Counter@@SA?AU1@H@Z"(i32 %0)
  @cxx @implementation
  public static func make(_ v: Int32) -> Counter { return Counter(value: v) }

  // int Counter::get() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK7Counter3getEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?get@Counter@@QEBAHXZ"(ptr %0)
  @cxx @implementation
  public func get() -> Int32 { return value }

  // void Counter::add(int d);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN7Counter3addEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} void @"?add@Counter@@QEAAXH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public mutating func add(_ d: Int32) { value += d }

  // int Counter::overloadedByArity() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK7Counter17overloadedByArityEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArity@Counter@@QEBAHXZ"(ptr %0)
  @cxx @implementation
  public func overloadedByArity() -> Int32 { return value }

  // int Counter::overloadedByArity(int x) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK7Counter17overloadedByArityEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?overloadedByArity@Counter@@QEBAHH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public func overloadedByArity(_ x: Int32) -> Int32 { return value + x }

  // int Counter::renamedTarget() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK7Counter13renamedTargetEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?renamedTarget@Counter@@QEBAHXZ"(ptr %0)
  @cxx(renamedTarget) @implementation
  public func swiftRenamed() -> Int32 { return value }
}


// The const and the non-const overload are emitted under their own symbols.

extension Pair {
  // int Pair::adjust(int x) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4Pair6adjustEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?adjust@Pair@@QEBAHH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public func adjust(_ x: Int32) -> Int32 { return value + x }

  // int Pair::adjust(int x);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN4Pair6adjustEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?adjust@Pair@@QEAAHH@Z"(ptr %0, i32 %1)
  @cxx(adjust) @implementation
  public mutating func adjustMutating(_ x: Int32) -> Int32 { value += x; return value }

  // int Pair::adjust(int x, int y);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN4Pair6adjustEii(ptr %0, i32 %1, i32 %2)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?adjust@Pair@@QEAAHHH@Z"(ptr %0, i32 %1, i32 %2)
  @cxx @implementation
  public mutating func adjust(_ x: Int32, _ y: Int32) -> Int32 { value += x + y; return value }
}


// An indirect result follows `this` on the Itanium ABI and precedes it on the
// Microsoft ABI.

extension Holder {
  // Triple Holder::spread(int k) const;
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZNK6Holder6spreadEi(ptr {{[^,]*}}sret{{[^,]*}} %0, ptr %1, i32 %2)
  // CHECK-WIN-LABEL: define{{.*}} @"?spread@Holder@@QEBA?AUTriple@@H@Z"(ptr %0, ptr {{[^,]*}}sret
  @cxx @implementation
  public func spread(_ k: Int32) -> Triple {
    return Triple(a: CLong(value), b: CLong(k), c: CLong(value + k))
  }

  // static Triple Holder::makeTriple(long a);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN6Holder10makeTripleEl(ptr {{[^,]*}}sret{{[^,]*}} %0, i64 %1)
  // CHECK-WIN-LABEL: define{{.*}} @"?makeTriple@Holder@@SA?AUTriple@@J@Z"(ptr {{[^,]*}}sret{{[^,]*}} %0, i32 %1)
  @cxx @implementation
  public static func makeTriple(_ a: CLong) -> Triple { return Triple(a: a, b: a, c: a) }
}


// A non-trivial receiver is passed by pointer.

extension NonTrivialReceiver {
  // int NonTrivialReceiver::read() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK18NonTrivialReceiver4readEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?read@NonTrivialReceiver@@QEBAHXZ"(ptr %0)
  @cxx @implementation
  public func read() -> Int32 { return value }

  // void NonTrivialReceiver::write(int v);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN18NonTrivialReceiver5writeEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} void @"?write@NonTrivialReceiver@@QEAAXH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public mutating func write(_ v: Int32) { value = v }
}


// Swift-side calls

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}11callMethodsys5Int32VSo7CounterVz_So4PairVzSo6HolderVztF"
// CHECK-SYSV:   invoke i32 @_ZN7Counter4makeEi(i32 1)
// CHECK-SYSV:   invoke void @_ZN7Counter3addEi(ptr %0, i32 2)
// CHECK-SYSV:   invoke i32 @_ZNK7Counter3getEv(ptr %0)
// CHECK-SYSV:   invoke i32 @_ZNK7Counter17overloadedByArityEv(ptr %0)
// CHECK-SYSV:   invoke i32 @_ZNK7Counter17overloadedByArityEi(ptr %0, i32 3)
// CHECK-SYSV:   invoke i32 @_ZNK7Counter13renamedTargetEv(ptr %0)
// CHECK-SYSV:   invoke i32 @_ZNK4Pair6adjustEi(ptr %1, i32 4)
// CHECK-SYSV:   invoke i32 @_ZN4Pair6adjustEi(ptr %1, i32 5)
// CHECK-SYSV:   invoke i32 @_ZN4Pair6adjustEii(ptr %1, i32 6, i32 7)
// CHECK-SYSV:   invoke void @_ZNK6Holder6spreadEi(ptr {{[^,]*}}sret{{[^,]*}}, ptr %2, i32 8)
// CHECK-SYSV:   invoke void @_ZN6Holder10makeTripleEl(ptr {{[^,]*}}sret{{[^,]*}}, i64 9)
public func callMethods(_ c: inout Counter, _ p: inout Pair, _ h: inout Holder) -> Int32 {
  var result = Counter.make(1).value
  c.add(2)
  result += c.get() + c.overloadedByArity() + c.overloadedByArity(3) + c.renamedTarget()
  result += p.adjust(4) + p.adjustMutating(5) + p.adjustMutating(6, 7)
  result += Int32(h.spread(8).a) + Int32(Holder.makeTriple(9).b)
  return result
}
