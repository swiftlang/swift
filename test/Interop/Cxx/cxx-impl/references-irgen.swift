// Verifies that a `@cxx @implementation` function implementing a C++ function
// with reference parameters or returns is emitted under the matched
// declaration's mangled symbol, with each reference lowered to a pointer, and
// that Swift-side calls target the same foreign entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import References


// Mutable references

// int addOne(int &x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z6addOneRi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?addOne@@YAHAEAH@Z"(ptr %0)
@unsafe @cxx @implementation
public func addOne(_ x: inout CInt) -> CInt {
  x += 1
  return x
}

// void swapRefs(int &a, int &b);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z8swapRefsRiS_(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} void @"?swapRefs@@YAXAEAH0@Z"(ptr %0, ptr %1)
@unsafe @cxx @implementation
public func swapRefs(_ a: inout CInt, _ b: inout CInt) {
  let t = a
  a = b
  b = t
}


// Const references. The parameter is passed indirectly, as the C++ ABI
// requires; the body loads it from the incoming address.

// int readConstRef(const int &x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z12readConstRefRKi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?readConstRef@@YAHAEBH@Z"(ptr %0)
// CHECK: getelementptr inbounds{{.*}} %Ts5Int32V, ptr %0
@unsafe @cxx @implementation
public func readConstRef(_ x: CInt) -> CInt { return x * 10 }


// Reference returns

let refStorage: UnsafeMutablePointer<CInt> = .allocate(capacity: 1)
let ptrSlot: UnsafeMutablePointer<UnsafeMutablePointer<CInt>> = .allocate(capacity: 1)

// int &mutableRefReturn();
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z16mutableRefReturnv()
// CHECK-WIN-LABEL: define{{.*}} ptr @"?mutableRefReturn@@YAAEAHXZ"()
@cxx @implementation
public func mutableRefReturn() -> UnsafeMutablePointer<CInt> { return refStorage }

// const int &constRefReturn();
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z14constRefReturnv()
// CHECK-WIN-LABEL: define{{.*}} ptr @"?constRefReturn@@YAAEBHXZ"()
@cxx @implementation
public func constRefReturn() -> UnsafePointer<CInt> { return UnsafePointer(refStorage) }

// int *_Nonnull &refToPtrReturn();
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z14refToPtrReturnv()
// CHECK-WIN-LABEL: define{{.*}} ptr @"?refToPtrReturn@@YAAEAPEAHXZ"()
@cxx @implementation
public func refToPtrReturn() -> UnsafeMutablePointer<UnsafeMutablePointer<CInt>> {
  return ptrSlot
}


// A reference to a pointer

// void reseatPtr(int *_Nullable &p);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z9reseatPtrRPi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?reseatPtr@@YAXAEAPEAH@Z"(ptr %0)
@unsafe @cxx @implementation
public func reseatPtr(_ p: inout UnsafeMutablePointer<CInt>?) { p = refStorage }


// Overloads split by reference-ness

// void refOverload(int &x);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z11refOverloadRi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?refOverload@@YAXAEAH@Z"(ptr %0)
@unsafe @cxx @implementation
public func refOverload(_ x: inout CInt) { x += 100 }

// void refOverload(const int &x);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z11refOverloadRKi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?refOverload@@YAXAEBH@Z"(ptr %0)
@unsafe @cxx @implementation
public func refOverload(_ x: CInt) { referencesGlobal = x }

// void refOverload(int *_Nonnull p);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z11refOverloadPi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?refOverload@@YAXPEAH@Z"(ptr %0)
@cxx @implementation
public func refOverload(_ p: UnsafeMutablePointer<CInt>) { p.pointee += 1000 }


// A method taking a reference

// int Accumulator::addTo(int &target) const;
// CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK11Accumulator5addToERi(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?addTo@Accumulator@@QEBAHAEAH@Z"(ptr %0, ptr %1)
extension Accumulator {
  @unsafe @cxx @implementation
  public func addTo(_ target: inout CInt) -> CInt {
    target += total
    return target
  }
}


// References to a struct

// void bumpTotal(Accumulator &acc);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z9bumpTotalR11Accumulator(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?bumpTotal@@YAXAEAUAccumulator@@@Z"(ptr %0)
// CHECK: getelementptr inbounds{{.*}} %TSo11AccumulatorV, ptr %0
@unsafe @cxx @implementation
public func bumpTotal(_ acc: inout Accumulator) { acc.total += 1 }

// int readTotal(const Accumulator &acc);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z9readTotalRK11Accumulator(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?readTotal@@YAHAEBUAccumulator@@@Z"(ptr %0)
// CHECK: getelementptr inbounds{{.*}} %TSo11AccumulatorV, ptr %0
@unsafe @cxx @implementation
public func readTotal(_ acc: Accumulator) -> CInt { return acc.total }


// Swift-side calls go through the imported declarations to the same entry
// points.

// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}15callCxxRefFuncsyyF"
// CHECK-SYSV:   invoke i32 @_Z6addOneRi
// CHECK-SYSV:   invoke void @_Z8swapRefsRiS_
// CHECK-SYSV:   invoke i32 @_Z12readConstRefRKi
// CHECK-SYSV:   invoke ptr @_Z16mutableRefReturnv
// CHECK-SYSV:   invoke ptr @_Z14constRefReturnv
// CHECK-SYSV:   invoke ptr @_Z14refToPtrReturnv
// CHECK-SYSV:   invoke void @_Z9reseatPtrRPi
// CHECK-SYSV:   invoke void @_Z11refOverloadRi
// CHECK-SYSV:   invoke void @_Z11refOverloadRKi
// CHECK-SYSV:   invoke void @_Z11refOverloadPi
// CHECK-SYSV:   invoke i32 @_ZNK11Accumulator5addToERi
// CHECK-SYSV:   invoke void @_Z9bumpTotalR11Accumulator
// CHECK-SYSV:   invoke i32 @_Z9readTotalRK11Accumulator
public func callCxxRefFuncs() {
  var x: CInt = 1
  var y: CInt = 2
  _ = addOne(&x)
  swapRefs(&x, &y)
  _ = readConstRef(x)
  _ = mutableRefReturn()
  _ = constRefReturn()
  _ = refToPtrReturn()

  var p: UnsafeMutablePointer<CInt>? = nil
  reseatPtr(&p)

  refOverload(&x) // `int &`
  refOverload(x) // `const int &`
  refOverload(p!) // `int *`

  _ = Accumulator(total: 5).addTo(&x)

  var acc = Accumulator(total: 5)
  bumpTotal(&acc)
  _ = readTotal(acc)
}
