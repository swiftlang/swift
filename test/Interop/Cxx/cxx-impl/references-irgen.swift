// Verifies that a `@cxx @implementation` function implementing a C++
// function with reference parameters or returns is emitted under the matched
// declaration's mangled symbol, with each reference spelled and lowered as a
// pointer, and that Swift-side calls target the same foreign entry points.

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
@cxx @implementation
public func addOne(_ x: UnsafeMutablePointer<CInt>) -> CInt {
  x.pointee += 1
  return x.pointee
}

// void swapRefs(int &a, int &b);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z8swapRefsRiS_(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} void @"?swapRefs@@YAXAEAH0@Z"(ptr %0, ptr %1)
@cxx @implementation
public func swapRefs(_ a: UnsafeMutablePointer<CInt>, _ b: UnsafeMutablePointer<CInt>) {
  let t = a.pointee
  a.pointee = b.pointee
  b.pointee = t
}

// int observe(int &a, int &b);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z7observeRiS_(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?observe@@YAHAEAH0@Z"(ptr %0, ptr %1)
@cxx @implementation
public func observe(_ a: UnsafeMutablePointer<CInt>, _ b: UnsafeMutablePointer<CInt>) -> CInt {
  a.pointee = 1
  b.pointee = 2
  return a.pointee
}

// int observeGlobal(int &x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z13observeGlobalRi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?observeGlobal@@YAHAEAH@Z"(ptr %0)
@cxx @implementation
public func observeGlobal(_ x: UnsafeMutablePointer<CInt>) -> CInt {
  referencesGlobal = 5
  x.pointee = 7
  return referencesGlobal
}


// Const references

// int readConstRef(const int &x);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z12readConstRefRKi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?readConstRef@@YAHAEBH@Z"(ptr %0)
// CHECK: getelementptr inbounds{{.*}} %Ts5Int32V, ptr %0
@cxx @implementation
public func readConstRef(_ x: UnsafePointer<CInt>) -> CInt { return x.pointee * 10 }


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
@cxx @implementation
public func reseatPtr(_ p: UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>) {
  p.pointee = refStorage
}


// Overloads split by reference-ness

// void refOverload(int &x);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z11refOverloadRi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?refOverload@@YAXAEAH@Z"(ptr %0)
@cxx @implementation
public func refOverload(_ x: UnsafeMutablePointer<CInt>) { x.pointee += 100 }

// void refOverload(const int &x);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z11refOverloadRKi(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?refOverload@@YAXAEBH@Z"(ptr %0)
@cxx @implementation
public func refOverload(_ x: UnsafePointer<CInt>) { referencesGlobal = x.pointee }


// A method taking a reference

// int Accumulator::addTo(int &target) const;
// CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK11Accumulator5addToERi(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?addTo@Accumulator@@QEBAHAEAH@Z"(ptr %0, ptr %1)
extension Accumulator {
  @cxx @implementation
  public func addTo(_ target: UnsafeMutablePointer<CInt>) -> CInt {
    target.pointee += total
    return target.pointee
  }
}


// References to a struct. The field access lands on the incoming pointer
// itself: the reference is passed as one level of indirection.

// void bumpTotal(Accumulator &acc);
// CHECK-SYSV-LABEL: define{{.*}} void @_Z9bumpTotalR11Accumulator(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} void @"?bumpTotal@@YAXAEAUAccumulator@@@Z"(ptr %0)
// CHECK: getelementptr inbounds{{.*}} %TSo11AccumulatorV, ptr %0
@cxx @implementation
public func bumpTotal(_ acc: UnsafeMutablePointer<Accumulator>) { acc.pointee.total += 1 }

// int readTotal(const Accumulator &acc);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_Z9readTotalRK11Accumulator(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} i32 @"?readTotal@@YAHAEBUAccumulator@@@Z"(ptr %0)
// CHECK: getelementptr inbounds{{.*}} %TSo11AccumulatorV, ptr %0
@cxx @implementation
public func readTotal(_ acc: UnsafePointer<Accumulator>) -> CInt { return acc.pointee.total }

// Accumulator &identityRef(Accumulator &acc);
// CHECK-SYSV-LABEL: define{{.*}} ptr @_Z11identityRefR11Accumulator(ptr %0)
// CHECK-WIN-LABEL: define{{.*}} ptr @"?identityRef@@YAAEAUAccumulator@@AEAU1@@Z"(ptr %0)
@cxx @implementation
public func identityRef(_ acc: UnsafeMutablePointer<Accumulator>) -> UnsafeMutablePointer<Accumulator> {
  return acc
}


// Swift-side calls go through the imported declarations (which keep the
// importer's `inout` and by-value projections) to the same entry points.

// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}15callCxxRefFuncsyyF"
// CHECK-SYSV:   invoke i32 @_Z6addOneRi
// CHECK-SYSV:   invoke void @_Z8swapRefsRiS_
// CHECK-SYSV:   invoke i32 @_Z7observeRiS_
// CHECK-SYSV:   invoke i32 @_Z13observeGlobalRi
// CHECK-SYSV:   invoke i32 @_Z12readConstRefRKi
// CHECK-SYSV:   invoke ptr @_Z16mutableRefReturnv
// CHECK-SYSV:   invoke ptr @_Z14constRefReturnv
// CHECK-SYSV:   invoke ptr @_Z14refToPtrReturnv
// CHECK-SYSV:   invoke void @_Z9reseatPtrRPi
// CHECK-SYSV:   invoke void @_Z11refOverloadRi
// CHECK-SYSV:   invoke void @_Z11refOverloadRKi
// CHECK-SYSV:   invoke i32 @_ZNK11Accumulator5addToERi
// CHECK-SYSV:   invoke void @_Z9bumpTotalR11Accumulator
// CHECK-SYSV:   invoke ptr @_Z11identityRefR11Accumulator
// CHECK-SYSV:   invoke i32 @_Z9readTotalRK11Accumulator
public func callCxxRefFuncs() {
  var x: CInt = 1
  var y: CInt = 2
  _ = addOne(&x)
  swapRefs(&x, &y)
  _ = observe(&x, &y)
  _ = observeGlobal(&x)
  _ = readConstRef(x)
  _ = mutableRefReturn()
  _ = constRefReturn()
  _ = refToPtrReturn()

  var p: UnsafeMutablePointer<CInt>? = nil
  reseatPtr(&p)

  refOverload(&x) // `int &`
  refOverload(x) // `const int &`

  _ = Accumulator(total: 5).addTo(&x)

  var acc = Accumulator(total: 5)
  bumpTotal(&acc)
  _ = identityRef(&acc)
  _ = readTotal(acc)
}


// A Swift-side call to an imported operator whose const reference is to a
// pointer passes the pointer's address, as the C++ ABI requires.

// CHECK-LABEL: define{{.*}} swiftcc i1 @"$s{{.*}}19callPointerOperator{{.*}}"
// CHECK-SYSV: [[SLOT:%[0-9]+]] = alloca %TSp
// CHECK-SYSV: invoke zeroext i1 @_ZeqRK13PointerHolderRKPi(ptr %{{[0-9]+}}, ptr [[SLOT]])
public func callPointerOperator(_ h: PointerHolder, _ p: UnsafeMutablePointer<CInt>) -> Bool {
  return h == p
}
