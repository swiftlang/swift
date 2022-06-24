// RUN: %target-swift-frontend -experimental-performance-annotations -emit-sil %s -o /dev/null -verify
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

protocol P {
  func protoMethod(_ a: Int) -> Int
}

open class Cl {
  open func classMethod() {}
  final func finalMethod() {}
}

func initFunc() -> Int { return 3 }

struct Str : P {
  let x: Int

  func protoMethod(_ a: Int) -> Int {
    return a + x
  }

  static let s = 27
  static var s2 = 10 + s
  static var s3 = initFunc() // expected-error {{global/static variable initialization can cause locking}}
}

struct AllocatingStr : P {
  func protoMethod(_ a: Int) -> Int {
    _ = Cl()                // expected-error {{Using type 'Cl' can cause metadata allocation or locks}} 
    return 0
  }
}

/* Currently disabled: rdar://90495704

func noRTCallsForArrayGet(_ a: [Str], _ i: Int) -> Int {
  return a[i].x
}

@_noLocks
func callArrayGet(_ a: [Str]) -> Int {
  return noRTCallsForArrayGet(a, 0)
}
*/

@_noLocks
func arcOperations(_ x: Cl) -> Cl {
  return x                // expected-error {{this code performs reference counting operations which can cause locking}} 
}

func genFunc<T: P>(_ t: T, _ a: Int) -> Int {
  let s = t
  return t.protoMethod(a) + s.protoMethod(a) // expected-note {{called from here}}
}

@_noAllocation
func callMethodGood(_ a: Int) -> Int {
  return genFunc(Str(x: 1), a)
}

@_noAllocation
func callMethodBad(_ a: Int) -> Int {
  return genFunc(AllocatingStr(), a) // expected-note {{called from here}}
}

@_noAllocation
func callClassMethod(_ c: Cl) {
  return c.classMethod() // expected-error {{called function is not known at compile time and can have unpredictable performance}}
}

@_noAllocation
func callFinalMethod(_ c: Cl) {
  return c.finalMethod()
}

@_noAllocation
func callProtocolMethod(_ p: P) -> Int {
  return p.protoMethod(0) // expected-error {{this code pattern can cause metadata allocation or locks}}
}

@_noAllocation
func dynamicCast(_ a: AnyObject) -> Cl? {
  return a as? Cl // expected-error {{dynamic casting can lock or allocate}}
}

@_noAllocation
func testUnsafePerformance(_ idx: Int) -> [Int] {
  return _unsafePerformance { [10, 20, 30, 40] }
}

@_noAllocation
func testMemoryLayout() -> Int {
  return MemoryLayout<Int>.size + MemoryLayout<Int>.stride + MemoryLayout<Int>.alignment
}

class MyError : Error {}

@_noLocks
func noDiagnosticsInThrowPath(_ b: Bool) throws -> Int {
  if b {
    return 28
  }
  throw MyError()
}

@_noLocks
func noDiagnosticsInCatch(_ b: Bool) throws -> Int? {
  do {
    return try noDiagnosticsInThrowPath(true)
  } catch let e as MyError {
    print(e)
    return nil
  }
}

@_noLocks
func testRecursion(_ i: Int) -> Int {
  if i > 0 {
    return testRecursion(i - 1)
  }
  return 0
}

@_noLocks
func testGlobal() -> Int {
  return Str.s + Str.s2
}

@_noLocks
func testGlobalWithComplexInit() -> Int {
  return Str.s3 // expected-note {{called from here}}
}

func metatypeArg<T>(_ t: T.Type, _ b: Bool) { // expected-error {{Using type 'Int' can cause metadata allocation or locks}}
}

@_noAllocation
func callFuncWithMetatypeArg() {
  metatypeArg(Int.self, false)                // expected-note {{called from here}}
}

