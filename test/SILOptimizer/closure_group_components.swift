// RUN: %target-swift-frontend -emit-sil %s -parse-as-library -module-name K -o /dev/null -Xllvm -sil-print-closure-groups 2>&1 | %FileCheck %s --check-prefix=GROUPS

// A closure component holds a function and every closure or local function it
// references, whatever they capture (ClosureGroupSchedule.h). Functions that
// aren't closures, such as accessors and initializers, stay apart.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K10defaultArgyS2iyXEF{{$}}
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K1SV1pSivg{{$}}

func run(_ body: () -> ()) { body() }
func auto(_ c: @autoclosure () -> Bool) -> Bool { c() }
var saved: (() -> Int)?

// A closure that captures a let, by value.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K7byValueyyF $s1K7byValueyyFyyXEfU_{{$}}
public func byValue() { let k = 3; run { _ = k } }

// A local function that captures a var, applied directly.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K8localVarSiyF $s1K8localVarSiyF4bumpL_yyF{{$}}
public func localVar() -> Int {
  var v = 0
  func bump() { v += 1 }
  bump()
  return v
}

// A local function that captures a let.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K8localLetSiyF $s1K8localLetSiyF3getL_SiyF{{$}}
public func localLet() -> Int {
  let k = 3
  func get() -> Int { k }
  return get()
}

// A local function and a closure that capture nothing.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K14localNoCaptureSiyF $s1K14localNoCaptureSiyF3oneL_SiyF{{$}}
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K16closureNoCaptureyyF $s1K16closureNoCaptureyyFyyXEfU_{{$}}
public func localNoCapture() -> Int {
  func one() -> Int { 1 }
  return one()
}
public func closureNoCapture() { run { print("hi") } }

// An autoclosure that captures a var.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K11autoclosureSbyF $s1K11autoclosureSbyFSbyXEfu_{{$}}
public func autoclosure() -> Bool {
  var v = 0
  v += 1
  return auto(v > 0)
}

// A weak capture.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K11weakCaptureyyAA1CCF $s1K11weakCaptureyyAA1CCFSiycfU_{{$}}
public final class C {}
public func weakCapture(_ c: C) { saved = { [weak c] in c == nil ? 0 : 1 } }

// A closure that captures self.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K1MC6methodyyF $s1K1MC6methodyyFyyXEfU_{{$}}
public final class M {
  var n = 0
  public func method() { run { self.n += 1 } }
}

// Closures in a global's initializer, a property's initializer, and a default
// argument.
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K6global_WZ $s1K6globalSivpfiSiyXEfU_{{$}}
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K1SV1pSivpfi $s1K1SV1pSivpfiSiyXEfU_{{$}}
// GROUPS-DAG: Closure group for diagnose-static-exclusivity: $s1K10defaultArgyS2iyXEFfA_ $s1K10defaultArgyS2iyXEFfA_SiycfU_{{$}}
public let global: Int = { 5 }()
public struct S { public var p: Int = { 7 }() }
public func defaultArg(_ f: () -> Int = { 1 }) -> Int { f() }
