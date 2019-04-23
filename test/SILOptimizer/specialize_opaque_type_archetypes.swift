// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/specialize_opaque_type_archetypes_2.swift -module-name External -emit-module -emit-module-path %t/External.swiftmodule
// RUN: %target-swift-frontend %S/Inputs/specialize_opaque_type_archetypes_3.swift -enable-library-evolution -module-name External2 -emit-module -emit-module-path %t/External2.swiftmodule
// RUN: %target-swift-frontend -I %t -module-name A -enforce-exclusivity=checked -Osize -emit-sil  -sil-verify-all %s | %FileCheck %s
import External
import External2

public protocol P {
  func myValue() -> Int64
}

extension Int64: P {
  public func myValue() -> Int64 {
    return self
  }
}

@inline(never)
func useP<T: P> (_ t: T) {
  print(t)
}

public func bar(_ x: Int64) -> some P {
  return x
}

public func foo(_ x: Int64) -> some P {
  if x > 0 {
    return bar(x + 1)
  }

  return bar(x - 1)
}

@inline(never)
func getInt() -> Int64 {
  return 2
}

@inline(never)
func identity<T>(_ t: T) -> T {
  return t
}

// CHECK-LABEL: sil @$s1A10testFooBaryyxAA1PRzlF : $@convention(thin) <T where T : P> (@in_guaranteed T) -> () {
// CHECK: bb3([[FOOS_INT:%.*]] : $Builtin.Int64):
// CHECK:  [[ID:%.*]] = function_ref @$s1A8identityyxxlFs5Int64V_Tg5 : $@convention(thin) (Int64) -> Int64
// CHECK:  [[FOO_RES:%.*]] = struct $Int64 ([[FOOS_INT]] : $Builtin.Int64)
// CHECK:  [[ID_RES:%.*]] = apply [[ID]]([[FOO_RES]]) : $@convention(thin) (Int64) -> Int64
// CHECK:  [[USEP:%.*]] = function_ref @$s1A4usePyyxAA1PRzlFs5Int64V_Tg5 : $@convention(thin) (Int64) -> ()
// CHECK:  %27 = apply [[USEP]]([[ID_RES]]) : $@convention(thin) (Int64) -> ()
// CHECK:  %29 = apply [[USEP]]([[FOO_RES]]) : $@convention(thin) (Int64) -> ()

public func testFooBar<T:P>(_ t : T) {
  let x = foo(getInt())
  useP(identity(x))
  useP(x.myValue())
}

struct AddressOnly : P{
  var p : P = Int64(1)

  func myValue() -> Int64 {
    return p.myValue()
  }
}

public func addressOnlyFoo() -> some P {
  return AddressOnly()
}

// CHECK-LABEL: sil @$s1A21testAddressOnlyFoobaryyF
// CHECK-NOT: return type of
// CHECK: return
public func testAddressOnlyFoobar() {
  let x = addressOnlyFoo()
  let y = x
  useP(x.myValue())
  useP(y.myValue())
}

public protocol CP : class {
  func myValue() -> Int64
}

class C : CP {
  func myValue() -> Int64 {
    return 0
  }
}

public func returnC() -> some CP {
  return C()
}

// CHECK-LABEL: sil @$s1A4useCyyF
// CHECK: [[INT:%.*]] = struct $Int64 (
// CHECK:  // function_ref specialized useP<A>(_:)
// CHECK: [[FUN:%.*]] = function_ref @$s1A4usePyyxAA1PRzlFs5Int64V_Tg5
// CHECK:  = apply [[FUN]]([[INT]])
public func useC() {
   let c = returnC()
   useP(c.myValue())
}

// CHECK-LABEL: sil @$s1A11useExternalyyF
// CHECK:  // function_ref Int64.myValue2()
// CHECK:  [[FUN:%.*]] = function_ref @$ss5Int64V8ExternalE8myValue2AByF
// CHECK:  apply [[FUN]]
public func useExternal() {
  let e = external()
  useP(e.myValue2())
}

// This should change once opaque types support resilience.

// CHECK-LABEL: sil @$s1A20useExternalResilientyyF
// CHECK:  // function_ref Int64.myValue3()
// CHECK:  [[FUN:%.*]] = function_ref @$ss5Int64V9External2E8myValue3AByF
// CHECK:  apply [[FUN]]
public func useExternalResilient() {
  let e = externalResilient()
  useP(e.myValue3())
}

struct Container {
  var x : some P {
    get {
      return Int64(1)
    }
  }
}

// CHECK-LABEL: sil @$s1A11usePropertyyyF
// CHECK:  [[VAL:%.*]] = struct $Int64
// CHECK:  // function_ref specialized useP<A>(_:)
// CHECK:  [[FUN:%.*]] = function_ref @$s1A4usePyyxAA1PRzlFs5Int64V_Tg5
// CHECK:  apply [[FUN]]([[VAL]])
public func useProperty() {
   let p = Container().x
   useP(p.myValue())
}

protocol Q {
  associatedtype T
  func f() -> T
  associatedtype T2
  func g() -> T2
}

struct S : Q {
  func f()->some P { return Int64(1) }
  func g()->some CP { return C() }
}

struct Container2 {
  var member : S.T
  mutating func blah(_ x: S.T) { member = x }
}

class Container3 {
  init(member : S.T) {
    self.member = member
  }
  var member : S.T
  func blah(_ x: S.T) { member = x }
}

struct Container4 {
  var member : S.T2
  mutating func blah(_ x: S.T2) { member = x }
}

struct Container5 {
  var member : (S.T2, S.T)
  mutating func blah(_ x: S.T2, _ y: S.T) { member = (x,y) }
}

struct Pair<T, V> {
  var first: T
  var second: V
}
// CHECK-LABEL: sil @$s1A10storedPropyyF
// CHECK-NOT: apply
// CHECK: store
// CHECK-NOT: apply
// CHECK: store
// CHECK-NOT: apply
// CHECK: store
// CHECK-NOT: apply
// CHECK: return
public func storedProp() {
  var c = Container2(member: S().f())
  c.blah(S().f())
  var c2 = Container3(member: S().f())
  c2.blah(S().f())
  var c3 = Container4(member: S().g())
  c3.blah(S().g())
  var s = S()
  var c4 = Container5(member: (s.g(), s.f()))
  c4.blah(s.g(), s.f())
}

public func usePair() {
  var x = Pair(first: bar(1), second: returnC())
  useP(x.first.myValue())
  useP(x.second.myValue())
}
