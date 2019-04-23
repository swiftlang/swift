// RUN: %target-swift-frontend -module-name A -enforce-exclusivity=checked -Osize -emit-sil -Xllvm -enable-opaque-archetype-specializer %s | %FileCheck %s

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
