// RUN: %target-swift-frontend -module-name A -enforce-exclusivity=checked -Osize -emit-sil  %s | %FileCheck %s
public protocol P {
  func myValue() -> Int
}

extension Int: P {
  public func myValue() -> Int {
    return self
  }
}

@inline(never)
func useP<T: P> (_ t: T) {
  print(t)
}

public func bar(_ x: Int) -> some P {
  return x
}

public func foo(_ x: Int) -> some P {
  if x > 0 {
    return bar(x + 1)
  }

  return bar(x - 1)
}

@inline(never)
func getInt() -> Int {
  return 2
}

@inline(never)
func identity<T>(_ t: T) -> T {
  return t
}

// CHECK-LABEL: sil @$s1A10testFooBaryyxAA1PRzlF : $@convention(thin) <T where T : P> (@in_guaranteed T) -> () {
// CHECK: bb3([[FOOS_INT:%.*]] : $Builtin.Int64):
// CHECK:  [[ID:%.*]] = function_ref @$s1A8identityyxxlFSi_Tg5 : $@convention(thin) (Int) -> Int
// CHECK:  [[FOO_RES:%.*]] = struct $Int ([[FOOS_INT]] : $Builtin.Int64)
// CHECK:  [[ID_RES:%.*]] = apply [[ID]]([[FOO_RES]]) : $@convention(thin) (Int) -> Int
// CHECK:  [[USEP:%.*]] = function_ref @$s1A4usePyyxAA1PRzlFSi_Tg5 : $@convention(thin) (Int) -> ()
// CHECK:  %27 = apply [[USEP]]([[ID_RES]]) : $@convention(thin) (Int) -> ()
// CHECK:  %29 = apply [[USEP]]([[FOO_RES]]) : $@convention(thin) (Int) -> ()

public func testFooBar<T:P>(_ t : T) {
  let x = foo(getInt())
  useP(identity(x))
  useP(x.myValue())
}

struct AddressOnly : P{
  var p : P = 1

  func myValue() -> Int {
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
