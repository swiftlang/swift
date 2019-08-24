// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift

class C {}
class D : C {}

protocol L {}
protocol M : L {}
protocol N : L {}
protocol P : M {}
protocol Q : M {}
protocol R : L {}
protocol Y {}

protocol FakeEquatable {}
protocol FakeHashable : FakeEquatable {}
protocol FakeExpressibleByIntegerLiteral {}
protocol FakeNumeric : FakeEquatable, FakeExpressibleByIntegerLiteral {}
protocol FakeSignedNumeric : FakeNumeric {}
protocol FakeComparable : FakeEquatable {}
protocol FakeStrideable : FakeComparable {}
protocol FakeCustomStringConvertible {}
protocol FakeBinaryInteger : FakeHashable, FakeNumeric, FakeCustomStringConvertible, FakeStrideable {}
protocol FakeLosslessStringConvertible {}
protocol FakeFixedWidthInteger : FakeBinaryInteger, FakeLosslessStringConvertible {}
protocol FakeUnsignedInteger : FakeBinaryInteger {}
protocol FakeSignedInteger : FakeBinaryInteger, FakeSignedNumeric {}
protocol FakeFloatingPoint : FakeSignedNumeric, FakeStrideable, FakeHashable {}
protocol FakeExpressibleByFloatLiteral {}
protocol FakeBinaryFloatingPoint : FakeFloatingPoint, FakeExpressibleByFloatLiteral {}

func expectEqualType<T>(_: T.Type, _: T.Type) {}
func commonSupertype<T>(_: T, _: T) -> T {}

expectEqualType(Builtin.type_join(Int.self, Int.self), Int.self)
expectEqualType(Builtin.type_join_meta(D.self, C.self), C.self)

expectEqualType(Builtin.type_join(Int?.self, Int?.self), Int?.self)
expectEqualType(Builtin.type_join(Int.self, Int?.self), Int?.self)
expectEqualType(Builtin.type_join(Int?.self, Int.self), Int?.self)
expectEqualType(Builtin.type_join(Int.self, Int??.self), Int??.self)
expectEqualType(Builtin.type_join(Int??.self, Int.self), Int??.self)
expectEqualType(Builtin.type_join(Int?.self, Int??.self), Int??.self)
expectEqualType(Builtin.type_join(Int??.self, Int?.self), Int??.self)
expectEqualType(Builtin.type_join(D?.self, D?.self), D?.self)
expectEqualType(Builtin.type_join(C?.self, D?.self), C?.self)
expectEqualType(Builtin.type_join(D?.self, C?.self), C?.self)
expectEqualType(Builtin.type_join(D.self, D?.self), D?.self)
expectEqualType(Builtin.type_join(D?.self, D.self), D?.self)
expectEqualType(Builtin.type_join(C.self, D?.self), C?.self)
expectEqualType(Builtin.type_join(D?.self, C.self), C?.self)
expectEqualType(Builtin.type_join(D.self, C?.self), C?.self)
expectEqualType(Builtin.type_join(C?.self, D.self), C?.self)
expectEqualType(Builtin.type_join(Any?.self, D.self), Any?.self)
expectEqualType(Builtin.type_join(D.self, Any?.self), Any?.self)
expectEqualType(Builtin.type_join(Any.self, D?.self), Any?.self)
expectEqualType(Builtin.type_join(D?.self, Any.self), Any?.self)
expectEqualType(Builtin.type_join(Any?.self, Any.self), Any?.self)
expectEqualType(Builtin.type_join(Any.self, Any?.self), Any?.self)

expectEqualType(Builtin.type_join(Builtin.Int1.self, Builtin.Int1.self), Builtin.Int1.self)
expectEqualType(Builtin.type_join(Builtin.Int32.self, Builtin.Int1.self), Any.self)
expectEqualType(Builtin.type_join(Builtin.Int1.self, Builtin.Int32.self), Any.self)

expectEqualType(Builtin.type_join(L.self, L.self), L.self)
expectEqualType(Builtin.type_join(L.self, M.self), L.self)
expectEqualType(Builtin.type_join(L.self, P.self), L.self)
expectEqualType(Builtin.type_join(L.self, Y.self), Any.self)
expectEqualType(Builtin.type_join(N.self, P.self), L.self)
expectEqualType(Builtin.type_join(Q.self, P.self), M.self)
expectEqualType(Builtin.type_join((N & P).self, (Q & R).self), M.self)
expectEqualType(Builtin.type_join((Q & P).self, (Y & R).self), L.self)
expectEqualType(Builtin.type_join(FakeEquatable.self, FakeEquatable.self), FakeEquatable.self)
expectEqualType(Builtin.type_join(FakeHashable.self, FakeEquatable.self), FakeEquatable.self)
expectEqualType(Builtin.type_join(FakeEquatable.self, FakeHashable.self), FakeEquatable.self)
expectEqualType(Builtin.type_join(FakeNumeric.self, FakeHashable.self), FakeEquatable.self)
expectEqualType(Builtin.type_join((FakeHashable & FakeStrideable).self, (FakeHashable & FakeNumeric).self),
                                  FakeHashable.self)
expectEqualType(Builtin.type_join((FakeNumeric & FakeStrideable).self,
                                  (FakeHashable & FakeNumeric).self), FakeNumeric.self)
expectEqualType(Builtin.type_join(FakeBinaryInteger.self, FakeFloatingPoint.self),
                                  (FakeHashable & FakeNumeric & FakeStrideable).self)
expectEqualType(Builtin.type_join(FakeFloatingPoint.self, FakeBinaryInteger.self),
                                  (FakeHashable & FakeNumeric & FakeStrideable).self)

func joinFunctions(
  _ escaping: @escaping () -> (),
  _ nonescaping: () -> ()
) {
  _ = commonSupertype(escaping, escaping)
  _ = commonSupertype(nonescaping, escaping)
  // expected-error@-1 {{converting non-escaping value to 'T' may allow it to escape}}
  _ = commonSupertype(escaping, nonescaping)
  // expected-error@-1 {{converting non-escaping value to 'T' may allow it to escape}}
  let x: Int = 1
  // FIXME: We emit these diagnostics here because we refuse to allow
  //        Any to be inferred for the generic type. That's pretty
  //        arbitrary.
  _ = commonSupertype(escaping, x)
  // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '() -> ()'}}
  _ = commonSupertype(x, escaping)
  // expected-error@-1 {{cannot convert value of type '() -> ()' to expected argument type 'Int'}}

  let a: Any = 1
  _ = commonSupertype(nonescaping, a)
  // expected-error@-1 {{converting non-escaping value to 'Any' may allow it to escape}}
  _ = commonSupertype(a, nonescaping)
  // expected-error@-1 {{converting non-escaping value to 'Any' may allow it to escape}}
  _ = commonSupertype(escaping, a)
  _ = commonSupertype(a, escaping)

  expectEqualType(Builtin.type_join(((C) -> C).self, ((C) -> D).self),
    ((C) -> C).self)
}

func rdar37241221(_ a: C?, _ b: D?) {
  let c: C? = C()
  let array_c_opt = [c]
  let inferred = [a!, b]
  expectEqualType(type(of: array_c_opt).self, type(of: inferred).self)
}
