// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift

class C {}
class D : C {}

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
