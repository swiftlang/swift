// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift

class C {}
class D : C {}

public func expectEqualType<T>(_: T.Type, _: T.Type) {}

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

func rdar37241221(_ a: C?, _ b: D?) {
  let c: C? = C()
  let array_c_opt = [c]
  let inferred = [a!, b]
  expectEqualType(type(of: array_c_opt).self, type(of: inferred).self)
}
