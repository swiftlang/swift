// RUN: %target-typecheck-verify-swift -parse-stdlib

import Swift

class C {}
class D : C {}

public func expectEqualType<T>(_: T.Type, _: T.Type) {}

expectEqualType(Builtin.type_join(Int.self, Int.self), Int.self)
expectEqualType(Builtin.type_join_meta(D.self, C.self), C.self)

func rdar37241221(_ a: C?, _ b: D?) {
  let _ = [a!, b] // Should be inferred as `[C?]`
}
