// RUN: not %target-swift-frontend %s -typecheck

class C {}
class D : C {}

@_silgen_name("consume")
func consume(_: [C])  // note, returns ()

// Assert/crash while emitting diagnostic for coercion from () to Bool
// in the context of a collection cast.
func test(x: [D]) -> Bool {
  return consume(x)  // no way to coerce from () to Bool
}
