// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -solver-disable-shrink -disable-constraint-solver-performance-hacks -solver-enable-operator-designated-types
// REQUIRES: tools-release,no_asserts

infix operator <*> : AdditionPrecedence
func <*><A, B>(lhs: ((A) -> B)?, rhs: A?) -> B? {
  if let lhs1 = lhs, let rhs1 = rhs {
    return lhs1(rhs1)
  }
  return nil
}

func cons<T, U>(lhs: T) -> (U) -> (T, U) {
   return { rhs in (lhs, rhs) }
}

var str: String? = "x"
if let f = cons <*> str <*> (cons <*> str <*> (cons <*> str <*> (cons <*> str <*> (cons <*> str <*> (cons <*> str <*> (cons <*> str <*> str)))))) {
   print("\(f)")
}
