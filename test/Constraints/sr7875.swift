// RUN: %target-typecheck-verify-swift

protocol Proto {}
class Base {}
class Test : Base, Proto {}

struct A {}
struct B {}

func overloaded<T: Proto & Base>(_ f: () -> T, _ g: (T, A) -> ()) {}
func overloaded<T: Proto & Base>(_ f: () -> T, _ g: (T, B) -> ()) {}

func f() -> Test { return Test() }

func g<T: Proto & Base>(_ t: T, _ a: A) -> () {}

func test() {
  overloaded(f, g as (Test, A) -> ())
  overloaded(f, g)
}
