// RUN: %target-typecheck-verify-swift

struct A { }
struct B { }

@_disfavoredOverload
func f0<T>(_: T) -> A { return A() }

func f0(_: Int32) -> B { return B() }


func f1(_: StaticString) -> B { return B() }

@_disfavoredOverload
func f1<T>(_: T) -> A { return A() }

func f2(_: Substring) -> B { return B() }

@_disfavoredOverload
func f2<T>(_: T) -> A { return A() }

func test(s: String, answer: Int) {
  let r0a = f0(17)
  let _: B = r0a
  let r0b = f0(answer)
  let _: A = r0b

  let r1 = f1("hello")
  let _: B = r1
  
  let r2a = f2("hello")
  let _: B = r2a
  let r2b = f2("the answer is \(answer)")
  let _: B = r2b
  let r2c = f2(s)
  let _: A = r2c
}

do {
  @available(*, deprecated)
  @_disfavoredOverload
  func test(v: Int) {}

  func test(v: Any) {}

  func call(v: Int) {
    test(v: v) // Ok (the overload that takes `Int` is disfavored)
  }
}
