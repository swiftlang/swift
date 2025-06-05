// RUN: %target-typecheck-verify-swift

extension Array<Int> {
  func someIntFuncOnArray() {}
}

let _ = [0, 1, 2].someIntFuncOnArray()

extension [Character] {
  func makeString() -> String { fatalError() }
}

let _ = ["a", "b", "c"].makeString()
let _ = [1, 2, 3].makeString() // expected-error 3 {{cannot convert value of type 'Int' to expected element type 'Character'}}

extension Set<_> {} // expected-error {{cannot extend a type that contains placeholders}}

// https://github.com/apple/swift/issues/47452

struct Foo<T, U> {
  var x: T
  var y: U
}

typealias IntFoo<U> = Foo<Int, U>

extension IntFoo where U == Int {
  func hello() {
    print("hello")
  }
}

Foo(x: "test", y: 1).hello()


struct Field<Tag,Value> {
  let tag: Tag
  let value: Value
}

typealias IntField<Tag> = Field<Tag,Int>

extension IntField {
  func adding(_ value: Int) -> Self {
    Field(tag: tag, value: self.value + value)
  }
}

struct S10<X, Y> {}
typealias InferredSpecializedNestedTypes<X> = S10<X, (Int, [Int]?)>
extension InferredSpecializedNestedTypes {
    func returnTuple(value: Y) -> (Int, [Int]?) {
        return value
    }
}


struct S2<X,Y,Z> {
  let x: X
  let y: Y
  let z: Z
}

typealias A2<Y,X> = S2<Int,X,Y>

extension A2 {
  func adding(_ x: Int) -> Self {
    S2(x: self.x + x, y: y, z: z) // expected-error {{binary operator '+' cannot be applied to operands of type 'X' and 'Int'}} expected-note {{overloads for '+' exist with these partially matching parameter lists: (Int, Int)}}
  }
}


struct S4<A, B> {
  // Generic parameters: <A, B, C>
  // Depth:               0  0  1
  struct Nested<C> {
      let c: C
  }
}

struct S5<A> {
  // Generic parameters: <A, B, C>
  // Depth:               0  1  1
  typealias Alias<B, C> = S4<A, B>.Nested<C>

}

extension S5.Alias{
  func adding(_ c: Int) -> Self {
        S4.Nested(c: self.c + c) //expected-error {{binary operator '+' cannot be applied to operands of type 'C' and 'Int'}} expected-note {{overloads for '+' exist with these partially matching parameter lists: (Int, Int)}}
    }
}

struct S11<T> {
  struct Inner<U> {}
}

struct S12<T> {
  struct Inner<U> {}
  typealias A1<U> = S11<T>.Inner<U>
  typealias A2<U> = S12<T>.Inner<U> where T == Int
}

extension S12<Int>.A1 {
  func foo1() {
    let int: Int
    let _: T = int // OK
  }
}

extension S12.A2 {
  func foo2() {
    let int: Int
    let _: T = int // OK
  }
}

struct MyType<TyA, TyB> {
  var a : TyA, b : TyB
}

typealias A<T1, T2> = MyType<T2, T1>

extension A {}

extension A<Float, Int> {}
extension A<Void, Void> {}

struct Tree<T> {
  struct Branch<B> {
    struct Nest<N> {
      struct Egg {}
    }
  }
}

extension Tree.Branch.Nest.Egg { static func tweet() {} }
extension Tree<Int>.Branch.Nest.Egg { static func twoot() {} }
extension Tree<Int>.Branch<String>.Nest.Egg { static func twote() {} }
extension Tree<Int>.Branch<String>.Nest<Void>.Egg { static func twite() {} }

func testNestedExtensions() {
  do {
    Tree<Void>.Branch<Void>.Nest<Void>.Egg.tweet()
  }

  do {
    Tree<Int>.Branch<Void>.Nest<Void>.Egg.twoot()
    Tree<Int>.Branch<Int>.Nest<Void>.Egg.twoot()
    Tree<Int>.Branch<Int>.Nest<Int>.Egg.twoot()
  }

  do {
    Tree<Int>.Branch<String>.Nest<Void>.Egg.twote()
    Tree<Int>.Branch<String>.Nest<Float>.Egg.twote()
  }

  Tree<Int>.Branch<String>.Nest<Void>.Egg.twite()
}

// rdar://111059036 - failed to produce a diagnostic in specialized extension
struct Test {
  struct Key<Value> {}
}

class State {
}

extension Test.Key<State> {
  static let state = Self<State>()
}

protocol P {}
struct S: P {}

extension Sequence<any P> where Self == [S] {}
// expected-error@-1 {{generic signature requires types 'S' and 'any P' to be the same}}
