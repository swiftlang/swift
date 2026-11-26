// RUN: %target-typecheck-verify-swift

// An implicit member expression like `.foo` can refer to a dynamic member
// through a static `subscript(dynamicMember:)`, the same as `T.foo`.

struct Color {
  let red: Double
  let green: Double
}

func takesProp(_: Prop) {}

// https://github.com/swiftlang/swift/issues/60574
@dynamicMemberLookup
struct Prop {
  static subscript<T>(dynamicMember member: KeyPath<Color, T>) -> Self {
    Prop()
  }

  var identity: Prop { self }
}

func testKeyPath() {
  _ = Prop.red
  let _: Prop = .red
  takesProp(.green)
  let _: Prop = .red.identity
  let _: [Prop] = [.red, .green]

  let _: Prop = .blue // expected-error {{has no dynamic member 'blue' using key path from root type 'Color'}}
}

// https://github.com/swiftlang/swift/issues/80615
struct Foo {
  var bar: Bar {
    fatalError()
  }
}

@dynamicMemberLookup
struct Bar {
  static subscript(dynamicMember keyPath: KeyPath<Foo, Bar>) -> Self {
    fatalError()
  }
}

func testKeyPathToSelf() {
  _ = Bar.bar
  let _: Bar = .bar
}

// https://github.com/swiftlang/swift/issues/84808
@dynamicMemberLookup
struct S {
  static subscript(dynamicMember _: String) -> Self {
    Self()
  }
}

func takesS(_: S) {}

func testString() {
  takesS(S.abc)
  takesS(.abc)
  let _: S = .anything
}

// A real static member is preferred over a dynamic member.
@dynamicMemberLookup
struct WithStatic {
  static var abc: WithStatic { WithStatic() }
  static subscript(dynamicMember _: String) -> Int { 0 }
}

func testRealMemberWins() {
  let _: WithStatic = .abc
}
