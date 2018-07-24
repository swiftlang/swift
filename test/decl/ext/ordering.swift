// RUN: %target-typecheck-verify-swift

/// First try different permutations.

struct Outer1 { }

extension Outer1.Inner.Inner { }

extension Outer1.Inner {
  struct Inner { }
}

extension Outer1 {
  struct Inner { }
}

///

struct Outer1A { }

extension Outer1A.Inner {
  struct Inner { }
}

extension Outer1A.Inner.Inner { }

extension Outer1A {
  struct Inner { }
}

///

struct Outer1B { }

extension Outer1B.Inner {
  struct Inner { }
}

extension Outer1B {
  struct Inner { }
}

extension Outer1B.Inner.Inner { }

///

struct Outer1C { }

extension Outer1C {
  struct Inner { }
}

extension Outer1C.Inner {
  struct Inner { }
}

extension Outer1C.Inner.Inner { }

///

struct Outer1D { }

extension Outer1D {
  struct Inner { }
}

extension Outer1D.Inner.Inner { }

extension Outer1D.Inner {
  struct Inner { }
}

/// Now try some generics.

struct Outer2 { }

extension Outer2.Inner.Inner { }

extension Outer2.Inner {
  struct Inner { }
}

extension Outer2 {
  struct Inner<T> { }
}

///

struct Outer3<T> { }

extension Outer3.Inner { }

extension Outer3 {
  struct Inner<U> { }
}

/// Extending a typealias inside an extension.

struct Outer4 { }

extension Outer4.Inner { }

struct Outer4Inner { }

extension Outer4 {
  typealias Inner = Outer4Inner
}

/// Subclassing.

enum Outer5 {}

enum OtherOuter5 {}

extension Outer5.Inner {}

extension Outer5 {
  class Inner : OtherOuter5.Super {}
}

extension OtherOuter5 {
  class Super {}
}

///

enum Outer5A {}

enum OtherOuter5A {}

extension Outer5A {
  class Inner : OtherOuter5A.Super {}
  // expected-error@-1 {{'Super' is not a member type of 'OtherOuter5A'}}
  // FIXME
}

extension Outer5A.Inner {}

extension OtherOuter5A {
  class Super {}
}

///

enum Outer5B {}

enum OtherOuter5B {}

extension Outer5B {
  class Inner : OtherOuter5B.Super {}
}

extension OtherOuter5B {
  class Super {}
}

extension Outer5B.Inner {}

///

enum Outer5C {}

enum OtherOuter5C {}

extension OtherOuter5C {
  class Super {}
}

extension Outer5C {
  class Inner : OtherOuter5C.Super {}
}

extension Outer5C.Inner {}

///

enum Outer5D {}

enum OtherOuter5D {}

extension OtherOuter5D {
  class Super {}
}

extension Outer5D.Inner {}

extension Outer5D {
  class Inner : OtherOuter5D.Super {}
}
