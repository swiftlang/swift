@resultBuilder
struct Builder {
  static func buildBlock<T>(_ x: T) -> T { x }
  static func buildExpression<T>(_ x: T) -> T { x }
}

// https://github.com/swiftlang/swift/issues/79696
// Make sure we don't pick up the builder as the container type.
struct A {
  struct B {
    @Builder
    var foo: Any {
      B
      // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):7 %s -- %s | %FileCheck %s --check-prefix NESTED_TY
      // NESTED_TY-NOT: Container
    }
    @Builder var bar: Any {
      foo
      // RUN: %sourcekitd-test -req=cursor -pos=%(line-1):7 %s -- %s | %FileCheck %s --check-prefix PROP
      // PROP: <Container>$s4main1AV1BVD</Container>
    }
  }
}
