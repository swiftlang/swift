// https://github.com/swiftlang/swift/issues/80985
struct S<T> {
  func foo<U>(_ fn: (T) -> U) -> S<U> { fatalError() }
}

func foo(xs: S<(Int, Int)>) {
  _ = {
    let y = xs
      .foo{ $1 }
      .foo{ $0 }
    // RUN: %sourcekitd-test -req=complete -pos=%(line-1):11 %s -- %s
  }
}
