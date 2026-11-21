// rdar://185893898 (https://github.com/swiftlang/swift/issues/91727)
// RUN: %target-typecheck-verify-swift

struct S {
  func foo<T>(_ fn: (Int) -> T) -> T {}
  func bar() {
    let y: Bool = foo { x in }
    // expected-error@-1 {{cannot convert value of type '()' to specified type 'Bool'}}
  }
}
