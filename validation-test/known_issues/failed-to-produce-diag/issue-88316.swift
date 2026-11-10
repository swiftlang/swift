// rdar://174121036 (https://github.com/swiftlang/swift/issues/88316)
// RUN: %target-typecheck-verify-swift

class A {}
class B {}

func test(items: [Int], error: Unmanaged<A>?) {
  let _: [String] = items.compactMap { item in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    guard item == 0 else {
      print("\(error?.takeUnretainedValue() as B)")
      return nil
    }
    return nil
  }
}
