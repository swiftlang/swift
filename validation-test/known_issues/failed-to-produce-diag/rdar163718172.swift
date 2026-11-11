// rdar://163718172

// RUN: %target-typecheck-verify-swift

struct R<Success> {
  func expected() where Success == () {}
}

func check<T>(_ value: T, getting: (T) -> Bool) -> R<Void> { fatalError() }
func check<T, U>(_ value: T, getting: (T) -> U?) -> R<U> { fatalError() }

protocol BoolShaped {
  var boolValue: Bool { get }
}

func test<each T: BoolShaped>(_ conditions: repeat each T) {
  return check((each conditions).self, getting: { $0.boolValue }).expected()
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}

func testManual<each T: BoolShaped>(_ conditions: repeat each T) {
  repeat check((each conditions).self, getting: { $0.boolValue }).expected()
}
