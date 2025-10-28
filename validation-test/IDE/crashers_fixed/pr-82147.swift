// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

// https://github.com/swiftlang/swift/pull/82147

protocol P {
  associatedtype X
}

struct S<T> {
  init<U, V>() where T == (U, V) {}
}
extension S : P where T : P {
  typealias X = T.X
}

func foo<T: P, U>(_: () -> T) where U == T.X {}

foo {
  S(#^COMPLETE^#)
}
