// RUN: %target-swift-frontend -emit-module-path /dev/null %s

// https://github.com/apple/swift/issues/52656

func foo<T>(_: (T) -> ()) -> T { fatalError() }

let y = foo { (x: @escaping () -> (), y: Int) in }
