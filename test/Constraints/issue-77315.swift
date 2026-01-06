// RUN: %target-typecheck-verify-swift

func foo<T>(_: () -> (Optional<T>, Int)) -> T { fatalError() }

let x: Int = foo { () -> (Optional, Int) in fatalError() }
