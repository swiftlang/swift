// RUN: %target-swift-frontend -emit-module-path /dev/null %s

func foo<T>(_: (T) -> ()) -> T { fatalError() }

let y = foo { (x: @escaping () -> (), y: Int) in }
