// RUN: not %target-swift-frontend %s -emit-silgen

func foo(_ f: @convention(c) @autoclosure () -> Int) -> Void {}
foo(1)
