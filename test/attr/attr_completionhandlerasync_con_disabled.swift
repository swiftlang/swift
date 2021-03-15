// RUN: %target-typecheck-verify-swift

// expected-error@+1{{'completionHandlerAsync' attribute is only valid when experimental concurrency is enabled}}
@completionHandlerAsync("foobar", completionHandlerIndex: 1)
func func2() {}
