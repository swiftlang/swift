// RUN: %target-typecheck-verify-swift

// expected-error@+1{{'@completionHandlerAsync' should be attached to a non-async completion-handler function}}
@completionHandlerAsync("foobar", completionHandlerIndex: 1)
func func2() {}
