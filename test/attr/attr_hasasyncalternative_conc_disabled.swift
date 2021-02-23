// RUN: %target-typecheck-verify-swift

@hasAsyncAlternative // expected-error {{'hasAsyncAlternative' attribute is only valid when experimental concurrency is enabled}}
func func1() {}
