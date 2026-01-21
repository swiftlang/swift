// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

func test() {
  _ = "test"
  /* comment */ #if true
  #endif
}
