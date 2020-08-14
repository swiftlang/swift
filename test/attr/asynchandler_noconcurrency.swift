// RUN: %target-swift-frontend -typecheck -verify %s

@asyncHandler func asyncHandler1() { }
// expected-error@-1{{'@asyncHandler' is only valid when experimental concurrency is enabled}}
