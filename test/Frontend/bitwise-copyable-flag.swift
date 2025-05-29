// RUN: %target-swift-frontend                           \
// RUN:     %s                                           \
// RUN:     -typecheck -verify

// Verify that the BitwiseCopyable feature flag works both in asserts and noasserts builds.

struct S : BitwiseCopyable {
}
