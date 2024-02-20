// RUN: %target-swift-frontend                           \
// RUN:     -enable-experimental-feature BitwiseCopyable \
// RUN:     %s                                           \
// RUN:     -typecheck -verify

// Verify that the BitwiseCopyable feature flag works both in asserts and noasserts builds.

struct S : _BitwiseCopyable {
}
