// RUN: not %target-swift-frontend -typecheck -enable-experimental-com-interop -com-interop-model=bogus %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -enable-experimental-com-interop -com-interop-model=none %s 2>&1 | %FileCheck %s -check-prefix NONE

// The initial models are `microsoft` and `corefoundation`; `none` is not one.
// CHECK: unsupported argument 'bogus' to option '-com-interop-model='
// NONE: unsupported argument 'none' to option '-com-interop-model='
