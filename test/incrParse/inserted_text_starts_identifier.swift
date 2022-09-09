// RUN: %empty-directory(%t)
// RUN: %validate-incrparse %s --test-case STRING

// rdar://problem/45259469
// https://github.com/apple/swift/issues/51498

self = <<STRING<|||_                            _>>>foo(1)[object1, object2] + o bar(1)
