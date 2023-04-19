
// >> First verify that when building the stdlib, we do have the copyable constraint. Note the module-name!!
// RUN: not %target-swift-frontend -typecheck -parse-stdlib -module-name Swift %s 2>&1 | %FileCheck --check-prefix CHECK-SWIFT-MODULE %s

// Now demonstrate that plain -parse-stdlib, such as in some arbitrary test, doesn't get the Copyable constraint.
// But we also now can't declare a noncopyable type in this mode.
// RUN: not %target-swift-frontend -typecheck -verify -parse-stdlib %s 2>&1 | %FileCheck %s

@_marker public protocol Copyable {}

func nextTime<T>(_ t: T) {}

struct MO : ~Copyable {} // CHECK: cannot suppress implicit conformance to 'Copyable'

nextTime(MO()) // CHECK-SWIFT-MODULE: move-only type 'MO' cannot be used with generics yet
