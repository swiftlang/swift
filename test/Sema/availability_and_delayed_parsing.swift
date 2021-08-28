/// Check for reliable availability checking in inlinable code even when
/// skipping some function bodies. rdar://82269657

// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s -target x86_64-apple-macos10.10 2>&1 \
// RUN:    | %FileCheck %s --check-prefixes TRC-API,TRC-INLINABLE,TRC-WITHTYPES,TRC-FULL
// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s -target x86_64-apple-macos10.10 -experimental-skip-non-inlinable-function-bodies-without-types 2>&1 \
// RUN:    | %FileCheck %s --check-prefixes TRC-API,TRC-INLINABLE,TRC-WITHTYPES,TRC-FULL-NOT
// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s -target x86_64-apple-macos10.10 -experimental-skip-non-inlinable-function-bodies 2>&1 \
// RUN:    | %FileCheck %s --check-prefixes TRC-API,TRC-INLINABLE,TRC-WITHTYPES-NOT,TRC-FULL-NOT
// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s -target x86_64-apple-macos10.10 -experimental-skip-all-function-bodies 2>&1 \
// RUN:    | %FileCheck %s --check-prefixes TRC-API,TRC-INLINABLE-NOT,TRC-WITHTYPES-NOT,TRC-FULL-NOT

// REQUIRES: OS=macosx

@available(macOS 10.12, *)
public func foo() { }
// TRC-API: (root versions=[10.10.0,+Inf)
// TRC-API:   (decl versions=[10.12,+Inf) decl=foo()

@inlinable public func inlinableFunc() {
    if #available(macOS 10.12, *) {
        foo()
    }
}
// TRC-INLINABLE:  (condition_following_availability versions=[10.12,+Inf)
// TRC-INLINABLE:  (if_then versions=[10.12,+Inf)
// TRC-INLINABLE-NOT-NOT:  (condition_following_availability versions=[10.12,+Inf)
// TRC-INLINABLE-NOT-NOT:  (if_then versions=[10.12,+Inf)

public func funcWithType() {
    struct S {}
    if #available(macOS 10.13, *) {
        foo()
    }
}
// TRC-WITHTYPES:  (condition_following_availability versions=[10.13,+Inf)
// TRC-WITHTYPES:  (if_then versions=[10.13,+Inf)
// TRC-WITHTYPES-NOT-NOT:  (condition_following_availability versions=[10.13,+Inf)
// TRC-WITHTYPES-NOT-NOT:  (if_then versions=[10.13,+Inf)

public func funcSkippable() {
    if #available(macOS 10.14, *) {
        foo()
    }
}
// TRC-FULL:  (condition_following_availability versions=[10.14,+Inf)
// TRC-FULL:  (if_then versions=[10.14,+Inf)
// TRC-FULL-NOT-NOT:  (condition_following_availability versions=[10.14,+Inf)
// TRC-FULL-NOT-NOT:  (if_then versions=[10.14,+Inf)
