/// Check for reliable availability checking in inlinable code even when
/// skipping some function bodies. rdar://82269657

// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s -target x86_64-apple-macos10.12
// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s -target x86_64-apple-macos10.12 -experimental-skip-non-inlinable-function-bodies
// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s -target x86_64-apple-macos10.12 -experimental-skip-non-inlinable-function-bodies-without-types
// RUN: %target-swift-frontend -typecheck -dump-type-refinement-contexts %s -target x86_64-apple-macos10.12 -experimental-skip-all-function-bodies

// REQUIRES: OS=macosx

@available(macOS 10.14, *)
public func foo() { }

@inlinable public func inlinableFunc() {
    if #available(macOS 10.14, *) {
        foo()
    }
}

public func funcWithType() {
    struct S {}
    if #available(macOS 10.14, *) {
        foo()
    }
}
