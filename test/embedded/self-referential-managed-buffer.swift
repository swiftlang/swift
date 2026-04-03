// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature Embedded -verify
// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// Self-referential generic superclass should be diagnosed in Embedded Swift
// because it creates a circular metadata dependency.

final class Tree: ManagedBuffer<Int, Tree> { // expected-error {{class 'Tree' cannot inherit from 'ManagedBuffer<Int, Tree>' in Embedded Swift because a generic argument of the superclass refers to the subclass itself, creating a circular metadata dependency}}
    var children: AnySequence<Tree> { AnySequence([]) }
}
