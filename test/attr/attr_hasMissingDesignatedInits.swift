// RUN: %target-swift-frontend -typecheck %s -verify

// This test just makes sure we don't error if we see either of these attributes.

@_hasMissingDesignatedInitializers // no-error
class MyClass {}

@_inheritsConvenienceInitializers // no-error
class MyOtherClass {}
