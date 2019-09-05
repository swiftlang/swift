// RUN: %target-swift-frontend -typecheck %s -import-objc-header %S/Inputs/availability_platform_categories.h

// REQUIRES: OS=macos

// Test when a function is associated to an Objective-C category with the
// wrong unavailability. rdar://problem/53956555

print(SharedInterface.foo())
