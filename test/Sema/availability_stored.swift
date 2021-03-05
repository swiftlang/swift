// RUN: %target-typecheck-verify-swift

// Code should type check with a new enough deployment target:
// RUN: %target-swift-frontend -typecheck %s -target x86_64-apple-macos50

// REQUIRES: OS=macosx

@available(macOS 50, *)
struct NewStruct {}

@available(macOS 50, *)
struct GoodReference {
  var x: NewStruct
}

@available(macOS 50, *)
struct GoodNestedReference {
  struct Inner {
    var x: NewStruct
  }
}

struct BadReference1 {
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  var x: NewStruct
}

@available(macOS 40, *)
struct BadReference2 {
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  var x: NewStruct
}
