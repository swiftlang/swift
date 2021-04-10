// RUN: %target-typecheck-verify-swift

// Code should type check with a new enough deployment target:
// RUN: %target-swift-frontend -typecheck %s -target %target-cpu-apple-macos50

// REQUIRES: OS=macosx

@available(macOS 50, *)
struct NewStruct {}

@available(macOS 50, *)
struct GoodReferenceStruct {
  var x: NewStruct
}

@available(macOS 50, *)
struct GoodNestedReferenceStruct {
  struct Inner {
    var x: NewStruct
  }
}

struct BadReferenceStruct1 {
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  var x: NewStruct
}

@available(macOS 40, *)
struct BadReferenceStruct2 {
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  var x: NewStruct
}

// The same behavior should hold for enum elements with payloads.
@available(macOS 50, *)
enum GoodReferenceEnum {
  case x(NewStruct)
}

@available(macOS 50, *)
enum GoodNestedReferenceEnum {
  enum Inner {
    case x(NewStruct)
  }
}

enum BadReferenceEnum1 {
  // expected-error@+1 {{enum cases with associated values cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  case x(NewStruct)
}

@available(macOS 40, *)
enum BadReferenceEnum2 {
  // expected-error@+1 {{enum cases with associated values cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  case x(NewStruct)
}
