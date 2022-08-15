// RUN: %target-typecheck-verify-swift

// Code should type check with a new enough deployment target:
// RUN: %target-swift-frontend -typecheck %s -target %target-cpu-apple-macos50

// Code should also type check when the library client deployment target is
// lowered via -target-min-inlining-version.
// RUN: %target-swift-frontend -typecheck %s -target %target-cpu-apple-macos50 -enable-library-evolution -target-min-inlining-version min

// REQUIRES: OS=macosx

@available(macOS 50, *)
public struct NewStruct {}

@available(macOS 50, *)
@propertyWrapper
public struct NewPropertyWrapper<Value> {
  public var wrappedValue: Value

  public init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

@available(macOS 50, *)
struct GoodReferenceStruct {
  var x: NewStruct
  @NewPropertyWrapper var y: Int
  lazy var z: Int = 42
}

@available(macOS 50, *)
struct GoodNestedReferenceStruct {
  struct Inner {
    var x: NewStruct
    @NewPropertyWrapper var y: Int
    lazy var z: Int = 42
  }
}

struct BadReferenceStruct1 {
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  var x: NewStruct
  
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  @NewPropertyWrapper var y: Int
  
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  lazy var z: Int = 42
}

@available(macOS 40, *)
struct BadReferenceStruct2 {
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  var x: NewStruct
  
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  @NewPropertyWrapper var y: Int
  
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  lazy var z: Int = 42
}

@available(macOS 40, *)
public struct PublicStruct {
  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  public var x: NewStruct

  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  @NewPropertyWrapper public var y: Int

  // expected-error@+1 {{stored properties cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  public lazy var z: Int = 42
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

@available(macOS 40, *)
public enum PublicReferenceEnum {
  // expected-error@+1 {{enum cases with associated values cannot be marked potentially unavailable with '@available'}}
  @available(macOS 50, *)
  case x(NewStruct)
}
