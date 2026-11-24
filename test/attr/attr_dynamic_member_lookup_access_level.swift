// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-library-evolution -verify-additional-prefix resilient-
// RUN: %if swift_future %{ %target-typecheck-verify-swift -swift-version future -verify-additional-prefix future- %}

@dynamicMemberLookup
public struct Accessible1 {
  public subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
private struct Accessible2 {
  fileprivate subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
open class Accessible3 {
  public subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
open class Accessible4 {
  open subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
public struct Accessible5 {
  subscript(dynamicMember member: String) -> Int {
    return 42
  }

  public subscript(dynamicMember member: StaticString) -> Int {
    return 42
  }
}

@dynamicMemberLookup
public struct Inaccessible1 {
  // expected-swift6-warning @+3 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type; this will be an error in a future Swift language mode}}{{3-3=public }}
  // expected-resilient-error @+2 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-3=public }}
  // expected-future-error @+1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-3=public }}
  subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
public struct Inaccessible2 {
  // expected-swift6-warning @+3 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type; this will be an error in a future Swift language mode}}{{21-29=public}}
  // expected-resilient-error @+2 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{21-29=public}}
  // expected-future-error @+1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{21-29=public}}
  @usableFromInline internal subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
internal struct Inaccessible3 {
  // expected-swift6-warning @+3 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type; this will be an error in a future Swift language mode}}{{3-10=internal}}
  // expected-resilient-error @+2 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-10=internal}}
  // expected-future-error @+1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-10=internal}}
  private subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
private struct Inaccessible4 {
  // expected-swift6-warning @+3 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type; this will be an error in a future Swift language mode}}{{3-10=fileprivate}}
  // expected-resilient-error @+2 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-10=fileprivate}}
  // expected-future-error @+1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-10=fileprivate}}
  private subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
public struct Inaccessible5 {
  internal struct Nested { }
  var nested: Nested

  // expected-swift6-warning @+3 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type; this will be an error in a future Swift language mode}}{{3-11=public}}
  // expected-resilient-error @+2 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-11=public}}
  // expected-future-error @+1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-11=public}}
  internal subscript<Value>(dynamicMember keyPath: KeyPath<Nested, Value>) -> Value {
    nested[keyPath: keyPath]
  }
}

// Make sure we only diagnose the inaccessible candidate in this case.
@dynamicMemberLookup
struct InaccessibleAndInvalidCandidate {
  private subscript(dynamicMember x: String) -> Void { () }
  // expected-swift6-warning@-1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type; this will be an error in a future Swift language mode}}
  // expected-resilient-error@-2 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}
  // expected-future-error@-3 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}
  subscript(dynamicMember x: Int) -> Void { () }
}
