// RUN: %target-typecheck-verify-swift -swift-version 7
// REQUIRES: swift7

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
  // expected-error @+1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-3=public }}
  subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
public struct Inaccessible2 {
  // expected-error @+2 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{21-29=public}}
  // expected-error @+1 {{'@usableFromInline' attribute can only be applied to internal or package declarations, but subscript 'subscript(dynamicMember:)' is public}}
  @usableFromInline internal subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
internal struct Inaccessible3 {
  // expected-error @+1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-10=internal}}
  private subscript(dynamicMember member: String) -> Int {
    return 42
  }
}

@dynamicMemberLookup
private struct Inaccessible4 {
  // expected-error @+1 {{'@dynamicMemberLookup' requires 'subscript(dynamicMember:)' to be as accessible as its enclosing type}}{{3-10=fileprivate}}
  private subscript(dynamicMember member: String) -> Int {
    return 42
  }
}
