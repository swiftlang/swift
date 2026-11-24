// RUN: %target-swift-frontend -typecheck -verify %s %S/Inputs/inlinable-other.swift

@frozen public struct HasInitExpr {
  public var hasInlinableInit = mutatingFunc(&OtherStruct.staticProp)
  // expected-error@-1 {{setter for static property 'staticProp' is internal and cannot be referenced from a property initializer in a '@frozen' type}}
}

public func mutatingFunc(_: inout Int) -> Int {
  return 0
}
