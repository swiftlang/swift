// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/attr_dynamic_member_lookup_other.swift -emit-module-path %t/attr_dynamic_member_lookup_other.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/client.swift -I %t

//--- client.swift

import attr_dynamic_member_lookup_other

func f(m: MySubscript) -> Int {
  return m.x
}

//--- attr_dynamic_member_lookup_other.swift

public struct S {
  public var x: Int
}

@dynamicMemberLookup
public struct MySubscript {
  public subscript(dynamicMember keyPath: KeyPath<S, Int>) -> Int {
     return 0
  }
}
