// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -module-name ResilientFields -o %t%{fs-sep}ResilientFields.swiftmodule %t%{fs-sep}ResilientFields.swift
// RUN: %target-swift-emit-silgen %t%{fs-sep}main.swift -I %t -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeSubclassing -target %target-swift-5.8-abi-triple -verify

// REQUIRES: swift_feature_ForeignReferenceTypeSubclassing

//--- ResilientFields.swift

public struct NonFrozenStruct {
  public var x: Int
  public init() { x = 0 }
}

@frozen
public struct FrozenStruct {
  public var x: Int
  public init() { x = 0 }
}

public enum NonFrozenEnum {
  case a, b
}

public class ResilientClass {
  public init() {}
}

//--- main.swift

import InheritFRTSubclassing
import ResilientFields

// Size of each stored property must be known at compile time.
final class FixedSizeFields: SubclassableShared {
  var i = 0
  var s = ""
  var frozen = FrozenStruct()
  var resilientClass = ResilientClass()
}

final class NonFixedSizeFields: SubclassableShared {
  var nonFrozenStruct = NonFrozenStruct() // expected-error {{property 'nonFrozenStruct' cannot have type 'NonFrozenStruct', whose size is not known at compile time, because it is declared in a class that subclasses a C++ foreign reference type}}
  var nonFrozenEnum = NonFrozenEnum.a // expected-error {{property 'nonFrozenEnum' cannot have type 'NonFrozenEnum', whose size is not known at compile time, because it is declared in a class that subclasses a C++ foreign reference type}}
  var optional: NonFrozenStruct? = nil // expected-error {{property 'optional' cannot have type 'NonFrozenStruct?', whose size is not known at compile time, because it is declared in a class that subclasses a C++ foreign reference type}}
  var tuple = (0, NonFrozenStruct()) // expected-error {{property 'tuple' cannot have type '(Int, NonFrozenStruct)', whose size is not known at compile time, because it is declared in a class that subclasses a C++ foreign reference type}}
}
