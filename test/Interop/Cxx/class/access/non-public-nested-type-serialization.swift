// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -cxx-interoperability-mode=default -I %t/Inputs %t/Lib.swift -module-name Lib -o %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-sil -cxx-interoperability-mode=default -I %t/Inputs -I %t %t/Client.swift -o /dev/null

// A module can reference a non-public nested C++ type through a public typedef,
// and clients can read that reference back.

//--- Inputs/module.modulemap
module PrivateNested {
  header "header.h"
  requires cplusplus
}

//--- Inputs/header.h
class HasPrivateNested {
  class Private {};

public:
  typedef Private PublicAlias;
  static PublicAlias make() { return {}; }
};

//--- Lib.swift
import PrivateNested

public func roundTrip(_ x: HasPrivateNested.PublicAlias) -> HasPrivateNested.PublicAlias {
  return x
}

//--- Client.swift
import Lib
import PrivateNested

let _ = roundTrip(HasPrivateNested.make())
