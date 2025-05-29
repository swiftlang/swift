// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t %t/LibCore.swift -public-module-name Lib
// RUN: %target-swift-frontend -emit-module -I %t -o %t %t/Lib.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %t/other.swift -I %t -verify -swift-version 5 -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- main.swift

import Swift
// expected-note {{add import of module 'Lib'}}

func foo(_ x: Int) -> Int {
  x.bar // expected-error {{property 'bar' is not available due to missing import of defining module 'Lib'}}
}

//--- other.swift

import Lib

//--- Lib.swift

@_exported import LibCore

//--- LibCore.swift

extension Int {
  public var bar: Int {
    return self < 0 ? -self : self
  }
}
