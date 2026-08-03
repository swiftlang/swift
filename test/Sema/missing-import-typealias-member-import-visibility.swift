// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/Original.swiftmodule %t/Original.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t/Aliases.swiftmodule %t/Aliases.swift

// Without library evolution, MemberImportVisibility still diagnoses a public
// use of a typealias whose underlying type comes from a missing import.
// https://github.com/swiftlang/swift/issues/91096
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/Client.swift -I %t -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- Original.swift
public struct DataLike {
  public init() {}
}

//--- Aliases.swift
import Original
public typealias MyData = DataLike

//--- Client.swift
import Aliases

// expected-warning@+2 {{'MyData' aliases 'Original.DataLike' and cannot be used here because 'Original' was not imported by this file; this is an error in the Swift 6 language mode}}
// expected-note@+1 {{add import of module 'Original'}} {{1-1=import Original\n}}
public func foo(x: MyData) {}
