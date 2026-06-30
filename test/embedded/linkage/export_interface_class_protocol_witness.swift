// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: swift_feature_Embedded
// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -emit-ir -emit-module -o %t/Library.ll -emit-module-path %t/Library.swiftmodule %t/Library.swift -enable-experimental-feature Embedded -parse-as-library -module-name Library
// RUN: %FileCheck %s -check-prefix LIBRARY-IR < %t/Library.ll

// RUN: %target-swift-frontend -emit-ir -o %t/Client.ll %t/Client.swift -I %t -enable-experimental-feature Embedded -parse-as-library -module-name Client
// RUN: %FileCheck %s -check-prefix CLIENT-IR < %t/Client.ll

//--- Library.swift

// A class-bound protocol declared in the same module as the conforming class.
public protocol Resource: AnyObject {
  func read() -> Int
}

// `@export(interface)` makes Library the unique strong definer of `Stream`
// and its protocol conformances. The protocol witness table for
// `Stream: Resource` should therefore be a strong definition here.
// LIBRARY-IR-DAG: @"$e7Library6StreamCAA8ResourceAAWP" = {{(protected )?}}constant
@export(interface)
public class Stream: Resource {
  public var byte: Int = 42
  public init() {}
  public func read() -> Int { return byte }
}

public func makeStream() -> Stream {
  return Stream()
}

//--- Client.swift
import Library

// The importer must reference the protocol witness table for
// `Stream: Resource` as an external declaration rather than re-emitting it.
// Before the fix the importer emitted its own (constant) definition here,
// which collided with Library's strong definition at link time.
// CLIENT-IR-DAG: @"$e7Library6StreamCAA8ResourceAAWP" = external global
// CLIENT-IR-NOT: @"$e7Library6StreamCAA8ResourceAAWP" ={{.*}} constant

public func use() -> Int {
  let stream = makeStream()
  // Forming a class-bound existential of `Resource` from the exported
  // class is what causes the importer to need the witness table.
  let any: any Resource = stream
  return any.read()
}
