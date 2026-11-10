// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -wmo -parse-as-library -c -I %t %t/Lib.swift -o %t/Lib.o -emit-module -emit-module-path %t/Lib.swiftmodule -emit-empty-object-file
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -Werror HeapAllocation -wmo -parse-as-library -emit-ir -verify -I %t %t/Main.swift -o /dev/null

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

// A problem in a witness of a conformance from another module is reported at each use of the
// conformance - here, each place an existential is created - not only at the first one. An
// ignored function doesn't hide it from the others.

//--- Lib.swift
public protocol P: AnyObject {
  func make() -> [Int]
}

public final class C: P {
  public init() {}
  public func make() -> [Int] { [1, 2, 3] }
}

//--- Main.swift
import Lib

public func first() -> any P { C() }
// expected-error@-1 {{creating an instance of type 'C' involves heap allocation}}
// expected-error@-2 {{creating an instance of type '_ContiguousArrayStorage<Int>' involves heap allocation}}

public func second() -> any P { C() }
// expected-error@-1 {{creating an instance of type 'C' involves heap allocation}}
// expected-error@-2 {{creating an instance of type '_ContiguousArrayStorage<Int>' involves heap allocation}}

@diagnose(HeapAllocation, as: ignored, reason: "the test's exception")
public func ignored() -> any P { C() }

public func third() -> any P { C() }
// expected-error@-1 {{creating an instance of type 'C' involves heap allocation}}
// expected-error@-2 {{creating an instance of type '_ContiguousArrayStorage<Int>' involves heap allocation}}
