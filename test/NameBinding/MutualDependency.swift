// RUN: %target-swift-frontend -parse %s -enable-source-import -I %S/Inputs -parse-as-library
// RUN: %target-swift-frontend -parse %s -enable-source-import -I %S/Inputs
// RUN: %target-swift-frontend -parse %S/Inputs/MutualDependencyHelper.swift -enable-source-import -I %S

// FIXME: We should be able to handle this even in -i mode.
// RUN: %target-swift-frontend -interpret -I %S/Inputs -enable-source-import %s -verify


import MutualDependencyHelper

public class MyClass {
  // FIXME: This is an error in -i mode.
  public var delegate : MyDelegate // expected-error {{use of undeclared type}}

  public init() {}
}
