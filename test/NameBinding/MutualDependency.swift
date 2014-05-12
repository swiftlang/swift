// RUN: %swift -parse %s -enable-source-import -I=%S/Inputs -parse-as-library
// RUN: %swift -parse %s -enable-source-import -I=%S/Inputs
// RUN: %swift -parse %S/Inputs/MutualDependencyHelper.swift -enable-source-import -I=%S

// FIXME: We should be able to handle this even in -i mode.
// RUN: %swift -i -I=%S/Inputs -enable-source-import %s -verify


import MutualDependencyHelper

class MyClass {
  // FIXME: This is an error in -i mode.
  var delegate : MyDelegate // expected-error {{use of undeclared type}}

  init() {}
}
