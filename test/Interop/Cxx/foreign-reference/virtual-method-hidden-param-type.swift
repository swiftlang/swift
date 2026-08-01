// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Module forgets to re-export the module defining the parameter type.
// RUN: not --crash %target-swift-frontend -typecheck -I %t%{fs-sep}Inputs -I %t%{fs-sep}FailingInputs \
// RUN:   -cxx-interoperability-mode=default -verify -verify-additional-prefix modularization-failure- %t/test.swift

// Same header contents, same Swift contents - only module re-export differs.
// RUN: cp %t/FailingInputs/test.h %t/PassingInputs/test.h

// RUN: %target-swift-frontend -typecheck -I %t%{fs-sep}Inputs -I %t%{fs-sep}PassingInputs \
// RUN:   -cxx-interoperability-mode=default %t/test.swift -verify

// When importing the incorrectly modularized header ClangImporter crashes when
// trying to create a thunk:
// Assertion failed: ((M->isGlobalModule() || Loc.isValid()) &&
// "setVisible expects a valid import location"), function setVisible
//
// The assertion only fires when the method is virtual (no thunk is synthesized
// otherwise), when the enclosing type is a foreign reference type (value types
// need no thunk either), and when the method has a body in the header (a call
// to a declaration does not need the parameter type to be complete).

//--- Inputs/module.modulemap
module Foo {
  header "foo.h"
  requires cplusplus
}

//--- Inputs/foo.h
struct Foo { };

//--- FailingInputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}
//--- PassingInputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
  export Foo
}

//--- FailingInputs/test.h
#include "foo.h"

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) Base {
  // This function decl needs to be a definition to reproduce.
  virtual void takeFoo(Foo values) {}
};

//--- test.swift
import Test

@available(SwiftStdlib 5.8, *)
func test(_ base: Base) {
  _ = base.takeFoo
}
