// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Module forgets to re-export the module defining the parameter type.
// RUN: %target-swift-frontend -typecheck -I %t%{fs-sep}Inputs -I %t%{fs-sep}FailingInputs \
// RUN:   -cxx-interoperability-mode=default -disable-objc-interop \
// RUN:   -verify -verify-additional-prefix modularization-failure- \
// RUN:   -verify-additional-file %t%{fs-sep}FailingInputs%{fs-sep}test.h \
// RUN:   -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}foo.h %t/test.swift

// Same header contents, same Swift contents - only module re-export differs.
// RUN: cp %t/FailingInputs/test.h %t/PassingInputs/test.h

// RUN: %target-swift-frontend -typecheck -I %t%{fs-sep}Inputs -I %t%{fs-sep}PassingInputs \
// RUN:   -cxx-interoperability-mode=default -disable-objc-interop %t%{fs-sep}test.swift -verify

// Importing a virtual method of a foreign reference type synthesizes a C++
// thunk that calls the method, and building that call requires the parameter
// type to be complete. When the header is incorrectly modularized the
// definition of the parameter type is not reachable from the client, which is
// diagnosed as a modularization failure. This exercises a path that used to
// crash when emitting the error.

//--- Inputs/module.modulemap
module Foo {
  header "foo.h"
  requires cplusplus
}

//--- Inputs/foo.h
// expected-modularization-failure-note@+1{{definition here is not reachable}}
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
  // expected-modularization-failure-error@+1{{missing '#include "foo.h"'; 'Foo' must be defined before it is used}}
  virtual void takeFoo(Foo values) {}
};

//--- test.swift
import Test

@available(SwiftStdlib 5.8, *)
func test(_ base: Base) {
  _ = base.takeFoo
}
