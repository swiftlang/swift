// RUN: rm -rf %t
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:   -D ImportAB \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %t%{fs-sep}Inputs %t%{fs-sep}importer.swift

// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %t%{fs-sep}Inputs %t%{fs-sep}importer.swift

//--- Inputs/module.modulemap
module ModuleA {
  header "headerA.h"
  requires cplusplus
}

module ModuleB {
  header "headerB.h"
  requires cplusplus
}

//--- Inputs/headerA.h
struct B;
struct A {
  using FromA = int; // type tag to validate we have the right type
  using Other = B;
};

//--- Inputs/headerB.h
struct A;
struct B {
  using FromB = int; // type tag to validate we have the right type
  using Other = A;
};

//--- importer.swift

// Import order shouldn't matter. Test both configurations just in case.
#if ImportAB
import ModuleA
import ModuleB
#else
import ModuleB
import ModuleA
#endif

// Check all decls in NS are usable

let _: A
let _: A.FromA = 42
let _: A.FromB = 42 // expected-error {{not a member}}
let _: B
let _: B.FromA = 42 // expected-error {{not a member}}
let _: B.FromB = 42

func unifyA(a: A) {
  // Check that A === B.Other
  let _: B.Other = a
  let _: A.Other.Other = a
  let _: B.Other.Other.Other = a
}

func unifyB(b: B) {
  // Check that B === A.Other
  let _: A.Other = b
  let _: B.Other.Other = b
  let _: A.Other.Other.Other = b
}
