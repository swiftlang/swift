// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/refined-then-plain.swift -I %t -Xcc -Wno-nullability-completeness
// RUN: %target-swift-frontend -typecheck -verify %t/plain-then-refined.swift -I %t -Xcc -Wno-nullability-completeness
// RUN: %target-swift-frontend -typecheck -verify %t/transitiverefined-then-transitiveplain.swift -I %t -Xcc -Wno-nullability-completeness

// When two unrelated C modules declare the same function with signatures that
// differ only in nullability (one `_Nonnull`, one plain), they share a single
// clang redeclaration chain, so ClangImporter collapses them into one Swift
// declaration. Which redeclaration supplies the imported signature must not
// depend on the order the modules happen to be imported: the importer always
// selects the most refined (here, `_Nonnull`) redeclaration. We check this by
// passing `nil`, which is rejected only by the refined (non-optional
// parameter) import, across several import orderings.

//--- Refined.h
#ifndef REFINED_H
#define REFINED_H
void * _Nonnull sharedFunc(void * _Nonnull p);
#endif

//--- Plain.h
#ifndef PLAIN_H
#define PLAIN_H
void *sharedFunc(void *p);
#endif

//--- TransitiveRefined.h
#include "Refined.h"

//--- TransitivePlain.h
#include "Plain.h"

//--- module.modulemap
module Refined {
    header "Refined.h"
    export *
}
module Plain {
    header "Plain.h"
    export *
}
module TransitiveRefined {
    header "TransitiveRefined.h"
    export *
}
module TransitivePlain {
    header "TransitivePlain.h"
    export *
}

//--- refined-then-plain.swift
import Refined
import Plain

func test(_ p: UnsafeMutableRawPointer) {
  // 'nil' is rejected only if the refined (`_Nonnull`) import was selected.
  _ = sharedFunc(nil) // expected-error {{'nil' is not compatible with expected argument type 'UnsafeMutableRawPointer'}}
  _ = sharedFunc(p)
}

//--- plain-then-refined.swift
import Plain
import Refined

func test(_ p: UnsafeMutableRawPointer) {
  _ = sharedFunc(nil) // expected-error {{'nil' is not compatible with expected argument type 'UnsafeMutableRawPointer'}}
  _ = sharedFunc(p)
}

//--- transitiverefined-then-transitiveplain.swift
// The transitive case is interesting because the order the compiler processes
// transitive imports may depend on clang implementation details.
import TransitiveRefined
import TransitivePlain

func test(_ p: UnsafeMutableRawPointer) {
  _ = sharedFunc(nil) // expected-error {{'nil' is not compatible with expected argument type 'UnsafeMutableRawPointer'}}
  _ = sharedFunc(p)
}
