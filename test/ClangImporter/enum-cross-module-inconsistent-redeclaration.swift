// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify %t/typed-then-untyped.swift -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/untyped-then-typed.swift -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/transitivetyped-then-transitiveuntyped.swift -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/typed-then-untyped-then-untypedduplicate.swift -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/untyped-then-untypedduplicate-then-typed.swift -I %t

// Check that, when two unrelated C modules declare enum constants with the same
// name but different underlying types, ClangImporter does not conflate them
// despite both being in the same redeclaration chain. Since this behavior can
// depend on the order modules are imported in, we test several different
// combinations of `import` statements.
//
// We also check the three-way case: when three unrelated C modules declare
// enum constants with the same name, where two (Untyped and UntypedDuplicate)
// share an underlying type and the third (Typed) does not, ClangImporter
// imports Untyped and UntypedDuplicate (but not Typed) as a single Swift
// declaration, regardless of which import order makes which enum canonical.

//--- Typed.h
#ifndef TYPED_H
#define TYPED_H

enum : unsigned short {
    SHARED_CONSTANT_NAME = 0x00A8,
};

#endif

//--- Untyped.h
#ifndef UNTYPED_H
#define UNTYPED_H

enum {
    SHARED_CONSTANT_NAME = 0x00A8,
};

#endif

//--- TransitiveTyped.h
#include "Typed.h"

//--- TransitiveUntyped.h
#include "Untyped.h"

//--- UntypedDuplicate.h
#ifndef UNTYPED_DUPLICATE_H
#define UNTYPED_DUPLICATE_H

enum {
    SHARED_CONSTANT_NAME = 5,
};

#endif

//--- module.modulemap
module Typed {
    header "Typed.h"
    export *
}
module Untyped {
    header "Untyped.h"
    export *
}
module TransitiveTyped {
    header "TransitiveTyped.h"
    export *
}
module TransitiveUntyped {
    header "TransitiveUntyped.h"
    export *
}
module UntypedDuplicate {
    header "UntypedDuplicate.h"
    export *
}

//--- typed-then-untyped.swift
import Typed
import Untyped

let z: UInt16 = SHARED_CONSTANT_NAME

//--- untyped-then-typed.swift
import Untyped
import Typed

let z: UInt16 = SHARED_CONSTANT_NAME

//--- transitivetyped-then-transitiveuntyped.swift
// This transitive case is interesting because the order that the compiler
// processes transitive imports may depend on clang implementation details.

import TransitiveTyped
import TransitiveUntyped

let z: UInt16 = SHARED_CONSTANT_NAME

//--- typed-then-untyped-then-untypedduplicate.swift
// Typed, whose type doesn't match Untyped/UntypedDuplicate, is imported first
// and becomes canonical. Untyped and UntypedDuplicate still merge into a
// single declaration.
import Typed
import Untyped
import UntypedDuplicate

let a: UInt16 = SHARED_CONSTANT_NAME
let b: Int = SHARED_CONSTANT_NAME

//--- untyped-then-untypedduplicate-then-typed.swift
// Untyped or UntypedDuplicate is imported first and becomes canonical, so the
// other one matches it by type directly and merges without needing the
// shared-representative bookkeeping that the other ordering relies on.
import Untyped
import UntypedDuplicate
import Typed

let a: UInt16 = SHARED_CONSTANT_NAME
let b: Int = SHARED_CONSTANT_NAME
