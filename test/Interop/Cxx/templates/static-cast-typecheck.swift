// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -o /dev/null -verify -suppress-remarks -suppress-notes \
// RUN:     -cxx-interoperability-mode=default \
// RUN:     -I %t%{fs-sep}Inputs %t%{fs-sep}main.swift \
// RUN:     -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}header.h

//--- Inputs/module.modulemap
module CxxHeader {
    header "header.h"
    requires cplusplus
}

//--- Inputs/header.h
#pragma once

// TODO: the following diagnostics should be moved to main.swift at the call site
//       that triggers the instantiation
template <class O, class I> O cxxCast(I i) { return static_cast<O>(i); }
// expected-error@-1 {{cannot cast 'const SubPrivateT' to its private base class 'const BaseT'}}
// expected-error@-2 {{cannot cast 'const SubProtectedT' to its protected base class 'const BaseT'}}
// expected-error@-3 {{cannot cast 'const SubClassT' to its private base class 'const BaseT'}}
// expected-error@-4 {{no matching conversion for static_cast from 'UnrelatedT' to 'BaseT'}}

struct BaseT { };

struct SubT          : BaseT            { }; // publicly inherit from BaseT
struct SubPrivateT   : private BaseT    { }; // privately inherit from BaseT
struct SubProtectedT : protected BaseT  { }; // privately inherit from BaseT
class  SubClassT     : BaseT            { }; // privately inherit from BaseT
struct UnrelatedT                       { }; // does not inherit from BaseT


//--- main.swift
import CxxHeader

let _: BaseT = cxxCast(SubT())          // valid:   upcast to public base
let _: BaseT = cxxCast(SubPrivateT())   // invalid: upcast to non-public base
let _: BaseT = cxxCast(SubProtectedT()) // invalid: upcast to non-public base
let _: BaseT = cxxCast(SubClassT())     // invalid: upcast to non-public base
let _: BaseT = cxxCast(UnrelatedT())    // invalid: cast to unrelated type
