// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -suppress-remarks -suppress-notes \
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
//       that triggers the instantiation (see static-cast-typecheck.swift)
// expected-error@+2{{could not substitute parameters for C++ function template 'funcTempl' with Swift closure of '(inout Int32) -> ()' type that has an inout parameter}}
// expected-error@+1{{could not substitute parameters for C++ function template 'funcTempl' with Swift closure of '(Int32, inout Int32) -> ()' type that has an inout parameter}}
template <class T> void funcTempl(T value) { }

// expected-error@+2 2{{could not substitute parameters for C++ function template 'funcTempl2' with Swift closure of '(inout Int32) -> ()' type that has an inout parameter}}
// expected-error@+1 2{{could not substitute parameters for C++ function template 'funcTempl2' with Swift closure of '(Int32, inout Int32) -> ()' type that has an inout parameter}}
template <class T, class U> void funcTempl2(T t, U u) { }

// expected-error@+1 {{could not substitute parameters for C++ function template 'funcTemplAndIntRef' with Swift closure of '(inout Int32) -> ()' type that has an inout parameter}}
template <class T> void funcTemplAndIntRef(T value, int &ref) { }

int get42(void) { return 42; }

//--- main.swift
import CxxHeader

func takesTrue() { funcTempl(true) }
func takesBool(x: Bool) { funcTempl(x) }

var x: Int32 = 1
var y: Int32 = 2

// OK: () -> ()
func takesRecursively() { funcTempl(takesRecursively) }
func takesRecursiveClosure() { funcTempl({() in takesRecursiveClosure()}) }
func takesSwiftClosure() { funcTempl({() in ()}) }
func takesTakesTrue() { funcTempl(takesTrue) }

// OK: () -> Bool
func takesSwiftClosureReturningBool() { funcTempl({() -> Bool in true}) }
// OK: (Bool) -> ()
func takesSwiftClosureTakingBool() { funcTempl({(x: Bool) in ()}) }
func takesTakesBool() { funcTempl(takesBool) }

// OK: (Int32, Int32) -> Int32
func add(_ a: Int32, _ b: Int32) -> Int32 { return a + b }
funcTempl(add)

// OK: C++ func
funcTempl(get42)

// OK: (Int32, Int32) -> Int32 & C++ func
funcTempl2(add, get42)
funcTempl2(get42, add)

// BAD: (inout Int32) -> ()
func inoutFoo(_ a : inout Int32) { a += 1 }
funcTempl(inoutFoo)

// BAD: (Int32, inout Int32) -> ()
func inoutBar(_ a: Int32, _ b : inout Int32) { b += 1 }
funcTempl(inoutBar)

// BAD: (Int32, Int32) -> Int32 & (inout Int32) -> ()
funcTempl2(add, inoutFoo)
funcTempl2(inoutFoo, add)

// BAD: (Int32, Int32) -> Int32 & (Int32, inout Int32) -> ()
funcTempl2(add, inoutBar)
funcTempl2(inoutBar, add)

// OK: Int32 & inout Int32
funcTemplAndIntRef(x, &y)

// OK: () -> () & inout Int32
funcTemplAndIntRef({() in ()}, &x)

// BAD: (inout Int32) -> () & inout Int32
funcTemplAndIntRef(inoutFoo, &x)
