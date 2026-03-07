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
// expected-error@+6 4 {{could not substitute parameter(s) for C++ function template 'takesValue' with Swift closure of '() -> ()' type; consider using 'std::function' as the C++ function parameter}}
// expected-error@+5   {{could not substitute parameter(s) for C++ function template 'takesValue' with Swift closure of '() -> Bool' type; consider using 'std::function' as the C++ function parameter}}
// expected-error@+4 2 {{could not substitute parameter(s) for C++ function template 'takesValue' with Swift closure of '(Bool) -> ()' type; consider using 'std::function' as the C++ function parameter}}
// expected-error@+3   {{could not substitute parameter(s) for C++ function template 'takesValue' with Swift closure of '(Int32, Int32) -> Int32' type; consider using 'std::function' as the C++ function parameter}}
// expected-error@+2   {{could not substitute parameter(s) for C++ function template 'takesValue' with Swift closure of '(inout Int32) -> ()' type; consider using 'std::function' as the C++ function parameter}}
// expected-error@+1   {{could not substitute parameter(s) for C++ function template 'takesValue' with Swift closure of '() -> Int32' type; consider using 'std::function' as the C++ function parameter}}
template <class T> void takesValue(T value) { }

int get42(void) { return 42; }

//--- main.swift
import CxxHeader

func takesTrue() { takesValue(true) }
func takesBool(x: Bool) { takesValue(x) }

// () -> ()
func takesRecursively() { takesValue(takesRecursively) }
func takesRecursiveClosure() { takesValue({() in takesRecursiveClosure()}) }
func takesSwiftClosure() { takesValue({() in ()}) }
func takesTakesTrue() { takesValue(takesTrue) }

// () -> Bool
func takesSwiftClosureReturningBool() { takesValue({() -> Bool in true}) }
// (Bool) -> ()
func takesSwiftClosureTakingBool() { takesValue({(x: Bool) in ()}) }
func takesTakesBool() { takesValue(takesBool) }

// (Int32, Int32) -> Int32
func add(_ a: Int32, _ b: Int32) -> Int32 { return a + b }
takesValue(add)

// Make sure it doesn't crash when a param is inout
func callbackInout(_ v : inout Int32) { v += 1 }
takesValue(callbackInout)

// C++ func
takesValue(get42)
