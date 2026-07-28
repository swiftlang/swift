// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// This test guards against a compiler crash that occurred when importing
// a Swift-generated C++ module in which a method returns a Swift type whose
// definition isn't reachable (only forward-declared) at the import site.

// RUN: %target-swift-frontend -typecheck %t/use.swift \
// RUN:   -I %t/Inputs -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking

//--- Inputs/module.modulemap
module CxxModule {
    header "CxxModule.h"
    requires cplusplus
}

//--- Inputs/CxxModule.h
#pragma once

// Mimic the `external_source_symbol` annotation that the Swift->C++ header
// generator applies to C++ declarations representing Swift declarations.
#if __has_attribute(external_source_symbol)
#define SWIFT_SYMBOL(usrValue)                                                 \
  __attribute__((external_source_symbol(language = "Swift",                    \
                                        defined_in = "SwiftModule",            \
                                        generated_declaration, USR = usrValue)))
#else
#define SWIFT_SYMBOL(usrValue)
#endif

// A type that is annotated as a Swift class type but whose definition is
// unreachable here: it is only forward-declared.
class SWIFT_SYMBOL("s:11SwiftModule10SwiftClassC") SwiftClass;

class Container {
public:
  SwiftClass getSC();
};

//--- use.swift
import CxxModule
