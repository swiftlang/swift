// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %s -parse -verify -show-diagnostics-after-fatal
// RUN: %swift -emit-module -o %t %S/Inputs/def_func.swift -module-name new_module
// RUN: %swift %s -parse -I=%t

// These errors should happen before we've built the module to import.
import new_module // expected-error{{no such module 'new_module'}}

new_module.getZero() // expected-error {{use of unresolved identifier 'new_module'}}
