// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend %s -typecheck -verify -show-diagnostics-after-fatal
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_func.swift -module-name new_module
// RUN: %target-swift-frontend %s -typecheck -I %t

// These errors should happen before we've built the module to import.
import new_module // expected-error{{no such module 'new_module'}}

new_module.getZero() // expected-error {{use of unresolved identifier 'new_module'}}
