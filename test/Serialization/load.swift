// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -verify -show-diagnostics-after-fatal -swift-version 3
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_func.swift -module-name new_module -swift-version 3
// RUN: %target-swift-frontend %s -typecheck -I %t -swift-version 3

// These errors should happen before we've built the module to import.
import new_module // expected-error{{no such module 'new_module'}}

new_module.getZero() // expected-error {{use of unresolved identifier 'new_module'}}
