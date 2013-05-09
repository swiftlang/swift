// RUN: %swift %s -parse -verify
// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/new_module.sm %S/empty.swift
// RUN: %swift %s -parse -I=%t

// These errors should happen before we've built the module to import.
import new_module // expected-error{{no such module 'new_module'}}

new_module // expected-error{{use of unresolved identifier 'new_module'}}
