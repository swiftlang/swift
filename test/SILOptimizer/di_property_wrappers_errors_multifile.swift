// rdar://58495602: The order used to matter here. If use compiles before def
// then you got a DI error. If def compiles before use then you got a linker error.

// RUN: %target-swift-frontend -emit-sil -verify %s %S/Inputs/di_property_wrappers_errors_multifile_2.swift
// RUN: %target-swift-frontend -emit-sil -verify %S/Inputs/di_property_wrappers_errors_multifile_2.swift %s

let contentView = ContentView() // expected-error {{missing argument for parameter 'runner' in call}}

