// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-module-interface(%t/A.swiftinterface) -module-name A %t/a.swift
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -I %t

//--- a.swift
public let foo = 0

//--- main.swift
import A

// Make sure we still diagnose this even if there's an imported result available.
_ = foo // expected-error {{use of global variable 'foo' before its declaration}}
let foo = 0 // expected-note {{'foo' declared here}}
