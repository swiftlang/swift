// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// In a module with several source files, top-level code is only allowed in
// 'main.swift'. Explain this, once per file, when diagnosing top-level code.
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/a.swift %t/b.swift
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/stmt.swift %t/b.swift

// The same code is accepted once the file is named 'main.swift'.
// RUN: %target-swift-frontend -typecheck %t/main.swift %t/b.swift

//--- a.swift
print("hello") // expected-error {{expressions are not allowed at the top level}}
// expected-note@-1 {{top-level code is only allowed in 'main.swift' or a single-file module; use '@main' to define an entry point elsewhere}}
helper() // expected-error {{expressions are not allowed at the top level}}

// Global variables are declarations, not top-level code.
let x = 1

//--- stmt.swift
if true { helper() } // expected-error {{statements are not allowed at the top level}}
// expected-note@-1 {{top-level code is only allowed in 'main.swift' or a single-file module; use '@main' to define an entry point elsewhere}}

//--- main.swift
print("hello")
helper()

//--- b.swift
func helper() {}
