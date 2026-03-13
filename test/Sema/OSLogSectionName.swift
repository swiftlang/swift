// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend -typecheck -module-name OSLog %t/src/missing.swift 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -verify -module-name OSLog %t/src/noinit.swift
// RUN: %target-swift-frontend -typecheck -verify -module-name OSLog %t/src/badinit.swift
// RUN: %target-swift-frontend -typecheck -verify -module-name OSLog %t/src/goodinit.swift

//--- missing.swift

// CHECK: global variable 'osLogStringSectionName' is missing from the OSLog module; defaulting to '__TEXT,__oslogstring,cstring_literals'

//--- noinit.swift

// expected-error@+1{{global variable 'osLogStringSectionName' requires a string literal initializer}}
var osLogStringSectionName: String {
  "hello"
}

//--- badinit.swift

func getSection() -> String { "hello" }

// expected-error@+1{{global variable 'osLogStringSectionName' requires a string literal initializer}}
let osLogStringSectionName: String = getSection()

//--- goodinit.swift
let osLogStringSectionName = "hello"
