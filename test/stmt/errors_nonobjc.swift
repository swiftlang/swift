// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name Foundation -o %t/Foundation.swiftmodule %S/Inputs/Foundation-with-NSError.swift
// RUN: %target-swift-frontend -I %t -typecheck -verify %s
// UNSUPPORTED: objc_interop

import Foundation

// Catching `as NSError` ought *not* to be exhaustive when ObjC interop is
// disabled. It's just another error type.

func bar() throws {}

func foo() {
  do {
    try bar() // expected-error{{enclosing catch is not exhaustive}}
  } catch _ as NSError {
  }
}
