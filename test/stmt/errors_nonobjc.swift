// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name Foundation -o %t/Foundation.swiftmodule %S/Inputs/Foundation-with-NSError.swift
// RUN: %target-swift-frontend -I %t -typecheck -verify %s
// UNSUPPORTED: objc_interop

import Foundation

// Since we enabled bridging on non-ObjC platforms, NSError ought to be treated as exhaustive.

func bar() throws {}

func foo() {
  do {
    try bar()
  } catch _ as NSError {
  }
}
