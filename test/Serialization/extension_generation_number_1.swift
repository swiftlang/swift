// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/extension_generation_number.swift
// RUN: %target-swift-frontend -typecheck -I %t %s

// REQUIRES: objc_interop

import extension_generation_number
import Foundation

func foo(x: NSString) {
  x.f()
}
