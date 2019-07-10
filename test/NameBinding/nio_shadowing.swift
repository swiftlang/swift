// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/NIOFoundationCompat.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -verify

// REQUIRES: objc_interop
import Foundation
import NIOFoundationCompat

func test(data: Data) {
  data.withUnsafeBytes { x in print(x) }
}
