// Try with and without whole module optimization

// RUN: %target-build-swift %S/Inputs/library.swift %S/main.swift
// RUN: %target-build-swift -whole-module-optimization %S/Inputs/library.swift %S/main.swift

// REQUIRES: objc_interop

import CoreGraphics

protocol MyPoint {
  associatedtype FloatType

  var x: FloatType { get set }
  var y: FloatType { get set }
}

extension CGPoint: MyPoint {}

// Dummy statement
_ = ()
