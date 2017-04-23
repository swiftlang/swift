// Try with and without whole module optimization

// RUN: %target-build-swift %S/library.swift %S/main.swift
// RUN: %target-build-swift -whole-module-optimization %S/library.swift %S/main.swift

// REQUIRES: executable_test
// REQUIRES: objc_interop

import CoreGraphics

// Case 1 - witness is imported accessor

protocol MyPoint {
  associatedtype FloatType

  var x: FloatType { get set }
  var y: FloatType { get set }
}

extension CGPoint: MyPoint {}

// Case 2 - witness is extension method of imported type

protocol MyProto {
  var z: Float { get set }
}

extension CGPoint : MyProto {}

// Dummy statement
_ = ()
