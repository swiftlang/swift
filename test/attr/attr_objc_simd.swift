// RUN: rm -rf %t
// RUN: mkdir %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/simd.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -I %t -parse -verify %s

// REQUIRES: objc_interop

import Foundation
import simd

@objc class Foo: NSObject {
  @objc func doStuffWithFloat4(x: Float.Vector4) -> Float.Vector4 { return x }
  @objc func doStuffWithDouble2(x: Double.Vector2) -> Double.Vector2 { return x }
  @objc func doStuffWithInt3(x: Int32.Vector3) -> Int32.Vector3 { return x }
}

