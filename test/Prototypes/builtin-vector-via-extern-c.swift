//===----------------------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_feature_BuiltinModule
// RUN: %target-swift-frontend -primary-file %s -enable-experimental-feature BuiltinModule -enable-experimental-feature Extern -emit-ir | %FileCheck %s --check-prefix=CHECK

import Builtin

@_extern(c, "llvm.uadd.sat.v16i8") @usableFromInline
func _uaddSat(_ a: Builtin.Vec16xInt8, _ b: Builtin.Vec16xInt8) -> Builtin.Vec16xInt8

@_transparent
public func saturatingAdd(_ a: SIMD16<UInt8>, _ b: SIMD16<UInt8>) -> SIMD16<UInt8> {
  // Hack around init from Builtin type being stdlib-internal using unsafeBitCast.
  unsafeBitCast(_uaddSat(a._storage._value, b._storage._value), to: SIMD16<UInt8>.self)
}
// CHECK: saturatingAdd{{.*}} {
// CHECK: entry:
// CHECK: call <16 x i8> @llvm.uadd.sat.v16i8

