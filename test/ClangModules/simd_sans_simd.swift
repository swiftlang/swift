// RUN: rm -rf %t
// RUN: mkdir %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift
// RUN:  %target-swift-frontend(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -emit-module -o %t  %S/../Inputs/clang-importer-sdk/swift-modules/simd.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -I %t -module-name main -parse -verify -enable-simd-import %s

import c_simd

// Ensure that the SIMD types from the imported module get mapped into the
// SIMD Swift module, even if we in the importing module don't import SIMD.

takes_float4(makes_float4())
takes_int3(makes_int3())
takes_double2(makes_double2())

// FIXME: Float.Vector4 should not have been transitively imported.
// rdar://problem/20787803
// let x: Float.Vector4 = makes_float4() // expected-fixme-error{{'Vector4' is not a member type of 'Float'}}
