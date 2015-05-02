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
import simd

let f4: Float.Vector4 = makes_float4()
let i3: Int32.Vector3 = makes_int3()
let d2: Double.Vector2 = makes_double2()

takes_float4(f4)
takes_int3(i3)
takes_double2(d2)

// byte17 isn't available since there isn't a type in the simd module to map it to.

let b17 = makes_byte17 // expected-error{{unresolved identifier 'makes_byte17'}}
takes_byte17(b17)  // expected-error{{unresolved identifier 'takes_byte17'}}

