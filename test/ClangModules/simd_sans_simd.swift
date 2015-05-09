// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -enable-simd-import %s

import c_simd

// Ensure that the SIMD types from the imported module get mapped into the
// SIMD Swift module, even if we in the importing module don't import SIMD.

takes_float4(makes_float4())
takes_int3(makes_int3())
takes_double2(makes_double2())

// Float4 should not have been transitively imported.
let x: Float4 = makes_float4() // expected-error{{undeclared type 'Float4'}}
