// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

import c_simd

// Ensure that the SIMD types from the imported module get mapped into the
// SIMD Swift module, even if we in the importing module don't import SIMD.

takes_float4(makes_float4())
takes_int3(makes_int3())
takes_double2(makes_double2())

// FIXME: float4 should not have been transitively imported.
let x: float4 = makes_float4() // fixme-error{{undeclared type 'float4'}}
