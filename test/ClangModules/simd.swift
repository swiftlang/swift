// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -enable-simd-import %s

import c_simd
import SIMD

let f4: Float4 = makes_float4()
let i3: Int3 = makes_int3()
let d2: Double2 = makes_double2()

takes_float4(f4)
takes_int3(i3)
takes_double2(d2)

// byte17 isn't available since there isn't a type in the SIMD module to map it to.

let mb17 = makes_byte17 // expected-error{{unresolved identifier 'makes_byte17'}}
let t17 = takes_byte17  // expected-error{{unresolved identifier 'takes_byte17'}}
