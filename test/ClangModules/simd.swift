// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name main -parse -verify %s

import c_simd
import simd

let f4: float4 = makes_float4()
let i3: int3 = makes_int3()
let d2: double2 = makes_double2()

takes_float4(f4)
takes_int3(i3)
takes_double2(d2)

// byte17 isn't available since there isn't a type in the simd module to map it to.

let b17 = makes_byte17 // expected-error{{unresolved identifier 'makes_byte17'}}
takes_byte17(b17)  // expected-error{{unresolved identifier 'takes_byte17'}}
