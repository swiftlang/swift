// RUN: %target-swift-frontend %clang-importer-sdk -enable-union-import -parse -verify %s

// XFAIL: linux

import ctypes

func useUnion(vec: GLKVector4) -> GLKVector4 {
  // TODO: Make union fields available
  let x: CFloat = vec.x // expected-error{{}}
  let y: CFloat = vec.y // expected-error{{}}
  let z: CFloat = vec.z // expected-error{{}}
  let w: CFloat = vec.w // expected-error{{}}

  let r: CFloat = vec.r // expected-error{{}}
  let g: CFloat = vec.g // expected-error{{}}
  let b: CFloat = vec.b // expected-error{{}}
  let a: CFloat = vec.a // expected-error{{}}

  let s: CFloat = vec.s // expected-error{{}}
  let t: CFloat = vec.t // expected-error{{}}
  let p: CFloat = vec.p // expected-error{{}}
  let q: CFloat = vec.q // expected-error{{}}

  let v0: CFloat = vec.v.0 // expected-error{{}}
  let v1: CFloat = vec.v.1 // expected-error{{}}
  let v2: CFloat = vec.v.2 // expected-error{{}}
  let v3: CFloat = vec.v.3 // expected-error{{}}
  return vec
}

func useStructWithAnonymousUnion(u: AnonUnion) -> AnonUnion {
  // TODO: Make union fields available
  let a: CFloat = u.a // expected-error{{}}
  let b: CFloat = u.b // expected-error{{}}
  let c: CFloat = u.c // expected-error{{}}
  let d: CFloat = u.d // expected-error{{}}
  let x: CInt = u.x
  return u
}

func useStructWithBitfields(mrm: ModRM) -> ModRM {
  // TODO: Make bitfield fields available
  let rm: CUnsignedInt = mrm.rm // expected-error{{}}
  let reg: CUnsignedInt = mrm.reg // expected-error{{}}
  let mod: CUnsignedInt = mrm.mod // expected-error{{}}
  let opcode: CUnsignedInt = mrm.opcode
  return mrm
}
