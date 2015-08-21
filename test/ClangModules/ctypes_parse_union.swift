// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

import ctypes

func useStructWithUnion(var vec: GLKVector4) -> GLKVector4 {
  _ = vec.v.0
  _ = vec.v.1
  _ = vec.v.2
  _ = vec.v.3

  vec.v = (0, 0, 0, 0)
}

func useUnionIndirectFields(vec: GLKVector4) -> GLKVector4 {
  // TODO: Make indirect fields from anonymous structs in unions
  // accessible.
  // Anonymous indirect fields
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

  // Named indirect fields
  let v0: CFloat = vec.v.0
  let v1: CFloat = vec.v.1
  let v2: CFloat = vec.v.2
  let v3: CFloat = vec.v.3
  return vec
}

func useStructWithNamedUnion(u: NamedUnion) -> NamedUnion {
  var u1 = NamedUnion()
  u1.a = u.a
  u1.b = u.b
  u1.intfloat = u.intfloat
  return u1
}

func useStructWithAnonymousUnion(u: AnonUnion) -> AnonUnion {
  // TODO: Make union indirect fields from anonymous structs in unions
  // accessible.
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

// Incompletely imported structs shouldn't have elementwise initializers.
// They can still be zero-initialized using the default initializer.
func constructStructWithBitfields(x: CUnsignedInt) {
  _ = StructWithBitfields() as StructWithBitfields
  _ = StructWithBitfields(First: x) as StructWithBitfields// expected-error{{}}
  // TODO: Fully import bitfields.
  _ = StructWithBitfields(First: x, Second: x, Third: x) as StructWithBitfields// expected-error{{}}
}
