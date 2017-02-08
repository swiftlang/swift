// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

import ctypes

func useStructWithUnion(_ vec: GLKVector4) -> GLKVector4 {
  var vec = vec
  _ = vec.v.0
  _ = vec.v.1
  _ = vec.v.2
  _ = vec.v.3

  vec.v = (0, 0, 0, 0)
}

func useUnionIndirectFields(_ vec: GLKVector4) -> GLKVector4 {
  let _: CFloat = vec.x
  let _: CFloat = vec.y
  let _: CFloat = vec.z
  let _: CFloat = vec.w

  let _: CFloat = vec.r
  let _: CFloat = vec.g
  let _: CFloat = vec.b
  let _: CFloat = vec.a

  let _: CFloat = vec.s
  let _: CFloat = vec.t
  let _: CFloat = vec.p
  let _: CFloat = vec.q

  // Named indirect fields
  let _: CFloat = vec.v.0
  let _: CFloat = vec.v.1
  let _: CFloat = vec.v.2
  let _: CFloat = vec.v.3
  return vec
}

func useStructWithNamedUnion(_ u: NamedUnion) -> NamedUnion {
  var u1 = NamedUnion()
  u1.a = u.a
  u1.b = u.b
  u1.intfloat = u.intfloat
  return u1
}

func useStructWithAnonymousUnion(_ u: AnonUnion) -> AnonUnion {
  let _: CFloat = u.a
  let _: CFloat = u.b
  let _: CFloat = u.c
  let _: CFloat = u.d
  let _: CInt = u.x
  return u
}

func useStructWithUnnamedUnion(_ u: UnnamedUnion) -> UnnamedUnion {
  var u = u
  u.u.i = 100
  u.u.f = 1.0
}
