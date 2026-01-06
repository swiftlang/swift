// RUN: %target-typecheck-verify-swift

// Inspired by GRDB.swift.

protocol P {
  associatedtype A
  associatedtype B
  func f1(_: A)
  func f2(_: A)
  func f3(_: A)
  func f4(_: A)
  func f5(_: A)
  func f6(_: A)
  func f7(_: A)
  func f8(_: A)
  func f9(_: A)
  func f10(_: A)
  func f11(_: A)
  func f12(_: A)
  func f13(_: A)
  func f14(_: A)
  func f15(_: A)
  func f16(_: B)
  func f17(_: B)
  func f18(_: B)
  func f19(_: B)
  func f20(_: B)
  func f21(_: B)
  func f22(_: B)
  func f23(_: B)
  func f24(_: B)
  func f25(_: B)
  func f26(_: B)
  func f27(_: B)
  func f28(_: B)
  func f29(_: B)
  func f30(_: B)
  func f31(_: B)
  func f32(_: B)
}

extension P {
  func f1(_: A) {}
  func f2(_: A) {}
  func f3(_: A) {}
  func f4(_: A) {}
  func f5(_: A) {}
  func f6(_: A) {}
  func f7(_: A) {}
  func f8(_: A) {}
  func f9(_: A) {}
  func f10(_: A) {}
  func f11(_: A) {}
  func f12(_: A) {}
  func f13(_: A) {}
  func f14(_: A) {}
  func f15(_: A) {}
  func f16(_: A) {}
  func f17(_: A) {}
  func f18(_: A) {}
  func f19(_: A) {}
  func f20(_: A) {}
  func f21(_: A) {}
  func f22(_: A) {}
  func f23(_: A) {}
  func f24(_: A) {}
  func f25(_: A) {}
  func f26(_: A) {}
  func f27(_: A) {}
  func f28(_: A) {}
  func f29(_: A) {}
  func f30(_: A) {}
  func f31(_: A) {}
  func f32(_: A) {}
}

extension P {
  func f1(_: B) {}
  func f2(_: B) {}
  func f3(_: B) {}
  func f4(_: B) {}
  func f5(_: B) {}
  func f6(_: B) {}
  func f7(_: B) {}
  func f8(_: B) {}
  func f9(_: B) {}
  func f10(_: B) {}
  func f11(_: B) {}
  func f12(_: B) {}
  func f13(_: B) {}
  func f14(_: B) {}
  func f15(_: B) {}
  func f16(_: B) {}
  func f17(_: B) {}
  func f18(_: B) {}
  func f19(_: B) {}
  func f20(_: B) {}
  func f21(_: B) {}
  func f22(_: B) {}
  func f23(_: B) {}
  func f24(_: B) {}
  func f25(_: B) {}
  func f26(_: B) {}
  func f27(_: B) {}
  func f28(_: B) {}
  func f29(_: B) {}
  func f30(_: B) {}
  func f31(_: B) {}
  func f32(_: B) {}
}

struct S: P {
  func f1(_: Int) {}
  func f2(_: Int) {}
  func f3(_: Int) {}
  func f4(_: Int) {}
  func f5(_: Int) {}
  func f6(_: Int) {}
  func f7(_: Int) {}
  func f8(_: Int) {}
  func f9(_: Int) {}
  func f10(_: Int) {}
  func f11(_: Int) {}
  func f12(_: Int) {}
  func f13(_: Int) {}
  func f14(_: Int) {}
  func f15(_: Int) {}
  func f16(_: Int) {}
  func f17(_: Int) {}
  func f18(_: Int) {}
  func f19(_: Int) {}
  func f20(_: Int) {}
  func f21(_: Int) {}
  func f22(_: Int) {}
  func f23(_: Int) {}
  func f24(_: Int) {}
  func f25(_: Int) {}
  func f26(_: Int) {}
  func f27(_: Int) {}
  func f28(_: Int) {}
  func f29(_: Int) {}
  func f30(_: Int) {}
  func f31(_: Int) {}
  func f32(_: Int) {}
}
