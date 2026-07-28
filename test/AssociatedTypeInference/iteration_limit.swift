// RUN: %target-typecheck-verify-swift

/// If this test case ever starts to be type checked successfully because of
/// optimizations in associated type inference, you will need to change it
/// in some way to make it too complex again. If you are unable to construct
/// a test case that still takes too long with your new optimization,
/// congratulations, you found a proof that P = NP.

protocol P {
  associatedtype A: Error
  // expected-note@-1 {{protocol requires nested type 'A'}}

  func f1() throws(A)
  func f2() throws(A)
  func f3() throws(A)
  func f4() throws(A)
  func f5() throws(A)
  func f6() throws(A)
  func f7() throws(A)
  func f8() throws(A)
  func f9() throws(A)
  func f10() throws(A)
  func f11() throws(A)
  func f12() throws(A)
  func f13() throws(A)
  func f14() throws(A)
  func f15() throws(A)
  func f16() throws(A)
  func f17() throws(A)
  func f18() throws(A)
  func f19() throws(A)
  func f20() throws(A)
  func f21() throws(A)
  func f22() throws(A)
  func f23() throws(A)
  func f24() throws(A)
  func f25() throws(A)
  func f26() throws(A)
  func f27() throws(A)
  func f28() throws(A)
  func f29() throws(A)
  func f30() throws(A)
  func f31() throws(A)
  func f32() throws(A)
}

extension P {
  func f1() throws(A) {}
  func f2() throws(A) {}
  func f3() throws(A) {}
  func f4() throws(A) {}
  func f5() throws(A) {}
  func f6() throws(A) {}
  func f7() throws(A) {}
  func f8() throws(A) {}
  func f9() throws(A) {}
  func f10() throws(A) {}
  func f11() throws(A) {}
  func f12() throws(A) {}
  func f13() throws(A) {}
  func f14() throws(A) {}
  func f15() throws(A) {}
  func f16() throws(A) {}
  func f17() throws(A) {}
  func f18() throws(A) {}
  func f19() throws(A) {}
  func f20() throws(A) {}
  func f21() throws(A) {}
  func f22() throws(A) {}
  func f23() throws(A) {}
  func f24() throws(A) {}
  func f25() throws(A) {}
  func f26() throws(A) {}
  func f27() throws(A) {}
  func f28() throws(A) {}
  func f29() throws(A) {}
  func f30() throws(A) {}
  func f31() throws(A) {}
  func f32() throws(A) {}
}

protocol Q {}

extension Q {
  func f1() throws(any Error) {}
  func f2() throws(any Error) {}
  func f3() throws(any Error) {}
  func f4() throws(any Error) {}
  func f5() throws(any Error) {}
  func f6() throws(any Error) {}
  func f7() throws(any Error) {}
  func f8() throws(any Error) {}
  func f9() throws(any Error) {}
  func f10() throws(any Error) {}
  func f11() throws(any Error) {}
  func f12() throws(any Error) {}
  func f13() throws(any Error) {}
  func f14() throws(any Error) {}
  func f15() throws(any Error) {}
  func f16() throws(any Error) {}
  func f17() throws(any Error) {}
  func f18() throws(any Error) {}
  func f19() throws(any Error) {}
  func f20() throws(any Error) {}
  func f21() throws(any Error) {}
  func f22() throws(any Error) {}
  func f23() throws(any Error) {}
  func f24() throws(any Error) {}
  func f25() throws(any Error) {}
  func f26() throws(any Error) {}
  func f27() throws(any Error) {}
  func f28() throws(any Error) {}
  func f29() throws(any Error) {}
  func f30() throws(any Error) {}
  func f31() throws(any Error) {}
  func f32() throws(any Error) {}
}

struct S: P, Q {}
// expected-error@-1 {{type 'S' does not conform to protocol 'P'}}
// expected-error@-2 {{the compiler is unable to infer the associated types in this conformance of 'S' to 'P' in reasonable time}}
// expected-note@-3 {{add stubs for conformance}}