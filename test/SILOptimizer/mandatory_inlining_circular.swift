// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify

@_transparent func waldo(_ x: Double) -> Double {
  return fred(x); // expected-error {{inlining 'transparent' functions forms circular loop}} expected-note 1 {{while inlining here}}
}

@_transparent func fred(_ x: Double) -> Double {
  return waldo(x); // expected-error {{inlining 'transparent' functions forms circular loop}} expected-note 1 {{while inlining here}}
}

