// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify

@transparent func waldo(x: Double) -> Double {
  return fred(x); // expected-error {{inlining 'transparent' functions forms circular loop}} expected-note 1 {{while inlining here}}
}

@transparent func fred(x: Double) -> Double {
  return waldo(x); // expected-error {{inlining 'transparent' functions forms circular loop}} expected-note 1 {{while inlining here}}
}

