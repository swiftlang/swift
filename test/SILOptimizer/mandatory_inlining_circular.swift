// RUN: %target-swift-frontend -sil-verify-all -emit-sil %s -o /dev/null -verify
// RUN: %target-swift-frontend -sil-verify-all -emit-sil %s -o /dev/null -verify -enable-ownership-stripping-after-serialization

@_transparent func waldo(_ x: Double) -> Double {
  return fred(x);
}

@_transparent func fred(_ x: Double) -> Double { // expected-warning {{all paths through this function will call itself}}
  return waldo(x); // expected-error {{inlining 'transparent' functions forms circular loop}}
}
