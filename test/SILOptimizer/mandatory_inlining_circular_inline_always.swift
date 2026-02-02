// RUN: %target-swift-frontend -enable-experimental-feature InlineAlways -sil-verify-all -emit-sil %s -o /dev/null -verify

// REQUIRES: swift_feature_InlineAlways

@inline(always) func mutally(_ x: Double) -> Double {
  return recursive(x); // expected-error {{inlining '@inline(always)' functions forms circular loop}} expected-note 1 {{while inlining here}}
}

@inline(always) func recursive(_ x: Double) -> Double {
  return mutally(x); // expected-error {{inlining '@inline(always)' functions forms circular loop}} expected-note 1 {{while inlining here}}
}

