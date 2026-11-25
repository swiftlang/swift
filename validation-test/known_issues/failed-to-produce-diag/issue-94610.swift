// rdar://193528515 (https://github.com/swiftlang/swift/issues/94610)
// RUN: %target-typecheck-verify-swift -language-mode 6 -enable-experimental-feature KeyPathWithMethodMembers

// REQUIRES: swift_feature_KeyPathWithMethodMembers

struct S {
  static func f(_ x: Int) -> Int { x }
}

let kp = \S.Type.f(0) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
let kp2 = \S.Type.f(0 as Int) // ok
