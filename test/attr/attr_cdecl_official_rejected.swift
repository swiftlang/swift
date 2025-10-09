// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s \
// RUN:   -enable-experimental-feature CDecl

// REQUIRES: swift_feature_CDecl

@c("cdecl_foo") func foo() { } // expected-error {{'@c' requires '-enable-experimental-feature CDecl'}}

var computed: Int {
  @c("get_computed") get { return 0 } // expected-error {{'@c' requires '-enable-experimental-feature CDecl'}}
  @c("set_computed") set { } // expected-error {{'@c' requires '-enable-experimental-feature CDecl'}}
}
