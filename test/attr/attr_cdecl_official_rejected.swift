// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s \
// RUN:   -enable-experimental-feature CDecl

// REQUIRES: swift_feature_CDecl

@cdecl("cdecl_foo") func foo() { } // expected-error {{'@cdecl' requires '-enable-experimental-feature CDecl'}}

var computed: Int {
  @cdecl("get_computed") get { return 0 } // expected-error {{'@cdecl' requires '-enable-experimental-feature CDecl'}}
  @cdecl("set_computed") set { } // expected-error {{'@cdecl' requires '-enable-experimental-feature CDecl'}}
}
