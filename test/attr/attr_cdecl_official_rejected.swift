// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s

@c(cdecl_foo) func foo() { }

var computed: Int {
  @c(get_computed) get { return 0 }
  @c(set_computed) set { }
}
