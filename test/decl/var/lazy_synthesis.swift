// RUN: %target-swift-frontend -typecheck -dump-ast %s | %FileCheck %s

func compute() -> Bool {
  return true
}

struct Foo {
  // CHECK-DAG: pattern_named {{.*}} '$__lazy_storage_$_foo'
  // CHECK-DAG: var_decl {{.*}} "$__lazy_storage_$_foo" type='Bool?'
  // CHECK-DAG: pattern_named {{.*}} 'foo'
  // CHECK-DAG: var_decl {{.*}} "foo" type='Bool'
  lazy var foo = compute()
}

func foo() {
  // CHECK: pattern_named {{.*}} '$__lazy_storage_$_local'
  // CHECK: var_decl {{.*}} "$__lazy_storage_$_local" type='Bool?'
  // CHECK: pattern_named {{.*}} 'local'
  // CHECK: var_decl {{.*}} "local" type='Bool'
  lazy var local = compute()
}
