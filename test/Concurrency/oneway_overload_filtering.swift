// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -debug-constraints 2>%t.err
// RUN: %FileCheck %s < %t.err

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// The callee of a 'send' or of an 'await' is marked on its application
// constraint, and the solver attempts the '@oneway' overloads of the callee of
// a 'send' (or those which are not '@oneway' of the callee of an 'await')
// first. If one of them leads to a solution, the other overloads are never
// attempted. If none does, the other overloads are attempted next. See
// oneway_send_overload_filter.swift for which overload each call picks.

actor A {
  @oneway func tell(_ n: Int) {}
  func tell(_ n: Int) async {}
}

actor B {
  @oneway func tell(_ n: Int) async {}
  func tell(_ n: Int) {}
}

actor C {
  @oneway func set(_ s: String) {}
  func set(_ n: Int) {}

  @oneway func store<T: StringProtocol & Sendable>(_ v: T) {}
  func store(_ v: Int) {}
}

func make() -> Int { 0 }
func make() -> String { "" }

actor D {
  @oneway func tell(_ s: String) {}
  func tell(_ n: Int) {}
}

// The 'async' overload, which an async context would otherwise prefer, is not
// attempted
func sendAttemptsOnlyOneway(_ a: A) async {
  // CHECK: ---Constraint solving at [{{.*}}oneway_overload_filtering.swift:[[@LINE+5]]:
  // CHECK: applicable fn {{.*}} [send]
  // CHECK: (disjunction '{{\$T[0-9]+}}': active=2 disabled=0 {{.*}} score=100
  // CHECK: (attempting disjunction choice [[TELL:\$T[0-9]+]] bound to {{.*}}A.tell{{.*}} as A.(isolated A) -> (Int) -> () @
  // CHECK-NOT: (attempting disjunction choice [[TELL]] bound to {{.*}}A.tell{{.*}} as A.(isolated A) -> (Int) async -> () @
  send a.tell(1)
}

// The '@oneway' overload, which an async context would otherwise prefer, is
// not attempted
func awaitAttemptsOnlyNonOneway(_ b: B) async {
  // CHECK: ---Constraint solving at [{{.*}}oneway_overload_filtering.swift:[[@LINE+5]]:
  // CHECK: applicable fn {{.*}} [await]
  // CHECK: (disjunction '{{\$T[0-9]+}}': active=2 disabled=0 {{.*}} score=100
  // CHECK: (attempting disjunction choice [[TELL:\$T[0-9]+]] bound to {{.*}}B.tell{{.*}} as B.(isolated B) -> (Int) -> () @
  // CHECK-NOT: (attempting disjunction choice [[TELL]] bound to {{.*}}B.tell{{.*}} as B.(isolated B) -> (Int) async -> () @
  await b.tell(1)
}

// The '@oneway' overload can't take an integer literal, which is only known
// once it is attempted, so the other overload is attempted next
func sendFallsBackAfterLiteral(_ c: C) async {
  // CHECK: ---Constraint solving at [{{.*}}oneway_overload_filtering.swift:[[@LINE+4]]:
  // CHECK: applicable fn {{.*}} [send]
  // CHECK: (attempting disjunction choice [[SET:\$T[0-9]+]] bound to {{.*}}C.set{{.*}} as C.(isolated C) -> (String) -> () @
  // CHECK: (attempting disjunction choice [[SET]] bound to {{.*}}C.set{{.*}} as C.(isolated C) -> (Int) -> () @
  send c.set(1)
}

// The generic requirement of the '@oneway' overload isn't met
func sendFallsBackAfterRequirement(_ c: C) async {
  // CHECK: ---Constraint solving at [{{.*}}oneway_overload_filtering.swift:[[@LINE+4]]:
  // CHECK: applicable fn {{.*}} [send]
  // CHECK: (attempting disjunction choice [[STORE:\$T[0-9]+]] bound to {{.*}}C.store{{.*}} as C.<T where T : Sendable, T : StringProtocol> (isolated C) -> (T) -> () @
  // CHECK: (attempting disjunction choice [[STORE]] bound to {{.*}}C.store{{.*}} as C.(isolated C) -> (Int) -> () @
  send c.store(1)
}

// The callee is attempted before the overloaded argument. Otherwise both
// ('make() -> Int', 'tell(_: Int)') and ('make() -> String', '@oneway
// tell(_: String)') would be solutions, and the call would be ambiguous
func sendAttemptsCalleeBeforeOverloadedArgument(_ d: D) async {
  // CHECK: ---Constraint solving at [{{.*}}oneway_overload_filtering.swift:[[@LINE+6]]:
  // CHECK: applicable fn {{.*}} [send]
  // CHECK-NOT: (attempting disjunction choice {{.*}}.make()@
  // CHECK: (attempting disjunction choice [[TELL:\$T[0-9]+]] bound to {{.*}}D.tell{{.*}} as D.(isolated D) -> (String) -> () @
  // CHECK: (attempting disjunction choice {{.*}}.make()@{{.*}} as () -> String
  // CHECK-NOT: (attempting disjunction choice [[TELL]] bound to {{.*}}D.tell{{.*}} as D.(isolated D) -> (Int) -> () @
  send d.tell(make())
}

// Any other call is not marked
func plainCall(_ a: isolated A) {
  // CHECK: ---Constraint solving at [{{.*}}oneway_overload_filtering.swift:[[@LINE+2]]:
  // CHECK-NOT: applicable fn {{.*}} [{{send|await}}]
  a.tell(1)
}
