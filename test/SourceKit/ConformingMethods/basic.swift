protocol Target1 {}
protocol Target2 {}
protocol Target3 {}

struct ConcreteTarget1 : Target1 {}
struct ConcreteTarget2 : Target2 {}
struct ConcreteTarget3 : Target3 {}

protocol P {
  associatedtype Assoc
  func protocolMethod(asc: Assoc) -> Self
}
extension P {
  func protocolMethod(asc: Assoc) -> Self { return self }
}

class C : P {
  typealias Assoc = String
  static func staticMethod() -> Self {}
  func instanceMethod(x: Int) -> C {}
  func methodForTarget1() -> ConcreteTarget1 {}
  func methodForTarget2() -> ConcreteTarget2 {}
}

func testing(obj: C) {
  let _ = obj.
}

// RUN: %sourcekitd-test -req=conformingmethods -pos=26:14 -repeat-request=2 %s -req-opts=expectedtypes='$s8MyModule7Target2PD;$s8MyModule7Target1PD' -- -module-name MyModule %s > %t.response
// RUN: %diff -u %s.response %t.response
// RUN: %sourcekitd-test -req=conformingmethods -pos=26:14 -repeat-request=2 %s -req-opts=expectedtypes='$s8MyModule7Target2PD;$s8MyModule7Target1PD',reuseastcontext=0 -- -module-name MyModule %s | %FileCheck %s --check-prefix=DISABLED

// DISABLED-NOT: key.reuseastcontext
// DISABLED: key.members: [
// DISABLED-NOT: key.reuseastcontext
// DISABLED: key.members: [
// DISABLED-NOT: key.reuseastcontext
