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
enum MyEnum {
    case foo, bar
}

class C : P {
  typealias Assoc = String
  static func staticMethod() -> Self {}
  func instanceMethod(x: MyEnum) -> C {}
  func methodForTarget1() -> ConcreteTarget1 {}
  func methodForTarget2() -> ConcreteTarget2 {}
}

func testing(obj: C) {
  let _ = obj.
}
func testing(obj: C) {
  let _ = obj.instanceMethod(x: )
}


// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=29:14 %s -- %s -module-name MyModule == \
// RUN:   -req=conformingmethods -pos=29:14 -req-opts=expectedtypes='$s8MyModule7Target2PD;$s8MyModule7Target1PD' %s -- %s -module-name MyModule == \
// RUN:   -req=typecontextinfo -pos=32:33 %s -- %s -module-name MyModule == \
// RUN:   -req=complete -pos=29:14 %s -- %s -module-name MyModule > %t.response
// RUN: %diff -u %s.response %t.response
