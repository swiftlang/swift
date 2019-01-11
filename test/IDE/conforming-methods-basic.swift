// RUN: %target-swift-ide-test -conforming-methods -source-filename %s -code-completion-token=TEST_1 -module-name MyModule -conforming-methods-expected-types '$s8MyModule7Target1PD' -conforming-methods-expected-types '$s8MyModule7Target2PD' | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -conforming-methods -source-filename %s -code-completion-token=TEST_2 -module-name MyModule -conforming-methods-expected-types '$s8MyModule7Target1PD' -conforming-methods-expected-types '$s8MyModule7Target2PD' | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -conforming-methods -source-filename %s -code-completion-token=TEST_3 -module-name MyModule -conforming-methods-expected-types '$s8MyModule7Target1PD' -conforming-methods-expected-types '$s8MyModule7Target2PD' | %FileCheck %s -check-prefix=CHECK_1
// RUN: %target-swift-ide-test -conforming-methods -source-filename %s -code-completion-token=TEST_4 -module-name MyModule -conforming-methods-expected-types '$s8MyModule7Target1PD' -conforming-methods-expected-types '$s8MyModule7Target2PD' | %FileCheck %s -check-prefix=CHECK_1

protocol Target1 {
  associatedType Assoc
}
protocol Target2 {}
protocol Target3 {}

struct ConcreteTarget1 : Target1 {}
struct ConcreteTarget2 : Target2 {}
struct ConcreteTarget3 : Target3 {}


struct C {
  func returnsConcreteTarget1() -> ConcreteTarget1 { fatalError() }
  func returnsExistentialTarget1() -> Target1 { fatalError() }
}

protocol P {
  func returnsConcreteTarget2() -> ConcreteTarget2
  func returnsConcreteTarget3() -> ConcreteTarget3
}

extension P {
  func returnSelf() -> Self { return self }
  func returnsConcreteTarget2() -> ConcreteTarget2 { fatalError() }
  func returnsConcreteTarget3() -> ConcreteTarget3 { fatalError() }
}

extension C : P {}

func testing(obj: C) {
  let _ = obj #^TEST_1^#
  let _ = obj .#^TEST_2^#
  let _ = obj.returnSelf()#^TEST_3^#
  let _ = obj.returnSelf().#^TEST_4^#
}

// CHECK_1:      -----BEGIN CONFORMING METHOD LIST-----
// CHECK_1_NEXT: - TypeName: C
// CHECK_1_NEXT: - Members:
// CHECK_1_NEXT:    - Name: returnsConcreteTarget1()
// CHECK_1_NEXT:      TypeName: ConcreteTarget1
// CHECK_1_NEXT:    - Name: returnsConcreteTarget1()
// CHECK_1_NEXT:      TypeName: ConcreteTarget1
// CHECK_1_NEXT: -----END CONFORMING METHOD LIST-----
