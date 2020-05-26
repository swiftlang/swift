// RUN: %sourcekitd-test -req=collect-type %s -req-opts=expectedtypes='s:8filtered4ProtP;s:8filtered5Prot1P' -- %s | %FileCheck %s -check-prefix=BOTH
// RUN: %sourcekitd-test -req=collect-type %s -req-opts=expectedtypes='s:8filtered5Prot1P' -- %s | %FileCheck %s -check-prefix=PROTO1
// RUN: %sourcekitd-test -req=collect-type %s -req-opts=expectedtypes='s:8filtered6Proto2P' -- %s | %FileCheck %s -check-prefix=PROTO2

protocol Prot {}

protocol Prot1 {}

class Clas: Prot {
  var value: Clas { return self }
  func getValue() -> Clas { return self }
}

struct Stru: Prot, Prot1 {
  var value: Stru { return self }
  func getValue() -> Stru { return self }
}

class C {}

func ArrayC(_ a: [C]) {
	_ = a.count
	_ = a.description.count.advanced(by: 1).description
	_ = a[0]
}

func ArrayClas(_ a: [Clas]) {
	_ = a[0].value.getValue().value
}

func ArrayClas(_ a: [Stru]) {
	_ = a[0].value.getValue().value
}

protocol Proto2 {}

class Proto2Conformer: Proto2 {}

func foo(_ c: Proto2Conformer) { _ = c }

// BOTH: <ExpressionTypes>
// BOTH: (503, 507): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (545, 549): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (609, 613): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (651, 655): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (811, 838): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (811, 832): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (811, 821): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (811, 815): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (877, 904): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (877, 898): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (877, 887): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (877, 881): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: </ExpressionTypes>

// PROTO1: <ExpressionTypes>
// PROTO1: (609, 613): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (651, 655): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (877, 904): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (877, 898): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (877, 887): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (877, 881): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: </ExpressionTypes>

// PROTO2: <ExpressionTypes>
// PROTO2: (999, 1000): Proto2Conformer
// PROTO2: conforming to: s:8filtered6Proto2P
// PROTO2: </ExpressionTypes>
