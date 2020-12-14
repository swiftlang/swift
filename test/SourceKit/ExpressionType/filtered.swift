// RUN lines at the end - offset sensitive
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
// BOTH: (126, 130): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (168, 172): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (232, 236): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (274, 278): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (434, 461): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (434, 455): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (434, 444): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (434, 438): Clas
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: (500, 527): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (500, 521): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (500, 510): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: (500, 504): Stru
// BOTH: conforming to: s:8filtered4ProtP
// BOTH: conforming to: s:8filtered5Prot1P
// BOTH: </ExpressionTypes>

// PROTO1: <ExpressionTypes>
// PROTO1: (232, 236): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (274, 278): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (500, 527): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (500, 521): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (500, 510): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: (500, 504): Stru
// PROTO1: conforming to: s:8filtered5Prot1P
// PROTO1: </ExpressionTypes>

// PROTO2: <ExpressionTypes>
// PROTO2: (622, 623): Proto2Conformer
// PROTO2: conforming to: s:8filtered6Proto2P
// PROTO2: </ExpressionTypes>

// RUN: %sourcekitd-test -req=collect-type %s -req-opts=expectedtypes='[s:8filtered4ProtP;s:8filtered5Prot1P]' -- %s | %FileCheck %s -check-prefix=BOTH
// RUN: %sourcekitd-test -req=collect-type %s -req-opts=expectedtypes='[s:8filtered5Prot1P]' -- %s | %FileCheck %s -check-prefix=PROTO1
// RUN: %sourcekitd-test -req=collect-type %s -req-opts=expectedtypes='[s:8filtered6Proto2P]' -- %s | %FileCheck %s -check-prefix=PROTO2