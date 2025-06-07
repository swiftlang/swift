// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD_INT -swift-version=5 | %FileCheck %s -check-prefix=OVERLOAD_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD_INTLITERAL -swift-version=5 | %FileCheck %s -check-prefix=OVERLOAD_INTLITERAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD_FLT -swift-version=5 | %FileCheck %s -check-prefix=OVERLOAD_FLT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD_FLTLITERAL -swift-version=5 | %FileCheck %s -check-prefix=OVERLOAD_FLT

struct Messenger {
  init() {}
  func send(_ msg: Message) {}
}

struct Message : ExpressibleByStringInterpolation, ExpressibleByStringLiteral {
  init(stringInterpolation: MsgInterpolation) {}
  init(stringLiteral: String) {}
}

struct MsgInterpolation: StringInterpolationProtocol {
  init() {}
  init(literalCapacity: Int, interpolationCount: Int) {}
  mutating func appendLiteral(_ literal: String) {}

  enum IntFormat {
    case decimal, hex
  }
  struct FloatFormat {
    private init() {}
    static func precision(_: Int) -> FloatFormat { fatalError() }
    static var hex: FloatFormat { fatalError() }
  }
  mutating func appendInterpolation(_ value: @autoclosure () -> Int, format: IntFormat = .decimal) {}
  mutating func appendInterpolation(_ value: @autoclosure () -> Float, format: FloatFormat = .hex) {}
}

var messenger = Messenger()
func testMessenger(intVal: Int, fltVal: Float) {
  messenger.send("  \(intVal, format: .#^OVERLOAD_INT^#) ")
// OVERLOAD_INT: Begin completions, 3 items
// OVERLOAD_INT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: decimal[#MsgInterpolation.IntFormat#];
// OVERLOAD_INT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: hex[#MsgInterpolation.IntFormat#];
// OVERLOAD_INT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): MsgInterpolation.IntFormat#})[#(into: inout Hasher) -> Void#];

  messenger.send("  \(5, format: .#^OVERLOAD_INTLITERAL^#, extraneousArg: 10) ")
// OVERLOAD_INTLITERAL: Begin completions, 5 items
// OVERLOAD_INTLITERAL-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: decimal[#MsgInterpolation.IntFormat#];
// OVERLOAD_INTLITERAL-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: hex[#MsgInterpolation.IntFormat#];
// OVERLOAD_INTLITERAL-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): MsgInterpolation.IntFormat#})[#(into: inout Hasher) -> Void#];
// OVERLOAD_INTLITERAL-DAG: Decl[StaticMethod]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: precision({#Int#})[#MsgInterpolation.FloatFormat#];
// OVERLOAD_INTLITERAL-DAG: Decl[StaticVar]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: hex[#MsgInterpolation.FloatFormat#];

  messenger.send("  \(fltVal, format: .#^OVERLOAD_FLT^#) ")
  messenger.send("  \(5.0, format: .#^OVERLOAD_FLTLITERAL^#) ")
// OVERLOAD_FLT: Begin completions, 2 items
// OVERLOAD_FLT-DAG: Decl[StaticMethod]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: precision({#Int#})[#MsgInterpolation.FloatFormat#];
// OVERLOAD_FLT-DAG: Decl[StaticVar]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: hex[#MsgInterpolation.FloatFormat#];
}
