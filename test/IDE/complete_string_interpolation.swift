// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD_INT -swift-version=5 | %FileCheck %s -check-prefix=OVERLOAD_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOAD_FLT -swift-version=5 | %FileCheck %s -check-prefix=OVERLOAD_FLT

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
// OVERLOAD_INT: Begin completions, 5 items
// OVERLOAD_INT-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: decimal[#MsgInterpolation.IntFormat#];
// OVERLOAD_INT-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: hex[#MsgInterpolation.IntFormat#];
// OVERLOAD_INT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): MsgInterpolation.IntFormat#})[#(into: inout Hasher) -> Void#];
// OVERLOAD_INT-DAG: Decl[StaticMethod]/CurrNominal/TypeRelation[Identical]: precision({#Int#})[#MsgInterpolation.FloatFormat#];
// OVERLOAD_INT-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: hex[#MsgInterpolation.FloatFormat#];
// OVERLOAD_INT: End completions

  messenger.send("  \(fltVal, format: .#^OVERLOAD_FLT^#) ")
// OVERLOAD_FLT: Begin completions, 5 items
// OVERLOAD_FLT-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: decimal[#MsgInterpolation.IntFormat#];
// OVERLOAD_FLT-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: hex[#MsgInterpolation.IntFormat#];
// OVERLOAD_FLT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): MsgInterpolation.IntFormat#})[#(into: inout Hasher) -> Void#];
// OVERLOAD_FLT-DAG: Decl[StaticMethod]/ExprSpecific/TypeRelation[Identical]: precision({#Int#})[#MsgInterpolation.FloatFormat#];
// OVERLOAD_FLT-DAG: Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: hex[#MsgInterpolation.FloatFormat#];
// OVERLOAD_FLT: End completions
}
