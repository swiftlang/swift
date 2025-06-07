// RUN: %batch-code-completion -disable-objc-attr-requires-foundation-module

struct FooStruct {
  init() {}
  init(a: Int) {}
  init(a: Int, b: Float) {}
  mutating func instanceFunc2(_ a: Int, b: inout Double) {}
}

func testInsideFunctionCall_1(_ x: inout FooStruct) {
  x.instanceFunc(#^BEFORE_COMMA^#,
// BEFORE_COMMA-NOT: Pattern/{{.*}}:{{.*}}({{.*}}{#Int#}
// BEFORE_COMMA-NOT: Decl[InstanceMethod]/{{.*}}:{{.*}}({{.*}}{#Int#}
}
func testInsideFunctionCall_2(_ x: inout FooStruct) {
  x.instanceFunc(#^BEFORE_PLACEHOLDER^#<#placeholder#>
// BEFORE_PLACEHOLDER-NOT: Pattern/{{.*}}:{{.*}}({{.*}}{#Int#}
// BEFORE_PLACEHOLDER-NOT: Decl[InstanceMethod]/{{.*}}:{{.*}}({{.*}}{#Int#}
}
func testConstructor() {
  FooStruct(#^CONSTRUCTOR^#,
// CONSTRUCTOR: Begin completions, 3 items
// CONSTRUCTOR-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['('][')'][#FooStruct#];
// CONSTRUCTOR-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#a: Int#}[')'][#FooStruct#];
// CONSTRUCTOR-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#a: Int#}, {#b: Float#}[')'][#FooStruct#];
}

func firstArg(arg1 arg1: Int, arg2: Int) {}
func testArg2Name3() {
  firstArg(#^LABELED_FIRSTARG^#,
// LABELED_FIRSTARG: Begin completions, 1 item
// LABELED_FIRSTARG-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]: ['(']{#arg1: Int#}, {#arg2: Int#}[')'][#Void#];
}

func optionalClosure(optClosure: ((Int) -> Void)?, someArg: Int) {
  optClosure?(#^OPTIONAL_CLOSURE^#someArg)
  // OPTIONAL_CLOSURE-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: someArg[#Int#]; name=someArg
}

func optionalProtocolMethod() {
  @objc protocol Foo {
    @objc optional func foo(arg: Int)
  }

  func test(foo: Foo) {
    foo.foo?(#^OPTIONAL_PROTOCOL_METHOD^#)
    // OPTIONAL_PROTOCOL_METHOD-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]: ['(']{#arg: Int#}[')'][#Void#];
  }
}

func subscriptAccess(info: [String: Int]) {
  info[#^SUBSCRIPT_ACCESS^#]
// SUBSCRIPT_ACCESS: Pattern/CurrNominal/Flair[ArgLabels]: ['[']{#keyPath: KeyPath<[String : Int], Value>#}[']'][#Value#];
}

struct StaticMethods {
  static func before() {
      self.after(num)#^AFTER_STATIC_FUNC^#
  }
  static func after(_ num: Int) -> (() -> Int) {}
// AFTER_STATIC_FUNC: Begin completions, 2 items
// AFTER_STATIC_FUNC-DAG: Keyword[self]/CurrNominal:          .self[#() -> Int#];
// AFTER_STATIC_FUNC-DAG: Pattern/CurrModule/Flair[ArgLabels]: ()[#Int#];
// AFTER_STATIC_FUNC: End completions
}

struct AmbiguousInResultBuilder {
  @resultBuilder
  struct MyViewBuilder {
    static func buildBlock(_ elt: Text) -> Int {
      53
    }
  }

  struct Text {
    init(verbatim content: String) {}
    init<S>(_ content: S) where S : StringProtocol {}
  }

  func foo(@MyViewBuilder content: () -> Int) {}

  func test(myStr: String) {
    foo {
      Text(#^AMBIGUOUS_IN_RESULT_BUILDER?xfail=TODO^#)
// AMBIGUOUS_IN_RESULT_BUILDER: Begin completions
// AMBIGUOUS_IN_RESULT_BUILDER-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#verbatim: String#}[')'][#Text#]; name=verbatim:
// AMBIGUOUS_IN_RESULT_BUILDER-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#(content): _#}[')'][#Text#]; name=:
// AMBIGUOUS_IN_RESULT_BUILDER-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: myStr[#String#]; name=myStr
// AMBIGUOUS_IN_RESULT_BUILDER: End completions
    }
  }
}
