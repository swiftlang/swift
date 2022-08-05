// RUN %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

@resultBuilder
struct TupleBuilder<T> {
  static func buildBlock() -> () { }

  static func buildBlock(_ t1: T) -> T {
    return t1
  }

  static func buildBlock(_ t1: T, _ t2: T) -> (T, T) {
    return (t1, t2)
  }
  static func buildBlock(_ t1: T, _ t2: T, _ t3: T) -> (T, T, T) {
    return (t1, t2, t3)
  }
}

func buildStringTuple<Result>(@TupleBuilder<String> x: () -> Result) {}

enum StringFactory {
  static func makeString(x: String) -> String { return x }
}

enum Letters {
  case a
  case b
  case c
}

let MyConstantString = "MyConstant"
let MyConstantBool = true

func testGlobalLookup() {
  @TupleBuilder<String> var x1 {
    #^GLOBAL_LOOKUP^#
    // GLOBAL_LOOKUP: Begin completions
    // GLOBAL_LOOKUP: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]:         MyConstantString[#String#]; name=MyConstantString
    // GLOBAL_LOOKUP: End completions
  }

  @TupleBuilder<String> var x2 {
    if true {
      #^GLOBAL_LOOKUP_IN_IF_BODY?check=GLOBAL_LOOKUP_NO_TYPE_RELATION^#
// GLOBAL_LOOKUP_NO_TYPE_RELATION: Begin completions
// GLOBAL_LOOKUP_NO_TYPE_RELATION: Decl[GlobalVar]/CurrModule:         MyConstantString[#String#];
// GLOBAL_LOOKUP_NO_TYPE_RELATION: End completions
    }
  }

  @TupleBuilder<String> var x3 {
    if {
      #^GLOBAL_LOOKUP_IN_IF_BODY_WITHOUT_CONDITION?check=GLOBAL_LOOKUP_NO_TYPE_RELATION^#
    }
  }

  @TupleBuilder<String> var x4 {
    guard else {
      #^GLOBAL_LOOKUP_IN_GUARD_BODY_WITHOUT_CONDITION?check=GLOBAL_LOOKUP_NO_TYPE_RELATION^#
    }
  }

  @TupleBuilder<String> var x5 {
    "hello: \(#^GLOBAL_LOOKUP_IN_STRING_LITERAL^#)"
// GLOBAL_LOOKUP_IN_STRING_LITERAL: Begin completions
// GLOBAL_LOOKUP_IN_STRING_LITERAL: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: MyConstantString[#String#];
// GLOBAL_LOOKUP_IN_STRING_LITERAL: End completions
  }

  @TupleBuilder<String> var x5 {
    if #^GLOBAL_LOOKUP_IN_IF_CONDITION^# {
// GLOBAL_LOOKUP_IN_IF_CONDITION: Begin completions
// GLOBAL_LOOKUP_IN_IF_CONDITION: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: MyConstantBool[#Bool#]; name=MyConstantBool
// GLOBAL_LOOKUP_IN_IF_CONDITION: End completions
    }
  }
}

func testStaticMemberLookup() {
  @TupleBuilder<String> var x1 {
    StringFactory.#^COMPLETE_STATIC_MEMBER^#
    // COMPLETE_STATIC_MEMBER: Begin completions
    // COMPLETE_STATIC_MEMBER: Keyword[self]/CurrNominal:          self[#StringFactory.Type#]; name=self
    // COMPLETE_STATIC_MEMBER: Keyword/CurrNominal:                Type[#StringFactory.Type#]; name=Type
    // COMPLETE_STATIC_MEMBER: Decl[StaticMethod]/CurrNominal/TypeRelation[Convertible]:     makeString({#x: String#})[#String#];
    // COMPLETE_STATIC_MEMBER: End completions
  }

  @TupleBuilder<String> var x2 {
    if true {
      StringFactory.#^COMPLETE_STATIC_MEMBER_IN_IF_BODY^#
// COMPLETE_STATIC_MEMBER_IN_IF_BODY: Begin completions
// COMPLETE_STATIC_MEMBER_IN_IF_BODY: Decl[StaticMethod]/CurrNominal:     makeString({#x: String#})[#String#];
// COMPLETE_STATIC_MEMBER_IN_IF_BODY: End completions
    }
  }

  @TupleBuilder<String> var x3 {
    "hello: \(StringFactory.#^COMPLETE_STATIC_MEMBER_IN_STRING_LITERAL?check=COMPLETE_STATIC_MEMBER;xfail=rdar78015646^#)"
  }
}

struct FooStruct {
  var instanceVar : Int
  init(_: Int = 0) { }
  func boolGen() -> Bool { return false }
  func intGen() -> Int { return 1 }
}

func testPatternMatching() {
  @TupleBuilder<String> var x1 {
    let x = Letters.b
    if case .#^COMPLETE_PATTERN_MATCHING_IN_IF?check=COMPLETE_CASE^# = x {
// COMPLETE_CASE: Begin completions
// COMPLETE_CASE-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: a[#Letters#];
// COMPLETE_CASE-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: b[#Letters#];
// COMPLETE_CASE-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: c[#Letters#];
// COMPLETE_CASE: End completions
    }
  }

  @TupleBuilder<String> var x2 {
    let x = Letters.a
    switch x {
    case .#^COMPLETE_CASE_IN_SWITCH?check=COMPLETE_CASE^#:
      break
    }
  }

  @TupleBuilder<String> var x3 {
    let x: FooStruct? = FooStruct()
    guard case .#^GUARD_CASE_PATTERN_1?check=OPTIONAL_FOOSTRUCT^# = x {}
    // OPTIONAL_FOOSTRUCT: Begin completions, 2 items
    // OPTIONAL_FOOSTRUCT-DAG: Decl[EnumElement]/CurrNominal/IsSystem/TypeRelation[Convertible]: none[#Optional<FooStruct>#]; name=none
    // OPTIONAL_FOOSTRUCT-DAG: Decl[EnumElement]/CurrNominal/IsSystem/TypeRelation[Convertible]: some({#FooStruct#})[#Optional<FooStruct>#]; name=some()
    // OPTIONAL_FOOSTRUCT: End completions
  }

  @TupleBuilder<String> var x4 {
    let x: FooStruct? = FooStruct()
    guard case .#^GUARD_CASE_PATTERN_2?check=OPTIONAL_FOOSTRUCT^#some() = x {}
  }
}

func testCompleteFunctionArgumentLabels() {
  @TupleBuilder<String> var x1 {
    StringFactory.makeString(#^FUNCTION_ARGUMENT_LABEL^#)
    // FUNCTION_ARGUMENT_LABEL: Begin completions, 1 item
    // FUNCTION_ARGUMENT_LABEL: Decl[StaticMethod]/CurrNominal/Flair[ArgLabels]: ['(']{#x: String#}[')'][#String#];
    // FUNCTION_ARGUMENT_LABEL: End completions
  }
}

func testCompleteFunctionArgument() {
  @TupleBuilder<String> var x1 {
    StringFactory.makeString(x: #^ARGUMENT_LOOKUP^#)
    // ARGUMENT_LOOKUP: Begin completions
    // ARGUMENT_LOOKUP: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: MyConstantString[#String#];
    // ARGUMENT_LOOKUP: End completions
  }

  @TupleBuilder<String> var x2 {
    if true {
      StringFactory.makeString(x: #^ARGUMENT_LOOKUP_IN_IF_BODY?check=ARGUMENT_LOOKUP^#)
    }
  }
}

func testCompleteErrorTypeInCatch() {
  enum Error4 : Error {
    case E1
    case E2(Int32)
  }
  @TupleBuilder<String> var x1 {
    do {} catch Error4.#^CATCH2^#
  }
// CATCH2: Begin completions
// CATHC2-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: E1[#Error4#]; name=E1
// CATHC2-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: E2({#Int32#})[#Error4#]; name=E2(Int32)
// CATCH2: End completions
}

func testCompleteInStringLiteral() {
  struct Island {
    var turnipPrice: String
  }

  func takeTrailingClosure(_ x: () -> Void) -> Text { fatalError() }

  struct BStack<Content> {
    init(@ViewBuilder content: () -> Content) {}
  }

  protocol View {}

  struct Text: View {
    init(_ x: String) {}

    var body: Never { fatalError() }
  }

  @resultBuilder struct ViewBuilder {
    static func buildBlock() -> Text { fatalError() }
    static func buildBlock<C: View>(_ c: C) -> C { return c }
    static func buildBlock<C1: View, C2: View>(_ c: C1, _ d: C2) -> C1 { return c }
  }


  func foo(island: Island) {
    BStack {
      let b = "\(island.#^STRING_LITERAL_VAR^#turnipPrice)"
      takeTrailingClosure {}
    }
// STRING_LITERAL_VAR: Begin completions, 2 items
// STRING_LITERAL_VAR-DAG: Keyword[self]/CurrNominal:          self[#Island#]; name=self
// STRING_LITERAL_VAR-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: turnipPrice[#String#]; name=turnipPrice
// STRING_LITERAL_VAR: End completions
  }

  func bar(island: Island) {
    BStack {
      Text("\(island.#^STRING_LITERAL_AS_ARGUMENT?check=STRING_LITERAL_VAR^#turnipPrice)")
      takeTrailingClosure {}
    } 
  }
}

func testTypeRelationInResultBuilder() {
  protocol View2 {}

  @resultBuilder public struct ViewBuilder2 {
    static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
    static func buildBlock<C0, C1>(_ c0: C0, _ c1: C1) -> C0 where C0 : View2, C1: View2 { fatalError() }
  }

  struct MyText: View2 {}

  struct MyView {
    @ViewBuilder2 var body: some View2 {
      #^SINGLE_ELEMENT^#
    }
    // SINGLE_ELEMENT: Begin completions
    // SINGLE_ELEMENT-DAG: Decl[Struct]/Local/TypeRelation[Convertible]: MyText[#MyText#];
    // SINGLE_ELEMENT: End completions

    @ViewBuilder2 var body2: some View2 {
      MyText()
      #^SECOND_ELEMENT?check=SINGLE_ELEMENT^#
    }
  }
}
