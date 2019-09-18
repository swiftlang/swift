// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_TOP | %FileCheck %s -check-prefix=IN_CLOSURE_TOP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_NONTOP | %FileCheck %s -check-prefix=IN_CLOSURE_TOP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-function-builder-one-way-constraints -code-completion-token=IN_CLOSURE_COLOR_CONTEXT | %FileCheck %s -check-prefix=IN_CLOSURE_COLOR_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-function-builder-one-way-constraints -code-completion-token=IN_CLOSURE_COLOR_CONTEXT_DOT | %FileCheck %s -check-prefix=IN_CLOSURE_COLOR_CONTEXT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-function-builder-one-way-constraints -code-completion-token=CONTEXTUAL_TYPE_1 | %FileCheck %s -check-prefix=CONTEXTUAL_TYPE_VALID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-function-builder-one-way-constraints -code-completion-token=CONTEXTUAL_TYPE_2 | %FileCheck %s -check-prefix=CONTEXTUAL_TYPE_VALID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-function-builder-one-way-constraints -code-completion-token=CONTEXTUAL_TYPE_3 | %FileCheck %s -check-prefix=CONTEXTUAL_TYPE_VALID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-function-builder-one-way-constraints -code-completion-token=CONTEXTUAL_TYPE_4 | %FileCheck %s -check-prefix=CONTEXTUAL_TYPE_VALID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -disable-function-builder-one-way-constraints -code-completion-token=CONTEXTUAL_TYPE_5 | %FileCheck %s -check-prefix=CONTEXTUAL_TYPE_INVALID

struct Tagged<Tag, Entity> {
  let tag: Tag
  let entity: Entity

  static func fo
}

protocol Taggable {
}

extension Taggable {
  func tag<Tag>(_ tag: Tag) -> Tagged<Tag, Self> {
    return Tagged(tag: tag, entity: self)
  }
}

extension Int: Taggable { }
extension String: Taggable { }

@_functionBuilder
struct TaggedBuilder<Tag> {
  static func buildBlock() -> () { }

  static func buildBlock<T1>(_ t1: Tagged<Tag, T1>) -> Tagged<Tag, T1> {
    return t1
  }

  static func buildBlock<T1, T2>(_ t1: Tagged<Tag, T1>, _ t2: Tagged<Tag, T2>) -> (Tagged<Tag, T1>, Tagged<Tag, T2>) {
    return (t1, t2)
  }
  static func buildBlock<T1, T2, T3>(_ t1: Tagged<Tag, T1>, _ t2: Tagged<Tag, T2>, _ t2: Tagged<Tag, T3>) -> (Tagged<Tag, T1>, Tagged<Tag, T2>, Tagged<Tag, T3>) {
    return (t1, t2, t3)
  }
}

enum Color {
  case red, green, blue
}

func acceptColorTagged<Result>(@TaggedBuilder<Color> body: (Color) -> Result) {
  print(body(.blue))
}

var globalIntVal: Int = 1
let globalStringVal: String = ""

func testAcceptColorTagged(paramIntVal: Int, paramStringVal: String) {

  let taggedValue = paramIntVal.tag(Color.red)
  
  acceptColorTagged { color in
    #^IN_CLOSURE_TOP^#
// IN_CLOSURE_TOP_CONTEXT: Begin completions
// IN_CLOSURE_TOP-DAG: Decl[LocalVar]/Local:               taggedValue[#Tagged<Color, Int>#]; name=taggedValue
// IN_CLOSURE_TOP-DAG: Decl[GlobalVar]/CurrModule:         globalIntVal[#Int#]; name=globalIntVal
// IN_CLOSURE_TOP-DAG: Decl[GlobalVar]/CurrModule:         globalStringVal[#String#]; name=globalStringVal
// IN_CLOSURE_TOP-DAG: Decl[LocalVar]/Local:               color; name=color
// IN_CLOSURE_TOP-DAG: Decl[LocalVar]/Local:               paramIntVal[#Int#]; name=paramIntVal
// IN_CLOSURE_TOP-DAG: Decl[LocalVar]/Local:               paramStringVal[#String#]; name=paramStringVal
// IN_CLOSURE_TOP: End completions
  }

  acceptColorTagged { color in
    paramIntVal.tag(.red)
    #^IN_CLOSURE_NONTOP^#
// Same as IN_CLOSURE_TOP.
  }

  acceptColorTagged { color in
    paramIntVal.tag(#^IN_CLOSURE_COLOR_CONTEXT^#)
// IN_CLOSURE_COLOR_CONTEXT: Begin completions
// IN_CLOSURE_COLOR_CONTEXT-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#(tag): Color#}[')'][#Tagged<Color, Int>#]; name=tag: Color
// IN_CLOSURE_COLOR_CONTEXT-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]: color[#Color#]; name=color
// IN_CLOSURE_COLOR_CONTEXT-DAG: Decl[LocalVar]/Local:               taggedValue[#Tagged<Color, Int>#]; name=taggedValue
// IN_CLOSURE_COLOR_CONTEXT-DAG: Decl[LocalVar]/Local:               paramIntVal[#Int#]; name=paramIntVal
// IN_CLOSURE_COLOR_CONTEXT-DAG: Decl[LocalVar]/Local:               paramStringVal[#String#]; name=paramStringVal
// IN_CLOSURE_COLOR_CONTEXT-DAG: Decl[Enum]/CurrModule/TypeRelation[Identical]: Color[#Color#]; name=Color
// IN_CLOSURE_COLOR_CONTEXT: End completions
  }

  acceptColorTagged { color in
    paramIntVal.tag(.#^IN_CLOSURE_COLOR_CONTEXT_DOT^#)
// IN_CLOSURE_COLOR_CONTEXT_DOT: Begin completions, 3 items
// IN_CLOSURE_COLOR_CONTEXT_DOT-DAG: Decl[EnumElement]/ExprSpecific:     red[#Color#]; name=red
// IN_CLOSURE_COLOR_CONTEXT_DOT-DAG: Decl[EnumElement]/ExprSpecific:     green[#Color#]; name=green
// IN_CLOSURE_COLOR_CONTEXT_DOT-DAG: Decl[EnumElement]/ExprSpecific:     blue[#Color#]; name=blue
// IN_CLOSURE_COLOR_CONTEXT_DOT: End completions
  }
}

enum MyEnum {
  case east, west
  case north, south
}
@_functionBuilder
struct EnumToVoidBuilder {
  static func buildBlock() {}
  static func buildBlock(_ :MyEnum) {}
  static func buildBlock(_ :MyEnum, _: MyEnum) {}
  static func buildBlock(_ :MyEnum, _: MyEnum, _: MyEnum) {}
}
func acceptBuilder(@EnumToVoidBuilder body: () -> Void) {}

// CONTEXTUAL_TYPE_INVALID-NOT: Begin completions

// CONTEXTUAL_TYPE_VALID: Begin completions, 4 items
// CONTEXTUAL_TYPE_VALID-DAG: Decl[EnumElement]/ExprSpecific:     east[#MyEnum#]; name=east
// CONTEXTUAL_TYPE_VALID-DAG: Decl[EnumElement]/ExprSpecific:     west[#MyEnum#]; name=west
// CONTEXTUAL_TYPE_VALID-DAG: Decl[EnumElement]/ExprSpecific:     north[#MyEnum#]; name=north
// CONTEXTUAL_TYPE_VALID-DAG: Decl[EnumElement]/ExprSpecific:     south[#MyEnum#]; name=south
// CONTEXTUAL_TYPE_VALID: End completions 

func testContextualType() {
  acceptBuilder {
    .#^CONTEXTUAL_TYPE_1^#
  }
  acceptBuilder {
    .#^CONTEXTUAL_TYPE_2^#;
    .north;
  }
  acceptBuilder {
    .north;
    .#^CONTEXTUAL_TYPE_3^#;
  }
  acceptBuilder {
    .north;
    .east;
    .#^CONTEXTUAL_TYPE_4^#
  }
  acceptBuilder {
    .north;
    .east;
    .south;
// NOTE: Invalid because 'EnumToVoidBuilder' doesn't have 4 params overload.
    .#^CONTEXTUAL_TYPE_5^#
  }
}
