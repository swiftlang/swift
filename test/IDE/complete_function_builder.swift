// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_TOP | %FileCheck %s -check-prefix=IN_CLOSURE_TOP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_NONTOP | %FileCheck %s -check-prefix=IN_CLOSURE_TOP
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_COLOR_CONTEXT | %FileCheck %s -check-prefix=IN_CLOSURE_COLOR_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_COLOR_CONTEXT_DOT | %FileCheck %s -check-prefix=IN_CLOSURE_COLOR_CONTEXT_DOT

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

