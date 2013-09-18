// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_SELF_NO_DOT_1 | FileCheck %s -check-prefix=FUNC_SELF_NO_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_SELF_DOT_1 | FileCheck %s -check-prefix=FUNC_SELF_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_STATIC_SELF_NO_DOT_1 | FileCheck %s -check-prefix=FUNC_STATIC_SELF_NO_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_STATIC_SELF_DOT_1 | FileCheck %s -check-prefix=FUNC_STATIC_SELF_DOT_1

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SELF_NO_DOT_1 | FileCheck %s -check-prefix=CONSTRUCTOR_SELF_NO_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SELF_DOT_1 | FileCheck %s -check-prefix=CONSTRUCTOR_SELF_DOT_1

//===---
//===--- Tests for code completion after 'self'.
//===---

class ThisBase1 {
  var baseInstanceVar: Int

  func baseFunc0() {}
  func baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    baseInstanceVar = i
  }

  // FIXME: uncomment when we have static vars.
  // static var baseStaticVar : Int

  static func baseStaticFunc0() {}
}

extension ThisBase1 {
  var baseExtProp : Int {
  get:
    return 42
  set(val):
  }

  func baseExtInstanceFunc0() {}

  static func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  union BaseExtNestedUnion {
    case BaseExtUnionX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

class ThisDerived1 : ThisBase1 {
  var derivedInstanceVar: Int

  func derivedFunc0() {}

  subscript(i: Double) -> Int {
  get:
    return Int(i)
  set(val):
    baseInstanceVar = Int(i)
  }

  static func derivedStaticFunc0() {}

  constructor() {
    self#^CONSTRUCTOR_SELF_NO_DOT_1^#
// CONSTRUCTOR_SELF_NO_DOT_1: Begin completions
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedInstanceVar[#Int#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedFunc0()[#Void#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: [{#i: Double#}][#Int#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .test1()[#Void#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .test2()[#Void#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedExtProp[#Int#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedExtInstanceFunc0()[#Void#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedStruct[#ThisDerived1.DerivedExtNestedStruct.metatype#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedClass[#ThisDerived1.DerivedExtNestedClass.metatype#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedUnion[#ThisDerived1.DerivedExtNestedUnion.metatype#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseInstanceVar[#Int#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseExtProp[#Int#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseExtInstanceFunc0()[#Void#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedStruct[#ThisBase1.BaseExtNestedStruct.metatype#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedClass[#ThisBase1.BaseExtNestedClass.metatype#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedUnion[#ThisBase1.BaseExtNestedUnion.metatype#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: Keyword: .metatype[#ThisDerived1.metatype#]{{$}}
// CONSTRUCTOR_SELF_NO_DOT_1-NEXT: End completions
  }

  constructor(a : Int) {
    self.#^CONSTRUCTOR_SELF_DOT_1^#
// CONSTRUCTOR_SELF_DOT_1: Begin completions
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: derivedInstanceVar[#Int#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: derivedFunc0()[#Void#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: test1()[#Void#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: test2()[#Void#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: derivedExtProp[#Int#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: derivedExtInstanceFunc0()[#Void#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedStruct[#ThisDerived1.DerivedExtNestedStruct.metatype#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedClass[#ThisDerived1.DerivedExtNestedClass.metatype#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedUnion[#ThisDerived1.DerivedExtNestedUnion.metatype#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: baseInstanceVar[#Int#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: baseExtProp[#Int#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: baseExtInstanceFunc0()[#Void#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedStruct[#ThisBase1.BaseExtNestedStruct.metatype#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedClass[#ThisBase1.BaseExtNestedClass.metatype#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedUnion[#ThisBase1.BaseExtNestedUnion.metatype#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: Keyword: metatype[#ThisDerived1.metatype#]{{$}}
// CONSTRUCTOR_SELF_DOT_1-NEXT: End completions
  }

  func test1() {
    self#^FUNC_SELF_NO_DOT_1^#
// FUNC_SELF_NO_DOT_1: Begin completions
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedInstanceVar[#Int#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedFunc0()[#Void#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: [{#i: Double#}][#Int#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .test1()[#Void#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .test2()[#Void#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedExtProp[#Int#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedExtInstanceFunc0()[#Void#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedStruct[#ThisDerived1.DerivedExtNestedStruct.metatype#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedClass[#ThisDerived1.DerivedExtNestedClass.metatype#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedUnion[#ThisDerived1.DerivedExtNestedUnion.metatype#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseInstanceVar[#Int#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseExtProp[#Int#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseExtInstanceFunc0()[#Void#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedStruct[#ThisBase1.BaseExtNestedStruct.metatype#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedClass[#ThisBase1.BaseExtNestedClass.metatype#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedUnion[#ThisBase1.BaseExtNestedUnion.metatype#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: Keyword: .metatype[#ThisDerived1.metatype#]{{$}}
// FUNC_SELF_NO_DOT_1-NEXT: End completions
  }

  func test2() {
    self.#^FUNC_SELF_DOT_1^#
// FUNC_SELF_DOT_1: Begin completions
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: derivedInstanceVar[#Int#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: derivedFunc0()[#Void#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: test1()[#Void#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: test2()[#Void#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: derivedExtProp[#Int#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: derivedExtInstanceFunc0()[#Void#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedStruct[#ThisDerived1.DerivedExtNestedStruct.metatype#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedClass[#ThisDerived1.DerivedExtNestedClass.metatype#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedUnion[#ThisDerived1.DerivedExtNestedUnion.metatype#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: baseInstanceVar[#Int#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: baseExtProp[#Int#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: baseExtInstanceFunc0()[#Void#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedStruct[#ThisBase1.BaseExtNestedStruct.metatype#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedClass[#ThisBase1.BaseExtNestedClass.metatype#]{{$}}
// FUNC_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedUnion[#ThisBase1.BaseExtNestedUnion.metatype#]{{$}}
// FUNC_SELF_DOT_1-NEXT: Keyword: metatype[#ThisDerived1.metatype#]{{$}}
// FUNC_SELF_DOT_1-NEXT: End completions
  }

  static func staticTest1() {
    self#^FUNC_STATIC_SELF_NO_DOT_1^#
// FUNC_STATIC_SELF_NO_DOT_1: Begin completions
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: [{#i: Double#}][#Int#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedStaticFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .test1()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .test2()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .staticTest1()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .staticTest2()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedExtInstanceFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .derivedExtStaticFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedStruct[#ThisDerived1.DerivedExtNestedStruct.metatype#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedClass[#ThisDerived1.DerivedExtNestedClass.metatype#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedUnion[#ThisDerived1.DerivedExtNestedUnion.metatype#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .DerivedExtNestedTypealias[#Int.metatype#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseStaticFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseExtInstanceFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .baseExtStaticFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedStruct[#ThisBase1.BaseExtNestedStruct.metatype#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedClass[#ThisBase1.BaseExtNestedClass.metatype#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedUnion[#ThisBase1.BaseExtNestedUnion.metatype#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: SwiftDecl: .BaseExtNestedTypealias[#Int.metatype#]{{$}}
// Yes, '.metatype.metatype' is correct because we are in a static method.
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: Keyword: .metatype[#ThisDerived1.metatype.metatype#]{{$}}
// FUNC_STATIC_SELF_NO_DOT_1-NEXT: End completions
  }

  static func staticTest2() {
    self.#^FUNC_STATIC_SELF_DOT_1^#
// FUNC_STATIC_SELF_DOT_1: Begin completions
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: derivedFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: derivedStaticFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: test1()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: test2()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: staticTest1()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: staticTest2()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: derivedExtInstanceFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: derivedExtStaticFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedStruct[#ThisDerived1.DerivedExtNestedStruct.metatype#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedClass[#ThisDerived1.DerivedExtNestedClass.metatype#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedUnion[#ThisDerived1.DerivedExtNestedUnion.metatype#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: DerivedExtNestedTypealias[#Int.metatype#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: baseStaticFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: baseExtInstanceFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: baseExtStaticFunc0()[#Void#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedStruct[#ThisBase1.BaseExtNestedStruct.metatype#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedClass[#ThisBase1.BaseExtNestedClass.metatype#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedUnion[#ThisBase1.BaseExtNestedUnion.metatype#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: SwiftDecl: BaseExtNestedTypealias[#Int.metatype#]{{$}}
// Yes, '.metatype.metatype' is correct because we are in a static method.
// FUNC_STATIC_SELF_DOT_1-NEXT: Keyword: metatype[#ThisDerived1.metatype.metatype#]{{$}}
// FUNC_STATIC_SELF_DOT_1-NEXT: End completions
  }
}

extension ThisDerived1 {
  var derivedExtProp : Int {
  get:
    return 42
  set(val):
  }

  func derivedExtInstanceFunc0() {}

  static func derivedExtStaticFunc0() {}

  struct DerivedExtNestedStruct {}
  class DerivedExtNestedClass {}
  union DerivedExtNestedUnion {
    case DerivedExtUnionX(Int)
  }

  typealias DerivedExtNestedTypealias = Int
}

