// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_NO_DOT_1 | FileCheck %s -check-prefix=SUPER_NO_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_NO_DOT_2 | FileCheck %s -check-prefix=SUPER_NO_DOT_2
// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_NO_DOT_3 | FileCheck %s -check-prefix=SUPER_NO_DOT_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_NO_DOT_4 | FileCheck %s -check-prefix=SUPER_NO_DOT_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_DOT_1 | FileCheck %s -check-prefix=SUPER_DOT_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_DOT_2 | FileCheck %s -check-prefix=SUPER_DOT_2

//===---
//===--- Tests for code completion after 'super'.
//===---

class SuperBase1 {
  var baseInstanceVar: Int

  var baseProp : Int {
  get:
    return 42
  set(val):
  }

  // Don't declare constructors.

  func baseFunc0() {}
  func baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    baseInstanceVar = i
  }

  struct BaseNestedStruct {}
  class BaseNestedClass {}
  union BaseNestedUnion {
    case BaseUnionX(Int)
  }

  typealias BaseNestedTypealias = Int
}

extension SuperBase1 {
  var baseExtProp : Int {
  get:
    return 42
  set(val):
  }

  func baseExtFunc0() {}

  static func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  union BaseExtNestedUnion {
    case BaseExtUnionX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

class SuperDerived1 : SuperBase1 {
  var derivedInstanceVar: Int

  constructor() {
    // FIXME: Disabled because we don't delay constructors' bodies.
    // Results should include calls to base constructors that are implicitly defined.
    super#^SUPER_NO_DOT_1^#
  }

  func derivedFunc0() {}

  func test1() {
    super#^SUPER_NO_DOT_2^#
// SUPER_NO_DOT_2: Begin completions
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseInstanceVar[#Int#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseProp[#Int#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseNestedStruct[#SuperBase1.BaseNestedStruct.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseNestedClass[#SuperBase1.BaseNestedClass.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseNestedUnion[#SuperBase1.BaseNestedUnion.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseExtProp[#Int#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .baseExtFunc0()[#Void#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseExtNestedStruct[#SuperBase1.BaseExtNestedStruct.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseExtNestedClass[#SuperBase1.BaseExtNestedClass.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: SwiftDecl: .BaseExtNestedUnion[#SuperBase1.BaseExtNestedUnion.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: Keyword: .metatype[#SuperBase1.metatype#]{{$}}
// SUPER_NO_DOT_2-NEXT: End completions
  }

  func test2() {
    super.#^SUPER_DOT_1^#
// SUPER_DOT_1: Begin completions
// SUPER_DOT_1-NEXT: SwiftDecl: baseInstanceVar[#Int#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseProp[#Int#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseNestedStruct[#SuperBase1.BaseNestedStruct.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseNestedClass[#SuperBase1.BaseNestedClass.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseNestedUnion[#SuperBase1.BaseNestedUnion.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseExtProp[#Int#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: baseExtFunc0()[#Void#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseExtNestedStruct[#SuperBase1.BaseExtNestedStruct.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseExtNestedClass[#SuperBase1.BaseExtNestedClass.metatype#]{{$}}
// SUPER_DOT_1-NEXT: SwiftDecl: BaseExtNestedUnion[#SuperBase1.BaseExtNestedUnion.metatype#]{{$}}
// SUPER_DOT_1-NEXT: Keyword: metatype[#SuperBase1.metatype#]{{$}}
// SUPER_DOT_1-NEXT: End completions
  }
}

class SuperBase2 {
  var baseInstanceVar: Int

  var baseProp : Int {
  get:
    return 42
  set(val):
  }

  constructor() {}
  constructor(a: Double) {}

  func baseFunc0() {}
  func baseFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    baseInstanceVar = i
  }

  struct BaseNestedStruct {}
  class BaseNestedClass {}
  union BaseNestedUnion {
    case BaseUnionX(Int)
  }

  typealias BaseNestedTypealias = Int
}

extension SuperBase2 {
  var baseExtProp : Int {
  get:
    return 42
  set(val):
  }

  func baseExtFunc0() {}

  static func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  union BaseExtNestedUnion {
    case BaseExtUnionX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

class SuperDerived2 : SuperBase2 {
  var derivedInstanceVar: Int

  constructor() {
    // FIXME: Disabled because we don't delay constructors' bodies.
    // Results should include calls to base constructors that are explicitly defined.
    super#^SUPER_NO_DOT_3^#
  }

  func derivedFunc0() {}

  func test1() {
    super#^SUPER_NO_DOT_4^#
// SUPER_NO_DOT_4: Begin completions
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseInstanceVar[#Int#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseProp[#Int#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseFunc0()[#Void#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseFunc1({#a: Int#})[#Void#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: [{#i: Int#}][#Double#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseNestedStruct[#SuperBase2.BaseNestedStruct.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseNestedClass[#SuperBase2.BaseNestedClass.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseNestedUnion[#SuperBase2.BaseNestedUnion.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseExtProp[#Int#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .baseExtFunc0()[#Void#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseExtNestedStruct[#SuperBase2.BaseExtNestedStruct.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseExtNestedClass[#SuperBase2.BaseExtNestedClass.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: SwiftDecl: .BaseExtNestedUnion[#SuperBase2.BaseExtNestedUnion.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: Keyword: .metatype[#SuperBase2.metatype#]{{$}}
// SUPER_NO_DOT_4-NEXT: End completions
  }

  func test2() {
    super.#^SUPER_DOT_2^#
// SUPER_DOT_2: Begin completions
// SUPER_DOT_2-NEXT: SwiftDecl: baseInstanceVar[#Int#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseProp[#Int#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseFunc0()[#Void#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseFunc1({#a: Int#})[#Void#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseNestedStruct[#SuperBase2.BaseNestedStruct.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseNestedClass[#SuperBase2.BaseNestedClass.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseNestedUnion[#SuperBase2.BaseNestedUnion.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseExtProp[#Int#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: baseExtFunc0()[#Void#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseExtNestedStruct[#SuperBase2.BaseExtNestedStruct.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseExtNestedClass[#SuperBase2.BaseExtNestedClass.metatype#]{{$}}
// SUPER_DOT_2-NEXT: SwiftDecl: BaseExtNestedUnion[#SuperBase2.BaseExtNestedUnion.metatype#]{{$}}
// SUPER_DOT_2-NEXT: Keyword: metatype[#SuperBase2.metatype#]{{$}}
// SUPER_DOT_2-NEXT: End completions
  }
}

class SuperKWBase {
  func test() {
    // FIXME: make sure we don't code complete 'super' keyword here.
    #^BASE_SUPER_KW^#
  }
}

class SuperKWDerived : SuperKWBase {
  func test() {
    // FIXME: make sure include 'super' keyword in code completion results here.
    #^DERIVED_SUPER_KW^#
  }
}


