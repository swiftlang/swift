// RUN: %batch-code-completion

//===---
//===--- Tests for code completion after 'self'.
//===---

class ThisBase1 {
  var baseInstanceVar: Int

  func baseFunc0() {}
  func baseFunc1(_ a: Int) {}

  subscript(i: Int) -> Double {
    get {
      return Double(i)
    }
    set(v) {
      baseInstanceVar = i
    }
  }

  class var baseStaticVar: Int = 12

  class func baseStaticFunc0() {}
}

extension ThisBase1 {
  var baseExtProp : Int {
    get {
      return 42
    }
    set(v) {}
  }

  func baseExtInstanceFunc0() {}

  var baseExtStaticVar: Int

  var baseExtStaticProp: Int {
    get {
      return 42
    }
    sel(v) {}
  }

  class func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  enum BaseExtNestedEnum {
    case BaseExtEnumX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

class ThisDerived1 : ThisBase1 {
  var derivedInstanceVar: Int

  func derivedFunc0() {}

  subscript(i: Double) -> Int {
    get {
      return Int(i)
    }
    set(v) {
      baseInstanceVar = Int(i)
    }
  }

  subscript(s s: String) -> Int {
    get {
      return 0
    }
    set(v) {
    }
  }

  class var derivedStaticVar: Int = 42

  class func derivedStaticFunc0() {}

// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:    .derivedInstanceVar[#Int#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: .derivedFunc0()[#Void#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[Subscript]/CurrNominal:       [{#(i): Double#}][#Int#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[Subscript]/CurrNominal:       [{#s: String#}][#Int#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: .test1()[#Void#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: .test2()[#Void#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:    .derivedExtProp[#Int#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: .derivedExtInstanceFunc0()[#Void#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:    .derivedExtStaticProp[#Int#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceVar]/Super:          .baseInstanceVar[#Int#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/Super:       .baseFunc0()[#Void#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/Super:       .baseFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[Subscript]/Super:            [{#(i): Int#}][#Double#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceVar]/Super:          .baseExtProp[#Int#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/Super:       .baseExtInstanceFunc0()[#Void#]{{; name=.+$}}
// COMMON_SELF_NO_DOT_1-DAG: Decl[InstanceVar]/Super:          .baseExtStaticProp[#Int#]{{; name=.+$}}

// COMMON_SELF_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:    derivedInstanceVar[#Int#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: derivedFunc0()[#Void#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: test1()[#Void#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: test2()[#Void#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:    derivedExtProp[#Int#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: derivedExtInstanceFunc0()[#Void#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceVar]/CurrNominal:    derivedExtStaticProp[#Int#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceVar]/Super:          baseInstanceVar[#Int#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceMethod]/Super:       baseFunc0()[#Void#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceMethod]/Super:       baseFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceVar]/Super:          baseExtProp[#Int#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceMethod]/Super:       baseExtInstanceFunc0()[#Void#]{{; name=.+$}}
// COMMON_SELF_DOT_1-DAG: Decl[InstanceVar]/Super:          baseExtStaticProp[#Int#]{{; name=.+$}}

  init() {
    self#^CONSTRUCTOR_SELF_NO_DOT_1?check=CONSTRUCTOR_SELF_NO_DOT_1;check=COMMON_SELF_NO_DOT_1^#
// CONSTRUCTOR_SELF_NO_DOT_1: Begin completions, 21 items
// CONSTRUCTOR_SELF_NO_DOT_1-NOT: Decl[Constructor]
    let d: ThisDerived1
    d#^CONSTRUCTOR_NONSELF_NO_DOT_1?check=COMMON_SELF_NO_DOT_1;check=NO_INIT^#
// NO_INIT-NOT: init()
  }

  init(a: Int) {
    self.#^CONSTRUCTOR_SELF_DOT_1?check=CONSTRUCTOR_SELF_DOT_1;check=COMMON_SELF_DOT_1^#
// CONSTRUCTOR_SELF_DOT_1: Begin completions, 16 items
// CONSTRUCTOR_SELF_DOT_1-NOT: Decl[Constructor]
    let d: ThisDerived1
    d.#^CONSTRUCTOR_NONSELF_DOT_1?check=COMMON_SELF_DOT_1;check=NO_INIT^#
  }

  convenience init(conv: Int) {
    self.#^CONVENIENCE_SELF_DOT_1?check=CONVENIENCE_SELF_DOT_1;check=COMMON_SELF_DOT_1^#
// CONVENIENCE_SELF_DOT_1: Begin completions, 20 items
// CONVENIENCE_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal: init()[#ThisDerived1#]; name=init()
// CONVENIENCE_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal: init({#a: Int#})[#ThisDerived1#]; name=init(a:)
// CONVENIENCE_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal: init({#conv: Int#})[#ThisDerived1#]; name=init(conv:)
  }

  deinit {
    self#^DESTRUCTOR_SELF_NO_DOT_1?check=DESTRUCTOR_SELF_NO_DOT_1;check=COMMON_SELF_NO_DOT_1;check=NO_INIT^#
// DESTRUCTOR_SELF_NO_DOT_1: Begin completions, 21 items

    self.#^DESTRUCTOR_SELF_DOT_1?check=DESTRUCTOR_SELF_DOT_1;check=COMMON_SELF_DOT_1;check=NO_INIT^#
// DESTRUCTOR_SELF_DOT_1: Begin completions, 16 items
  }

  func test1() {
    self#^FUNC_SELF_NO_DOT_1?check=FUNC_SELF_NO_DOT_1;check=COMMON_SELF_NO_DOT_1;check=NO_INIT^#
// FUNC_SELF_NO_DOT_1: Begin completions, 21 items
  }

  func test2() {
    self.#^FUNC_SELF_DOT_1?check=FUNC_SELF_DOT_1;check=COMMON_SELF_DOT_1;check=NO_INIT^#
// FUNC_SELF_DOT_1: Begin completions, 16 items
  }

  class func staticTest1() {
    self#^FUNC_STATIC_SELF_NO_DOT_1^#
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   .derivedFunc0({#(self): ThisDerived1#})[#() -> Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[StaticVar]/CurrNominal:        .derivedStaticVar[#Int#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[StaticMethod]/CurrNominal:     .derivedStaticFunc0()[#Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Constructor]/CurrNominal:      .init()[#ThisDerived1#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Constructor]/CurrNominal:      .init({#a: Int#})[#ThisDerived1#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Constructor]/CurrNominal:      .init({#conv: Int#})[#ThisDerived1#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   .test1({#(self): ThisDerived1#})[#() -> Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   .test2({#(self): ThisDerived1#})[#() -> Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[StaticMethod]/CurrNominal:     .staticTest1()[#Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[StaticMethod]/CurrNominal:     .staticTest2()[#Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   .derivedExtInstanceFunc0({#(self): ThisDerived1#})[#() -> Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[StaticMethod]/CurrNominal:     .derivedExtStaticFunc0()[#Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Struct]/CurrNominal:           .DerivedExtNestedStruct[#DerivedExtNestedStruct#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Class]/CurrNominal:            .DerivedExtNestedClass[#DerivedExtNestedClass#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Enum]/CurrNominal:             .DerivedExtNestedEnum[#DerivedExtNestedEnum#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[TypeAlias]/CurrNominal:        .DerivedExtNestedTypealias[#Int#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Constructor]/CurrNominal:      .init({#someExtensionArg: Int#})[#ThisDerived1#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/Super:         .baseFunc0({#(self): ThisBase1#})[#() -> Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/Super:         .baseFunc1({#(self): ThisBase1#})[#(Int) -> Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[StaticVar]/Super:              .baseStaticVar[#Int#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[StaticMethod]/Super:           .baseStaticFunc0()[#Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[InstanceMethod]/Super:         .baseExtInstanceFunc0({#(self): ThisBase1#})[#() -> Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[StaticMethod]/Super:           .baseExtStaticFunc0()[#Void#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Struct]/Super:                 .BaseExtNestedStruct[#ThisBase1.BaseExtNestedStruct#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Class]/Super:                  .BaseExtNestedClass[#ThisBase1.BaseExtNestedClass#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[Enum]/Super:                   .BaseExtNestedEnum[#ThisBase1.BaseExtNestedEnum#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Decl[TypeAlias]/Super:              .BaseExtNestedTypealias[#Int#]
// FUNC_STATIC_SELF_NO_DOT_1-DAG: Keyword[self]/CurrNominal: .self[#ThisDerived1.Type#]; name=self
  }

  class func staticTest2() {
    self.#^FUNC_STATIC_SELF_DOT_1^#
// FUNC_STATIC_SELF_DOT_1: Begin completions, 28 items
// FUNC_STATIC_SELF_DOT_1-DAG: Keyword[self]/CurrNominal: self[#ThisDerived1.Type#]; name=self
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   derivedFunc0({#(self): ThisDerived1#})[#() -> Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[StaticVar]/CurrNominal:        derivedStaticVar[#Int#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[StaticMethod]/CurrNominal:     derivedStaticFunc0()[#Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal:      init()[#ThisDerived1#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal:      init({#a: Int#})[#ThisDerived1#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal:      init({#conv: Int#})[#ThisDerived1#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   test1({#(self): ThisDerived1#})[#() -> Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   test2({#(self): ThisDerived1#})[#() -> Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[StaticMethod]/CurrNominal:     staticTest1()[#Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[StaticMethod]/CurrNominal:     staticTest2()[#Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   derivedExtInstanceFunc0({#(self): ThisDerived1#})[#() -> Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[StaticMethod]/CurrNominal:     derivedExtStaticFunc0()[#Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Struct]/CurrNominal:           DerivedExtNestedStruct[#DerivedExtNestedStruct#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Class]/CurrNominal:            DerivedExtNestedClass[#DerivedExtNestedClass#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Enum]/CurrNominal:             DerivedExtNestedEnum[#DerivedExtNestedEnum#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[TypeAlias]/CurrNominal:        DerivedExtNestedTypealias[#Int#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal:      init({#someExtensionArg: Int#})[#ThisDerived1#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[InstanceMethod]/Super:         baseFunc0({#(self): ThisBase1#})[#() -> Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[InstanceMethod]/Super:         baseFunc1({#(self): ThisBase1#})[#(Int) -> Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[StaticVar]/Super:              baseStaticVar[#Int#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[StaticMethod]/Super:           baseStaticFunc0()[#Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[InstanceMethod]/Super:         baseExtInstanceFunc0({#(self): ThisBase1#})[#() -> Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[StaticMethod]/Super:           baseExtStaticFunc0()[#Void#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Struct]/Super:                 BaseExtNestedStruct[#ThisBase1.BaseExtNestedStruct#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Class]/Super:                  BaseExtNestedClass[#ThisBase1.BaseExtNestedClass#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[Enum]/Super:                   BaseExtNestedEnum[#ThisBase1.BaseExtNestedEnum#]
// FUNC_STATIC_SELF_DOT_1-DAG: Decl[TypeAlias]/Super:              BaseExtNestedTypealias[#Int#]
  }
}

class func staticTest3() {
  self(#^STATIC_SELF_PAREN^#
  // STATIC_SELF_PAREN-NOT: Decl[Constructor]
}

extension ThisDerived1 {
  var derivedExtProp : Int {
    get {
      return 42
    }
    set(v) {}
  }

  func derivedExtInstanceFunc0() {}

  var derivedExtStaticVar: Int

  var derivedExtStaticProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  class func derivedExtStaticFunc0() {}

  struct DerivedExtNestedStruct {}
  class DerivedExtNestedClass {}
  enum DerivedExtNestedEnum {
    case DerivedExtEnumX(Int)
  }

  typealias DerivedExtNestedTypealias = Int

  convenience init(someExtensionArg: Int) {
    self.#^EXTENSION_CONSTRUCTOR_SELF_DOT_1?check=COMMON_SELF_DOT_1;check=EXTENSION_CONSTRUCTOR_SELF_DOT_1^#
// EXTENSION_CONSTRUCTOR_SELF_DOT_1: Begin completions, 20 items
// EXTENSION_CONSTRUCTOR_SELF_DOT_1: Decl[Constructor]/CurrNominal:      init()[#ThisDerived1#]; name=init()
// EXTENSION_CONSTRUCTOR_SELF_DOT_1: Decl[Constructor]/CurrNominal:      init({#a: Int#})[#ThisDerived1#]; name=init(a:)
// EXTENSION_CONSTRUCTOR_SELF_DOT_1: Decl[Constructor]/CurrNominal:      init({#someExtensionArg: Int#})[#ThisDerived1#]; name=init(someExtensionArg:)

  }
}

struct S1 {
  init() {}
  init(x: Int) {
    self.#^STRUCT_CONSTRUCTOR_SELF_DOT_1^#
// STRUCT_CONSTRUCTOR_SELF_DOT_1: Begin completions, 4 items
// STRUCT_CONSTRUCTOR_SELF_DOT_1-DAG: Keyword[self]/CurrNominal: self[#S1#]; name=self
// STRUCT_CONSTRUCTOR_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal: init()[#S1#]; name=init()
// STRUCT_CONSTRUCTOR_SELF_DOT_1-DAG: Decl[Constructor]/CurrNominal: init({#x: Int#})[#S1#]; name=init(x:)
// STRUCT_CONSTRUCTOR_SELF_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal: f()[#Void#]; name=f()
    let s: S1
    s.#^STRUCT_CONSTRUCTOR_NONSELF_DOT_1?check=NO_INIT^#
  }
  func f() {
    self.#^STRUCT_FUNC_SELF_DOT_1?check=NO_INIT^#
  }
}
