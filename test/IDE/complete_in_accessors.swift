// RUN: %batch-code-completion

//===--- Helper types that are used in this test

struct FooStruct {
  var instanceVar: Int = 0
  func instanceFunc0() {}
  static var staticVar: Int
  static func staticFunc0() {}
}

func returnsInt() -> Int {}

// FOO_OBJECT_DOT: Begin completions, 3 items
// FOO_OBJECT_DOT-DAG: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// FOO_OBJECT_DOT-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}

// WITH_GLOBAL_DECLS-DAG: Decl[Struct]/CurrModule:       FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_GLOBAL_DECLS-DAG: Decl[FreeFunction]/CurrModule{{(/TypeRelation\[Convertible\])?}}: returnsInt()[#Int#]{{; name=.+$}}

// WITH_GLOBAL_DECLS1-DAG: Decl[Struct]/CurrModule:       FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_GLOBAL_DECLS1-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Convertible]: returnsInt()[#Int#]{{; name=.+$}}

// WITH_MEMBER_DECLS-DAG: Decl[Struct]/CurrModule:          FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_MEMBER_DECLS-DAG: Decl[FreeFunction]/CurrModule{{(/TypeRelation\[Convertible\])?}}:    returnsInt()[#Int#]{{; name=.+$}}
// WITH_MEMBER_DECLS-DAG: Decl[LocalVar]/Local:             self[#MemberAccessors#]{{; name=.+$}}
// WITH_MEMBER_DECLS-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Double#]{{; name=.+$}}
// WITH_MEMBER_DECLS-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Float#]{{; name=.+$}}

// WITH_MEMBER_DECLS_INIT-DAG: Decl[Struct]/CurrModule:          FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_MEMBER_DECLS_INIT-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Convertible]: returnsInt()[#Int#]{{; name=.+$}}
// WITH_MEMBER_DECLS_INIT-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(self): MemberAccessors#})[#(Int) -> Float#]{{; name=.+$}}

// WITH_MEMBER_DECLS_INIT_WRONG-NOT: self[
// WITH_MEMBER_DECLS_INIT_WRONG-NOT: instanceVar

// WITH_LOCAL_DECLS-DAG: Decl[Struct]/CurrModule:          FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_LOCAL_DECLS-DAG: Decl[FreeFunction]/CurrModule{{(/TypeRelation\[Convertible\])?}}:    returnsInt()[#Int#]{{; name=.+$}}
// WITH_LOCAL_DECLS-DAG: Decl[LocalVar]/Local{{(/TypeRelation\[Convertible\])?}}:             functionParam[#Int#]{{; name=.+$}}
// WITH_LOCAL_DECLS-DAG: Decl[FreeFunction]/Local:         localFunc({#(a): Int#})[#Float#]{{; name=.+$}}

// WITH_LOCAL_DECLS1-DAG: Decl[Struct]/CurrModule:          FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_LOCAL_DECLS1-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Convertible]:    returnsInt()[#Int#]{{; name=.+$}}
// WITH_LOCAL_DECLS1-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: functionParam[#Int#]{{; name=.+$}}
// WITH_LOCAL_DECLS1-DAG: Decl[FreeFunction]/Local:         localFunc({#(a): Int#})[#Float#]{{; name=.+$}}

// WITH_OLDVALUE-DAG: Decl[LocalVar]/Local: oldValue[#Int#]{{; name=.+$}}

// WITH_NEWVALUE-DAG: Decl[LocalVar]/Local: newValue[#Int#]{{; name=.+$}}


//===--- Check that we can complete inside accessors.

// Each test comes in two variants: a basic one, with global completions, and
// another one with a local variable to make sure that we type check accessor
// bodies.

var globalAccessorImplicitGet1: Int {
  #^GLOBAL_ACCESSOR_IMPLICIT_GET_1?check=WITH_GLOBAL_DECLS^#
}
var globalAccessorImplicitGet2: Int {
  var fs = FooStruct()
  fs.#^GLOBAL_ACCESSOR_IMPLICIT_GET_2?check=FOO_OBJECT_DOT^#
}

var globalAccessorGet1: Int {
  get {
    #^GLOBAL_ACCESSOR_GET_1?check=WITH_GLOBAL_DECLS^#
  }
}
var globalAccessorGet2: Int {
  get {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_GET_2?check=FOO_OBJECT_DOT^#
  }
}

var globalAccessorSet1: Int {
  set {
    #^GLOBAL_ACCESSOR_SET_1?check=WITH_GLOBAL_DECLS;check=WITH_NEWVALUE^#
  }
}
var globalAccessorSet2: Int {
  set {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_SET_2?check=FOO_OBJECT_DOT^#
  }
}
var globalAccessorSet3: Int {
  set(newValue) {
    #^GLOBAL_ACCESSOR_SET_3?check=WITH_GLOBAL_DECLS;check=WITH_NEWVALUE^#
  }
}
var globalAccessorSet4: Int {
  set(newValue) {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_SET_4?check=FOO_OBJECT_DOT^#
  }
}

var globalAccessorWillSet1: Int {
  willSet {
    #^GLOBAL_ACCESSOR_WILLSET_1?check=WITH_GLOBAL_DECLS;check=WITH_NEWVALUE^#
  }
}
var globalAccessorWillSet2: Int {
  willSet {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_WILLSET_2?check=FOO_OBJECT_DOT^#
  }
}
var globalAccessorWillSet3 = 42 {
willSet {
  #^GLOBAL_ACCESSOR_WILLSET_3?check=WITH_GLOBAL_DECLS;check=WITH_NEWVALUE^#
}
}

var globalAccessorDidSet1: Int {
  didSet {
    #^GLOBAL_ACCESSOR_DIDSET_1?check=WITH_GLOBAL_DECLS;check=WITH_OLDVALUE^#
  }
}
var globalAccessorDidSet2: Int {
  didSet {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_DIDSET_2?check=FOO_OBJECT_DOT^#
  }
}
var globalAccessorDidSet3 = 42 {
didSet {
  #^GLOBAL_ACCESSOR_DIDSET_3?check=WITH_GLOBAL_DECLS;check=WITH_OLDVALUE^#
}
}

var globalAccessorInit1: Int = #^GLOBAL_ACCESSOR_INIT_1?check=WITH_GLOBAL_DECLS1^# {
}
var globalAccessorInit2: Int = #^GLOBAL_ACCESSOR_INIT_2?check=WITH_GLOBAL_DECLS1^# {
  get {}
}

struct MemberAccessors {
  var instanceVar: Double
  func instanceFunc(_ a: Int) -> Float { return 0.0 }

  static var staticVar: Int
  static func staticFunc0(_ a: Float) -> Int { return 0 }

  var memberAccessorImplicitGet1: Int {
    #^MEMBER_ACCESSOR_IMPLICIT_GET_1?check=WITH_MEMBER_DECLS^#
  }
  var memberAccessorImplicitGet2: Int {
    var fs = FooStruct()
    fs.#^MEMBER_ACCESSOR_IMPLICIT_GET_2?check=FOO_OBJECT_DOT^#
  }

  var memberAccessorGet1: Int {
    get {
      #^MEMBER_ACCESSOR_GET_1?check=WITH_MEMBER_DECLS^#
    }
  }
  var memberAccessorGet2: Int {
    get {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_GET_2?check=FOO_OBJECT_DOT^#
    }
  }

  var memberAccessorSet1: Int {
    set {
      #^MEMBER_ACCESSOR_SET_1?check=WITH_MEMBER_DECLS;check=WITH_NEWVALUE^#
    }
  }
  var memberAccessorSet2: Int {
    set {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_SET_2?check=FOO_OBJECT_DOT^#
    }
  }
  var memberAccessorSet3: Int {
    set(newValue) {
      #^MEMBER_ACCESSOR_SET_3?check=WITH_MEMBER_DECLS;check=WITH_NEWVALUE^#
    }
  }
  var memberAccessorSet4: Int {
    set(newValue) {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_SET_4?check=FOO_OBJECT_DOT^#
    }
  }

  var memberAccessorWillSet1: Int {
    willSet {
      #^MEMBER_ACCESSOR_WILLSET_1?check=WITH_MEMBER_DECLS;check=WITH_NEWVALUE^#
    }
  }
  var memberAccessorWillSet2: Int {
    willSet {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_WILLSET_2?check=FOO_OBJECT_DOT^#
    }
  }
  var memberAccessorWillSet3 = 42 {
    willSet {
      #^MEMBER_ACCESSOR_WILLSET_3?check=WITH_MEMBER_DECLS;check=WITH_NEWVALUE^#
    }
  }

  var memberAccessorDidSet1: Int {
    didSet {
      #^MEMBER_ACCESSOR_DIDSET_1?check=WITH_MEMBER_DECLS;check=WITH_OLDVALUE^#
    }
  }
  var memberAccessorDidSet2: Int {
    didSet {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_DIDSET_2?check=FOO_OBJECT_DOT^#
    }
  }
  var memberAccessorDidSet3 = 42 {
    didSet {
      #^MEMBER_ACCESSOR_DIDSET_3?check=WITH_MEMBER_DECLS;check=WITH_OLDVALUE^#
    }
  }

  var memberAccessorInit1: Int = #^MEMBER_ACCESSOR_INIT_1?check=WITH_MEMBER_DECLS_INIT;check=WITH_MEMBER_DECLS_INIT_WRONG^# {
  }
  var memberAccessorInit2: Int = #^MEMBER_ACCESSOR_INIT_2?check=WITH_MEMBER_DECLS_INIT;check=WITH_MEMBER_DECLS_INIT_WRONG^# {
    get {}
  }
}

func accessorsInFunction(_ functionParam: Int) {
  func localFunc(_ a: Int) -> Float { return 0.0 }

  var memberAccessorImplicitGet1: Int {
    #^LOCAL_ACCESSOR_IMPLICIT_GET_1?check=WITH_LOCAL_DECLS^#
  }
  var memberAccessorImplicitGet2: Int {
    var fs = FooStruct()
    fs.#^LOCAL_ACCESSOR_IMPLICIT_GET_2?check=FOO_OBJECT_DOT^#
  }

  var memberAccessorGet1: Int {
    get {
      #^LOCAL_ACCESSOR_GET_1?check=WITH_LOCAL_DECLS^#
    }
  }
  var memberAccessorGet2: Int {
    get {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_GET_2?check=FOO_OBJECT_DOT^#
    }
  }

  var memberAccessorSet1: Int {
    set {
      #^LOCAL_ACCESSOR_SET_1?check=WITH_LOCAL_DECLS;check=WITH_NEWVALUE^#
    }
  }
  var memberAccessorSet2: Int {
    set {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_SET_2?check=FOO_OBJECT_DOT^#
    }
  }
  var memberAccessorSet3: Int {
    set(newValue) {
      #^LOCAL_ACCESSOR_SET_3?check=WITH_LOCAL_DECLS;check=WITH_NEWVALUE^#
    }
  }
  var memberAccessorSet4: Int {
    set(newValue) {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_SET_4?check=FOO_OBJECT_DOT^#
    }
  }

  var memberAccessorWillSet1: Int {
    willSet {
      #^LOCAL_ACCESSOR_WILLSET_1?check=WITH_LOCAL_DECLS;check=WITH_NEWVALUE^#
    }
  }
  var memberAccessorWillSet2: Int {
    willSet {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_WILLSET_2?check=FOO_OBJECT_DOT^#
    }
  }
  var memberAccessorWillSet3 = 42 {
    willSet {
      #^LOCAL_ACCESSOR_WILLSET_3?check=WITH_LOCAL_DECLS;check=WITH_NEWVALUE^#
    }
  }

  var memberAccessorDidSet1: Int {
    didSet {
      #^LOCAL_ACCESSOR_DIDSET_1?check=WITH_LOCAL_DECLS;check=WITH_OLDVALUE^#
    }
  }
  var memberAccessorDidSet2: Int {
    didSet {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_DIDSET_2?check=FOO_OBJECT_DOT^#
    }
  }
  var memberAccessorDidSet3: Int {
    didSet {
      #^LOCAL_ACCESSOR_DIDSET_3?check=WITH_LOCAL_DECLS;check=WITH_OLDVALUE^#
    }
  }

  var globalAccessorInit1: Int = #^LOCAL_ACCESSOR_INIT_1?check=WITH_LOCAL_DECLS1^# {
  }
  var globalAccessorInit2: Int = #^LOCAL_ACCESSOR_INIT_2?check=WITH_LOCAL_DECLS1^# {
    get {}
  }
}

// ACCESSORS_IN_MEMBER_FUNC_1-DAG: Decl[LocalVar]/Local:             self[#AccessorsInMemberFunction#]
// ACCESSORS_IN_MEMBER_FUNC_1-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]:             functionParam[#Int#]
// ACCESSORS_IN_MEMBER_FUNC_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Double#]
// ACCESSORS_IN_MEMBER_FUNC_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Float#]

// ACCESSORS_IN_MEMBER_FUNC_2-DAG: Decl[LocalVar]/Local:            self[#AccessorsInMemberFunction#]
// ACCESSORS_IN_MEMBER_FUNC_2-DAG: Decl[LocalVar]/Local{{(/TypeRelation\[Convertible\])?}}:            functionParam[#Int#]
// ACCESSORS_IN_MEMBER_FUNC_2-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Double#]
// ACCESSORS_IN_MEMBER_FUNC_2-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Float#]

struct AccessorsInMemberFunction {
  var instanceVar: Double
  func instanceFunc(_ a: Int) -> Float { return 0.0 }

  static var staticVar: Int
  static func staticFunc0(_ a: Float) -> Int { return 0 }

  func accessorsInInstanceFunction1(_ functionParam: Int) {
    var x: Int = #^ACCESSOR_IN_MEMBER_FUNC_1?check=WITH_GLOBAL_DECLS1;check=ACCESSORS_IN_MEMBER_FUNC_1^# {
      get {}
    }
  }
  func accessorsInInstanceFunction2(_ functionParam: Int) {
    var x: Int {
      get {
        #^ACCESSOR_IN_MEMBER_FUNC_2?check=WITH_GLOBAL_DECLS;check=ACCESSORS_IN_MEMBER_FUNC_2^#
      }
    }
  }
}

var testImplicitOldValue1: Int = 0 {
  didSet {
    var oldV = oldValue
    #^IMPLICIT_OLDVALUE_COPIED^#
// IMPLICIT_OLDVALUE_COPIED-DAG: Decl[LocalVar]/Local:               oldV[#Int#];
// IMPLICIT_OLDVALUE_COPIED-DAG: Decl[LocalVar]/Local:               oldValue[#Int#];
  }
}
var testImplicitOldValue2: Int = 0 {
  didSet {
    oldValue.#^IMPLICIT_OLDVALUE_MEMBER^#
// IMPLICIT_OLDVALUE_MEMBER-DAG: Keyword[self]/CurrNominal:          self[#Int#];
  }
}
var testImplicitOldValue3: Int = 0 {
  didSet {
    var oldV = oldValue
    oldV.#^IMPLICIT_OLDVALUE_COPIEDMEMBER^#
// IMPLICIT_OLDVALUE_COPIEDMEMBER-DAG: Keyword[self]/CurrNominal:          self[#Int#];
  }
}

var testExplicitOldValue1: Int = 0 {
  didSet(oldVal) {
    var oldV = oldVal
    #^EXPLICIT_OLDVALUE_COPIED^#
// EXPLICIT_OLDVALUE_COPIED-NOT: oldValue
// EXPLICIT_OLDVALUE_COPIED-DAG: Decl[LocalVar]/Local:               oldV[#Int#];
// EXPLICIT_OLDVALUE_COPIED-DAG: Decl[LocalVar]/Local:               oldVal[#Int#];
// EXPLICIT_OLDVALUE_COPIED-NOT: oldValue
  }
}
var testExplicitOldValue2: Int = 0 {
  didSet(oldVal) {
    oldVal.#^EXPLICIT_OLDVALUE_MEMBER^#
// EXPLICIT_OLDVALUE_MEMBER-DAG: Keyword[self]/CurrNominal:          self[#Int#];
  }
}
var testExplicitOldValue3: Int = 0 {
  didSet(oldVal) {
    var oldV = oldVal
    oldV.#^EXPLICIT_OLDVALUE_COPIEDMEMBER^#
// EXPLICIT_OLDVALUE_COPIEDMEMBER-DAG: Keyword[self]/CurrNominal:          self[#Int#];
  }
}
