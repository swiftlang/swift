// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_IMPLICIT_GET_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_IMPLICIT_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_GET_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_SET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_SET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_SET_3 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_SET_4 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_WILLSET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_WILLSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_DIDSET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_OLDVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_DIDSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_INIT_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_INIT_2 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_IMPLICIT_GET_1 | FileCheck %s -check-prefix=WITH_MEMBER_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_IMPLICIT_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_GET_1 | FileCheck %s -check-prefix=WITH_MEMBER_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_SET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_SET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_SET_3 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_SET_4 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_WILLSET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_WILLSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_DIDSET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_OLDVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_DIDSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_INIT_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS_INIT < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS_INIT_WRONG < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_INIT_2 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS_INIT < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS_INIT_WRONG < %t.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_IMPLICIT_GET_1 | FileCheck %s -check-prefix=WITH_LOCAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_IMPLICIT_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_GET_1 | FileCheck %s -check-prefix=WITH_LOCAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_SET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_LOCAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_SET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_SET_3 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_LOCAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_SET_4 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_WILLSET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_LOCAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_WILLSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_DIDSET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_LOCAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_OLDVALUE < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_DIDSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_INIT_1 | FileCheck %s -check-prefix=WITH_LOCAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LOCAL_ACCESSOR_INIT_2 | FileCheck %s -check-prefix=WITH_LOCAL_DECLS

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ACCESSOR_IN_MEMBER_FUNC_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=ACCESSORS_IN_MEMBER_FUNC_1 < %t.txt
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ACCESSOR_IN_MEMBER_FUNC_2 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=ACCESSORS_IN_MEMBER_FUNC_2 < %t.txt

//===--- Helper types that are used in this test

struct FooStruct {
  var instanceVar: Int = 0
  func instanceFunc0() {}
  static var staticVar: Int
  static func staticFunc0() {}
}

func returnsInt() -> Int {}

// FOO_OBJECT_DOT: Begin completions
// FOO_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{$}}
// FOO_OBJECT_DOT-NEXT: End completions

// WITH_GLOBAL_DECLS: Begin completions
// WITH_GLOBAL_DECLS-DAG: Decl[Struct]/CurrModule:       FooStruct[#FooStruct#]{{$}}
// WITH_GLOBAL_DECLS-DAG: Decl[FreeFunction]/CurrModule: returnsInt()[#Int#]{{$}}
// WITH_GLOBAL_DECLS: End completions

// WITH_MEMBER_DECLS: Begin completions
// WITH_MEMBER_DECLS-DAG: Decl[Struct]/CurrModule:          FooStruct[#FooStruct#]{{$}}
// WITH_MEMBER_DECLS-DAG: Decl[FreeFunction]/CurrModule:    returnsInt()[#Int#]{{$}}
// WITH_MEMBER_DECLS-DAG: Decl[LocalVar]/Local:             self[#MemberAccessors#]{{$}}
// WITH_MEMBER_DECLS-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Double#]{{$}}
// WITH_MEMBER_DECLS-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Float#]{{$}}
// WITH_MEMBER_DECLS: End completions

// WITH_MEMBER_DECLS_INIT: Begin completions
// WITH_MEMBER_DECLS_INIT-DAG: Decl[Struct]/CurrModule:          FooStruct[#FooStruct#]{{$}}
// WITH_MEMBER_DECLS_INIT-DAG: Decl[FreeFunction]/CurrModule:    returnsInt()[#Int#]{{$}}
// WITH_MEMBER_DECLS_INIT-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#self: MemberAccessors#})[#(Int) -> Float#]{{$}}
// WITH_MEMBER_DECLS_INIT: End completions

// WITH_MEMBER_DECLS_INIT_WRONG-NOT: self[
// WITH_MEMBER_DECLS_INIT_WRONG-NOT: instanceVar

// WITH_LOCAL_DECLS: Begin completions
// WITH_LOCAL_DECLS-DAG: Decl[Struct]/CurrModule:          FooStruct[#FooStruct#]{{$}}
// WITH_LOCAL_DECLS-DAG: Decl[FreeFunction]/CurrModule:    returnsInt()[#Int#]{{$}}
// WITH_LOCAL_DECLS-DAG: Decl[LocalVar]/Local:             functionParam[#Int#]{{$}}
// WITH_LOCAL_DECLS-DAG: Decl[FreeFunction]/Local:         localFunc({#(a): Int#})[#Float#]{{$}}
// WITH_LOCAL_DECLS: End completions


// WITH_OLDVALUE: Begin completions
// WITH_OLDVALUE-DAG: Decl[LocalVar]/Local: oldValue[#Int#]{{$}}
// WITH_OLDVALUE: End completions

// WITH_NEWVALUE: Begin completions
// WITH_NEWVALUE-DAG: Decl[LocalVar]/Local: newValue[#Int#]{{$}}
// WITH_NEWVALUE: End completions


//===--- Check that we can complete inside accessors.

// Each test comes in two variants: a basic one, with global completions, and
// another one with a local variable to make sure that we type check accessor
// bodies.

var globalAccessorImplicitGet1: Int {
  #^GLOBAL_ACCESSOR_IMPLICIT_GET_1^#
}
var globalAccessorImplicitGet2: Int {
  var fs = FooStruct()
  fs.#^GLOBAL_ACCESSOR_IMPLICIT_GET_2^#
}

var globalAccessorGet1: Int {
  get {
    #^GLOBAL_ACCESSOR_GET_1^#
  }
}
var globalAccessorGet2: Int {
  get {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_GET_2^#
  }
}

var globalAccessorSet1: Int {
  set {
    #^GLOBAL_ACCESSOR_SET_1^#
  }
}
var globalAccessorSet2: Int {
  set {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_SET_2^#
  }
}
var globalAccessorSet3: Int {
  set(newValue) {
    #^GLOBAL_ACCESSOR_SET_3^#
  }
}
var globalAccessorSet4: Int {
  set(newValue) {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_SET_4^#
  }
}

var globalAccessorWillSet1: Int {
  willSet {
    #^GLOBAL_ACCESSOR_WILLSET_1^#
  }
}
var globalAccessorWillSet2: Int {
  willSet {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_WILLSET_2^#
  }
}

var globalAccessorDidSet1: Int {
  didSet {
    #^GLOBAL_ACCESSOR_DIDSET_1^#
  }
}
var globalAccessorDidSet2: Int {
  didSet {
    var fs = FooStruct()
    fs.#^GLOBAL_ACCESSOR_DIDSET_2^#
  }
}

var globalAccessorInit1: Int = #^GLOBAL_ACCESSOR_INIT_1^# {
}
var globalAccessorInit2: Int = #^GLOBAL_ACCESSOR_INIT_2^# {
  get {}
}

struct MemberAccessors {
  var instanceVar: Double
  func instanceFunc(a: Int) -> Float { return 0.0 }

  static var staticVar: Int
  static func staticFunc0(a: Float) -> Int { return 0 }

  var memberAccessorImplicitGet1: Int {
    #^MEMBER_ACCESSOR_IMPLICIT_GET_1^#
  }
  var memberAccessorImplicitGet2: Int {
    var fs = FooStruct()
    fs.#^MEMBER_ACCESSOR_IMPLICIT_GET_2^#
  }

  var memberAccessorGet1: Int {
    get {
      #^MEMBER_ACCESSOR_GET_1^#
    }
  }
  var memberAccessorGet2: Int {
    get {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_GET_2^#
    }
  }

  var memberAccessorSet1: Int {
    set {
      #^MEMBER_ACCESSOR_SET_1^#
    }
  }
  var memberAccessorSet2: Int {
    set {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_SET_2^#
    }
  }
  var memberAccessorSet3: Int {
    set(newValue) {
      #^MEMBER_ACCESSOR_SET_3^#
    }
  }
  var memberAccessorSet4: Int {
    set(newValue) {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_SET_4^#
    }
  }

  var memberAccessorWillSet1: Int {
    willSet {
      #^MEMBER_ACCESSOR_WILLSET_1^#
    }
  }
  var memberAccessorWillSet2: Int {
    willSet {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_WILLSET_2^#
    }
  }

  var memberAccessorDidSet1: Int {
    didSet {
      #^MEMBER_ACCESSOR_DIDSET_1^#
    }
  }
  var memberAccessorDidSet2: Int {
    didSet {
      var fs = FooStruct()
      fs.#^MEMBER_ACCESSOR_DIDSET_2^#
    }
  }

  var memberAccessorInit1: Int = #^MEMBER_ACCESSOR_INIT_1^# {
  }
  var memberAccessorInit2: Int = #^MEMBER_ACCESSOR_INIT_2^# {
    get {}
  }
}

func accessorsInFunction(functionParam: Int) {
  func localFunc(a: Int) -> Float { return 0.0 }

  var memberAccessorImplicitGet1: Int {
    #^LOCAL_ACCESSOR_IMPLICIT_GET_1^#
  }
  var memberAccessorImplicitGet2: Int {
    var fs = FooStruct()
    fs.#^LOCAL_ACCESSOR_IMPLICIT_GET_2^#
  }

  var memberAccessorGet1: Int {
    get {
      #^LOCAL_ACCESSOR_GET_1^#
    }
  }
  var memberAccessorGet2: Int {
    get {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_GET_2^#
    }
  }

  var memberAccessorSet1: Int {
    set {
      #^LOCAL_ACCESSOR_SET_1^#
    }
  }
  var memberAccessorSet2: Int {
    set {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_SET_2^#
    }
  }
  var memberAccessorSet3: Int {
    set(newValue) {
      #^LOCAL_ACCESSOR_SET_3^#
    }
  }
  var memberAccessorSet4: Int {
    set(newValue) {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_SET_4^#
    }
  }

  var memberAccessorWillSet1: Int {
    willSet {
      #^LOCAL_ACCESSOR_WILLSET_1^#
    }
  }
  var memberAccessorWillSet2: Int {
    willSet {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_WILLSET_2^#
    }
  }

  var memberAccessorDidSet1: Int {
    didSet {
      #^LOCAL_ACCESSOR_DIDSET_1^#
    }
  }
  var memberAccessorDidSet2: Int {
    didSet {
      var fs = FooStruct()
      fs.#^LOCAL_ACCESSOR_DIDSET_2^#
    }
  }

  var globalAccessorInit1: Int = #^LOCAL_ACCESSOR_INIT_1^# {
  }
  var globalAccessorInit2: Int = #^LOCAL_ACCESSOR_INIT_2^# {
    get {}
  }
}

// ACCESSORS_IN_MEMBER_FUNC_1: Begin completions
// ACCESSORS_IN_MEMBER_FUNC_1-DAG: Decl[LocalVar]/Local:             self[#AccessorsInMemberFunction#]
// ACCESSORS_IN_MEMBER_FUNC_1-DAG: Decl[LocalVar]/Local:             functionParam[#Int#]
// ACCESSORS_IN_MEMBER_FUNC_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Double#]
// ACCESSORS_IN_MEMBER_FUNC_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(a): Int#})[#Float#]
// ACCESSORS_IN_MEMBER_FUNC_1: End completions

// ACCESSORS_IN_MEMBER_FUNC_2: Begin completions
// ACCESSORS_IN_MEMBER_FUNC_2-DAG: Decl[LocalVar]/Local:            self[#AccessorsInMemberFunction#]
// ACCESSORS_IN_MEMBER_FUNC_2-DAG: Decl[LocalVar]/Local:            functionParam[#Int#]
// ACCESSORS_IN_MEMBER_FUNC_2-DAG: Decl[InstanceVar]/OutNominal:    instanceVar[#Double#]
// ACCESSORS_IN_MEMBER_FUNC_2-DAG: Decl[InstanceMethod]/OutNominal: instanceFunc({#(a): Int#})[#Float#]
// ACCESSORS_IN_MEMBER_FUNC_2: End completions

struct AccessorsInMemberFunction {
  var instanceVar: Double
  func instanceFunc(a: Int) -> Float { return 0.0 }

  static var staticVar: Int
  static func staticFunc0(a: Float) -> Int { return 0 }

  func accessorsInInstanceFunction1(functionParam: Int) {
    var x: Int = #^ACCESSOR_IN_MEMBER_FUNC_1^# {
      get {}
    }
  }
  func accessorsInInstanceFunction2(functionParam: Int) {
    var x: Int {
      get {
        #^ACCESSOR_IN_MEMBER_FUNC_2^#
      }
    }
  }
}

