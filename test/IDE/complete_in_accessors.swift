// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_IMPLICIT_GET_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_IMPLICIT_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_GET_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_SET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_VALUE < %t.txt
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_SET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_SET_3 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_SET_4 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_WILLSET_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_WILLSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_DIDSET_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_ACCESSOR_DIDSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_IMPLICIT_GET_1 | FileCheck %s -check-prefix=WITH_MEMBER_DECLS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_IMPLICIT_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_GET_1 | FileCheck %s -check-prefix=WITH_MEMBER_DECLS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_GET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_SET_1 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_VALUE < %t.txt
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_SET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_SET_3 > %t.txt
// RUN: FileCheck %s -check-prefix=WITH_MEMBER_DECLS < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_NEWVALUE < %t.txt
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_SET_4 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_WILLSET_1 | FileCheck %s -check-prefix=WITH_MEMBER_DECLS
// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_WILLSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_DIDSET_1 | FileCheck %s -check-prefix=WITH_MEMBER_DECLS
// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER_ACCESSOR_DIDSET_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

//===--- Helper types that are used in this test

struct FooStruct {
  var instanceVar: Int = 0

  func instanceFunc0() {}
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
// WITH_MEMBER_DECLS-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#a: Int#})[#Float#]{{$}}
// WITH_MEMBER_DECLS: End completions

// WITH_VALUE: Begin completions
// WITH_VALUE-DAG: Decl[LocalVar]/Local: value[#Int#]{{$}}
// WITH_VALUE: End completions

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

struct MemberAccessors {
  var instanceVar: Double
  func instanceFunc(a: Int) -> Float { return 0.0 }

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
/*
  FIXME
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
*/
}

// FIXME: add tests for local properties inside a function.

