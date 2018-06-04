// RUN: sed -n -e '/VERIFY_BEGIN/,/VERIFY_END$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -verify -typecheck %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SUPER_NO_DOT_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_A_NO_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_NO_DOT_1 < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SUPER_DOT_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_A_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_DOT_1 < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONV_CONSTRUCTOR_SUPER_DOT > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_NOINIT < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONV_CONSTRUCTOR_SUPER_NO_DOT > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_NOINIT < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONV_CONSTRUCTOR_SUPER_PAREN > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_NOINIT < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SUPER_PAREN > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_NOINIT < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_FUNC_SUPER_PAREN > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_NOINIT < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SUPER_INIT_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_INIT_1 < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SUPER_INIT_PAREN_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_INIT_PAREN_1 < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DESTRUCTOR_SUPER_NO_DOT_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_A_NO_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=DESTRUCTOR_SUPER_NO_DOT_1 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DESTRUCTOR_SUPER_DOT_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_A_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=DESTRUCTOR_SUPER_DOT_1 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_SUPER_NO_DOT_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_A_NO_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=FUNC_SUPER_NO_DOT_1 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_SUPER_DOT_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_A_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=FUNC_SUPER_DOT_1 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SUPER_NO_DOT_2 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_B_NO_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_NO_DOT_2 < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONSTRUCTOR_SUPER_DOT_2 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_B_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=CONSTRUCTOR_SUPER_DOT_2 < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DESTRUCTOR_SUPER_NO_DOT_2 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_B_NO_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=DESTRUCTOR_SUPER_NO_DOT_2 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DESTRUCTOR_SUPER_DOT_2 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_B_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=DESTRUCTOR_SUPER_DOT_2 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_SUPER_NO_DOT_2 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_B_NO_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=FUNC_SUPER_NO_DOT_2 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FUNC_SUPER_DOT_2 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_B_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=FUNC_SUPER_DOT_2 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_FUNC_SUPER_NODOT > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CLASS_FUNC_SUPER < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_FUNC_SUPER_DOT > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CLASS_FUNC_SUPER < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_1 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_SUPER_DECLS < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_SUPER_DECLS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_3 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_3 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_SUPER_DECLS < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_4 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_4 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_SUPER_DECLS < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_5 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=SEMANTIC_CONTEXT_OVERRIDDEN_DECL_5 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_SUPER_DECLS < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_A_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=CLOSURE_1 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_2 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=COMMON_BASE_A_DOT < %t.super.txt
// RUN: %FileCheck %s -check-prefix=CLOSURE_2 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_CAPTURE_1 > %t.super.txt
// RUN: %FileCheck %s -check-prefix=CLOSURE_CAPTURE_1 < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_SUPER_DECLS < %t.super.txt
// RUN: %FileCheck %s -check-prefix=NO_CONSTRUCTORS < %t.super.txt

// NO_CONSTRUCTORS-NOT: init(

// NO_SUPER_DECLS-NOT: Decl/Super

//===---
//===--- Tests for code completion after 'super'.
//===---

//===--- Testcase A, with implicit constructors in the base class.

// VERIFY_BEGIN

class SuperBaseA {
  var baseInstanceVar = 0

  var baseProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  // Don't declare constructors.

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

  // expected-error@+1 {{class stored properties not supported}}
  class var baseStaticVar: Int = 0

  class var baseStaticProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  class func baseStaticFunc0() {}

  struct BaseNestedStruct {}
  class BaseNestedClass {}
  enum BaseNestedEnum {
    case BaseEnumX(Int)
  }

  typealias BaseNestedTypealias = Int
}

extension SuperBaseA {
  var baseExtProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  func baseExtFunc0() {}

  // expected-error@+1 {{class stored properties not supported}}
  class var baseExtStaticVar: Int = 0

  class func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  enum BaseExtNestedEnum {
    case BaseExtEnumX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

// VERIFY_END

// COMMON_BASE_A_NO_DOT: Begin completions
// COMMON_BASE_A_NO_DOT-DAG: Decl[InstanceVar]/CurrNominal:    .baseInstanceVar[#Int#]{{; name=.+$}}
// COMMON_BASE_A_NO_DOT-DAG: Decl[InstanceVar]/CurrNominal:    .baseProp[#Int#]{{; name=.+$}}
// COMMON_BASE_A_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal: .baseFunc0()[#Void#]{{; name=.+$}}
// COMMON_BASE_A_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal: .baseFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// COMMON_BASE_A_NO_DOT-DAG: Decl[Subscript]/CurrNominal:      [{#Int#}][#Double#]{{; name=.+$}}
// COMMON_BASE_A_NO_DOT-DAG: Decl[InstanceVar]/CurrNominal:    .baseExtProp[#Int#]{{; name=.+$}}
// COMMON_BASE_A_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal: .baseExtFunc0()[#Void#]{{; name=.+$}}
// COMMON_BASE_A_NO_DOT: End completions

// COMMON_BASE_A_DOT: Begin completions
// COMMON_BASE_A_DOT-DAG: Decl[InstanceVar]/CurrNominal:    baseInstanceVar[#Int#]{{; name=.+$}}
// COMMON_BASE_A_DOT-DAG: Decl[InstanceVar]/CurrNominal:    baseProp[#Int#]{{; name=.+$}}
// COMMON_BASE_A_DOT-DAG: Decl[InstanceMethod]/CurrNominal: baseFunc0()[#Void#]{{; name=.+$}}
// COMMON_BASE_A_DOT-DAG: Decl[InstanceMethod]/CurrNominal: baseFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// COMMON_BASE_A_DOT-DAG: Decl[InstanceVar]/CurrNominal:    baseExtProp[#Int#]{{; name=.+$}}
// COMMON_BASE_A_DOT-DAG: Decl[InstanceMethod]/CurrNominal: baseExtFunc0()[#Void#]{{; name=.+$}}
// COMMON_BASE_A_DOT: End completions

class SuperDerivedA : SuperBaseA {
  var derivedInstanceVar: Int

  func derivedFunc0() {}

  init() {
    super#^CONSTRUCTOR_SUPER_NO_DOT_1^#
// CONSTRUCTOR_SUPER_NO_DOT_1: Begin completions, 8 items
// CONSTRUCTOR_SUPER_NO_DOT_1-DAG: Decl[Constructor]/ExprSpecific: .init()[#SuperBaseA#]{{; name=.+$}}
// CONSTRUCTOR_SUPER_NO_DOT_1: End completions
  }

  init(a: Int) {
    super.#^CONSTRUCTOR_SUPER_DOT_1^#
    super(#^CONSTRUCTOR_SUPER_PAREN^#
// CONSTRUCTOR_SUPER_DOT_1: Begin completions, 7 items
// CONSTRUCTOR_SUPER_DOT_1-DAG: Decl[Constructor]/CurrNominal: init()[#SuperBaseA#]{{; name=.+$}}
// CONSTRUCTOR_SUPER_DOT_1: End completions
  }

  convenience init(foo1: Int) {
    super.#^CONV_CONSTRUCTOR_SUPER_DOT^#
    super#^CONV_CONSTRUCTOR_SUPER_NO_DOT^#
    super(#^CONV_CONSTRUCTOR_SUPER_PAREN^#
// CONSTRUCTOR_SUPER_NOINIT-NOT: Decl[Constructor]
  }

  init (a: Float) {
    super.init#^CONSTRUCTOR_SUPER_INIT_1^#
// CONSTRUCTOR_SUPER_INIT_1: Begin completions
// CONSTRUCTOR_SUPER_INIT_1-DAG: Pattern/CurrModule: ()[#SuperBaseA#];
// CONSTRUCTOR_SUPER_INIT_1: End completions
  }
  init (b: Float) {
    super.init(#^CONSTRUCTOR_SUPER_INIT_PAREN_1^#
// CONSTRUCTOR_SUPER_INIT_PAREN_1-NOT: Pattern/
  }

  deinit {
    super#^DESTRUCTOR_SUPER_NO_DOT_1^#
// DESTRUCTOR_SUPER_NO_DOT_1: Begin completions, 7 items
// DESTRUCTOR_SUPER_NO_DOT_1: End completions

    var resyncParser = 42

    super.#^DESTRUCTOR_SUPER_DOT_1^#
// DESTRUCTOR_SUPER_DOT_1: Begin completions, 6 items
// DESTRUCTOR_SUPER_DOT_1: End completions
  }

  func test1() {
    super#^FUNC_SUPER_NO_DOT_1^#
// FUNC_SUPER_NO_DOT_1: Begin completions, 7 items
// FUNC_SUPER_NO_DOT_1: End completions
  }

  func test2() {
    super.#^FUNC_SUPER_DOT_1^#
// FUNC_SUPER_DOT_1: Begin completions, 6 items
// FUNC_SUPER_DOT_1: End completions
  }
}

//===--- Testcase B, with explicit constructors in the base class.

// COMMON_BASE_B_NO_DOT: Begin completions
// COMMON_BASE_B_NO_DOT-DAG: Decl[InstanceVar]/CurrNominal:    .baseInstanceVar[#Int#]{{; name=.+$}}
// COMMON_BASE_B_NO_DOT-DAG: Decl[InstanceVar]/CurrNominal:    .baseProp[#Int#]{{; name=.+$}}
// COMMON_BASE_B_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal: .baseFunc0()[#Void#]{{; name=.+$}}
// COMMON_BASE_B_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal: .baseFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// COMMON_BASE_B_NO_DOT-DAG: Decl[Subscript]/CurrNominal:      [{#Int#}][#Double#]{{; name=.+$}}
// COMMON_BASE_B_NO_DOT-DAG: Decl[InstanceVar]/CurrNominal:    .baseExtProp[#Int#]{{; name=.+$}}
// COMMON_BASE_B_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal: .baseExtFunc0()[#Void#]{{; name=.+$}}
// COMMON_BASE_B_NO_DOT: End completions

// COMMON_BASE_B_DOT: Begin completions
// COMMON_BASE_B_DOT-DAG: Decl[InstanceVar]/CurrNominal:    baseInstanceVar[#Int#]{{; name=.+$}}
// COMMON_BASE_B_DOT-DAG: Decl[InstanceVar]/CurrNominal:    baseProp[#Int#]{{; name=.+$}}
// COMMON_BASE_B_DOT-DAG: Decl[InstanceMethod]/CurrNominal: baseFunc0()[#Void#]{{; name=.+$}}
// COMMON_BASE_B_DOT-DAG: Decl[InstanceMethod]/CurrNominal: baseFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// COMMON_BASE_B_DOT-DAG: Decl[InstanceVar]/CurrNominal:    baseExtProp[#Int#]{{; name=.+$}}
// COMMON_BASE_B_DOT-DAG: Decl[InstanceMethod]/CurrNominal: baseExtFunc0()[#Void#]{{; name=.+$}}
// COMMON_BASE_B_DOT: End completions

// VERIFY_BEGIN

class SuperBaseB {
  var baseInstanceVar: Int

  var baseProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  init() {}
  init(a: Double) {}
  init(int: Int) {}

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

  // expected-error@+1 {{class stored properties not supported}}
  class var baseStaticVar: Int = 0

  class var baseStaticProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  class func baseStaticFunc0() {}

  struct BaseNestedStruct {}
  class BaseNestedClass {}
  enum BaseNestedEnum {
    case BaseEnumX(Int)
  }

  typealias BaseNestedTypealias = Int
}

extension SuperBaseB {
  var baseExtProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  func baseExtFunc0() {}

  // expected-error@+1 {{class stored properties not supported}}
  class var baseExtStaticVar: Int = 0

  class var baseExtStaticProp: Int {
    get {
      return 42
    }
    set(v) {}
  }

  class func baseExtStaticFunc0() {}

  struct BaseExtNestedStruct {}
  class BaseExtNestedClass {}
  enum BaseExtNestedEnum {
    case BaseExtEnumX(Int)
  }

  typealias BaseExtNestedTypealias = Int
}

// VERIFY_END

class SuperDerivedB : SuperBaseB {
  var derivedInstanceVar: Int

  func derivedFunc0() {}

  init() {
    super#^CONSTRUCTOR_SUPER_NO_DOT_2^#
// CONSTRUCTOR_SUPER_NO_DOT_2: Begin completions, 10 items
// CONSTRUCTOR_SUPER_NO_DOT_2-DAG: Decl[Constructor]/ExprSpecific: .init()[#SuperBaseB#]{{; name=.+$}}
// CONSTRUCTOR_SUPER_NO_DOT_2-DAG: Decl[Constructor]/CurrNominal: .init({#a: Double#})[#SuperBaseB#]{{; name=.+$}}
// CONSTRUCTOR_SUPER_NO_DOT_2-DAG: Decl[Constructor]/CurrNominal: .init({#int: Int#})[#SuperBaseB#]{{; name=.+$}}
// CONSTRUCTOR_SUPER_NO_DOT_2: End completions
  }

  init(int a: Int) {
    super.#^CONSTRUCTOR_SUPER_DOT_2^#
// CONSTRUCTOR_SUPER_DOT_2: Begin completions, 9 items
// CONSTRUCTOR_SUPER_DOT_2-DAG: Decl[Constructor]/CurrNominal: init()[#SuperBaseB#]{{; name=.+$}}
// CONSTRUCTOR_SUPER_DOT_2-DAG: Decl[Constructor]/CurrNominal: init({#a: Double#})[#SuperBaseB#]{{; name=.+$}}
// CONSTRUCTOR_SUPER_DOT_2-DAG: Decl[Constructor]/ExprSpecific: init({#int: Int#})[#SuperBaseB#]{{; name=.+$}}
// CONSTRUCTOR_SUPER_DOT_2: End completions
  }

  deinit {
    super#^DESTRUCTOR_SUPER_NO_DOT_2^#
// DESTRUCTOR_SUPER_NO_DOT_2: Begin completions, 7 items
// DESTRUCTOR_SUPER_NO_DOT_2: End completions

    var resyncParser = 42

    super.#^DESTRUCTOR_SUPER_DOT_2^#
// DESTRUCTOR_SUPER_DOT_2: Begin completions, 6 items
// DESTRUCTOR_SUPER_DOT_2: End completions
  }

  func test1() {
    super#^FUNC_SUPER_NO_DOT_2^#
// FUNC_SUPER_NO_DOT_2: Begin completions, 7 items
// FUNC_SUPER_NO_DOT_2: End completions
  }

  func test2() {
    super.#^FUNC_SUPER_DOT_2^#
// FUNC_SUPER_DOT_2: Begin completions, 6 items
// FUNC_SUPER_DOT_2: End completions
  }

  class func test3() {
    super#^CLASS_FUNC_SUPER_NODOT^#
  }
  class func test4() {
    super.#^CLASS_FUNC_SUPER_DOT^#
  }
  class func test5() {
    super(#^CLASS_FUNC_SUPER_PAREN^#
  }
// CLASS_FUNC_SUPER: Decl[Constructor]/CurrNominal: {{.init|init}}()[#SuperBaseB#]
// CLASS_FUNC_SUPER: Decl[Constructor]/CurrNominal: {{.init|init}}({#a: Double#})[#SuperBaseB#]
// CLASS_FUNC_SUPER: Decl[Constructor]/CurrNominal: {{.init|init}}({#int: Int#})[#SuperBaseB#]
}

//===--- Check that we assign a special semantic context to the overridden decl.

class SemanticContextBase1 {
  init() {}
  init(a: Int) {}
  func instanceFunc1() {}
  func instanceFunc1(_ a: Int) {}
}

class SemanticContextDerived1 : SemanticContextBase1 {
  init() {}
  init(a: Int) {
    #^SEMANTIC_CONTEXT_OVERRIDDEN_DECL_1^#
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_1: Begin completions
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1()[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_1: End completions

    super.#^SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2^#
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2: Begin completions
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2-NEXT: Decl[Constructor]/CurrNominal:    init()[#SemanticContextBase1#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2-NEXT: Decl[Constructor]/ExprSpecific:    init({#a: Int#})[#SemanticContextBase1#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc1()[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_2-NEXT: End completions
  }
  func instanceFunc1() {
    #^SEMANTIC_CONTEXT_OVERRIDDEN_DECL_3^#
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_3: Begin completions
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_3-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1()[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_3-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_3: End completions

    super.#^SEMANTIC_CONTEXT_OVERRIDDEN_DECL_4^#
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_4: Begin completions
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_4-NEXT: Decl[InstanceMethod]/ExprSpecific: instanceFunc1()[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_4-NEXT: Decl[InstanceMethod]/CurrNominal:  instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_4-NEXT: End completions
  }
  func instanceFunc1(_ a: Int) {
    super.#^SEMANTIC_CONTEXT_OVERRIDDEN_DECL_5^#
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_5: Begin completions
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_5-NEXT: Decl[InstanceMethod]/CurrNominal:  instanceFunc1()[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_5-NEXT: Decl[InstanceMethod]/ExprSpecific: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// SEMANTIC_CONTEXT_OVERRIDDEN_DECL_5-NEXT: End completions
  }
}

class Closures : SuperBaseA {
  func foo() {
    func inner() {
      super.#^CLOSURE_1^#
      // CLOSURE_1: Begin completions, 6 items
      // CLOSURE_1: End completions
    }
  }

  func bar() {
    let inner = { () -> Void in
      // CLOSURE_2: Begin completions, 6 items
      // CLOSURE_2: End completions
      super.#^CLOSURE_2^#
    }
  }

  func baz() {
    let inner = { [weak self] in
      super.#^CLOSURE_CAPTURE_1^#
      // CLOSURE_CAPTURE_1-NOT: Begin completions
    }
  }
}


//===--- Code completion for 'super' keyword itself.

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
