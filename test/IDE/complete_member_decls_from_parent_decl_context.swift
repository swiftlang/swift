// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_INSTANCE_METHOD_1 | FileCheck %s -check-prefix=IN_CLASS_INSTANCE_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_CLASS_STATIC_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_CLASS_STATIC_METHOD_1_NEGATIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_CONSTRUCTOR_1 | FileCheck %s -check-prefix=IN_CLASS_CONSTRUCTOR_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_DESTRUCTOR_1 | FileCheck %s -check-prefix=IN_CLASS_DESTRUCTOR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_INSTANCE_METHOD_1 | FileCheck %s -check-prefix=IN_STRUCT_INSTANCE_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_STRUCT_STATIC_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_STRUCT_STATIC_METHOD_1_NEGATIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_CONSTRUCTOR_1 | FileCheck %s -check-prefix=IN_STRUCT_CONSTRUCTOR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_1 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_2 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_3 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_4 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_5 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_1 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_2 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_3 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_4 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_5 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_1 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_2 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_3 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_4 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_5 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_D_1 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_D_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_E_1 | FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_E_1

//===---
//===--- Test that we can code complete in methods, and correctly distinguish
//===--- static and non-static contexts.
//===---

class CodeCompletionInClassMethods1 {
  /// @{ Members.
  /// Warning: there are negative tests about code completion of instance
  /// members of this class.  Read the tests below before adding, removing or
  /// modifying members.

  var instanceVar: Int

  func instanceFunc0() {}
  func instanceFunc1(a: Int) {}

  subscript(i: Int) -> Double {
    get {
      return Double(i)
    }
    set(v) {
      instanceVar = i
    }
  }

  struct NestedStruct {}
  class NestedClass {}
  enum NestedEnum {}
  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  class var staticVar: Int

  class func staticFunc0() {}
  class func staticFunc1(a: Int) {}

  /// @} Members.

  /// @{ Tests.

  func instanceTest1() {
    #^IN_CLASS_INSTANCE_METHOD_1^#
// IN_CLASS_INSTANCE_METHOD_1: Begin completions
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInClassMethods1#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#CodeCompletionInClassMethods1.NestedStruct#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#CodeCompletionInClassMethods1.NestedClass#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#CodeCompletionInClassMethods1.NestedEnum#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1: End completions
  }

  class func staticTest1() {
    #^IN_CLASS_STATIC_METHOD_1^#
// Negative tests.
// IN_CLASS_STATIC_METHOD_1_NEGATIVE-NOT: instanceVar
//
// Positive tests.
// IN_CLASS_STATIC_METHOD_1: Begin completions
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInClassMethods1.Type#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0({#self: CodeCompletionInClassMethods1#})[#() -> Void#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#self: CodeCompletionInClassMethods1#})[#(Int) -> Void#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#CodeCompletionInClassMethods1.NestedStruct#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#CodeCompletionInClassMethods1.NestedClass#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#CodeCompletionInClassMethods1.NestedEnum#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[StaticMethod]/CurrNominal:   staticFunc0()[#Void#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[StaticMethod]/CurrNominal:   staticFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1: End completions
  }

  init() {
    #^IN_CLASS_CONSTRUCTOR_1^#
// IN_CLASS_CONSTRUCTOR_1: Begin completions
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInClassMethods1#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#CodeCompletionInClassMethods1.NestedStruct#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#CodeCompletionInClassMethods1.NestedClass#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#CodeCompletionInClassMethods1.NestedEnum#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1: End completions
  }

  deinit {
    #^IN_CLASS_DESTRUCTOR_1^#
// IN_CLASS_DESTRUCTOR_1: Begin completions
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInClassMethods1#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#CodeCompletionInClassMethods1.NestedStruct#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#CodeCompletionInClassMethods1.NestedClass#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#CodeCompletionInClassMethods1.NestedEnum#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1: End completions
  }

  /// @}
}

struct CodeCompletionInStructMethods1 {
  /// @{ Members.
  /// Warning: there are negative tests about code completion of instance
  /// members of this struct.  Read the tests below before adding, removing or
  /// modifying members.

  var instanceVar: Int

  mutating
  func instanceFunc0() {}
  mutating
  func instanceFunc1(a: Int) {}

  subscript(i: Int) -> Double {
    get {
      return Double(i)
    }
    set(v) {
      instanceVar = i
    }
  }

  struct NestedStruct {}
  class NestedClass {}
  enum NestedEnum {}
  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  static var staticVar: Int

  static func staticFunc0() {}
  static func staticFunc1(a: Int) {}

  /// @} Members.

  func instanceTest1() {
    #^IN_STRUCT_INSTANCE_METHOD_1^#
// IN_STRUCT_INSTANCE_METHOD_1: Begin completions
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInStructMethods1#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#CodeCompletionInStructMethods1.NestedStruct#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#CodeCompletionInStructMethods1.NestedClass#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#CodeCompletionInStructMethods1.NestedEnum#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1: End completions
  }

  static func staticTest1() {
    #^IN_STRUCT_STATIC_METHOD_1^#
// Negative tests.
// IN_STRUCT_STATIC_METHOD_1_NEGATIVE-NOT: instanceVar
//
// Positive tests.
// IN_STRUCT_STATIC_METHOD_1: Begin completions
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInStructMethods1.Type#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0({#self: &CodeCompletionInStructMethods1#})[#() -> Void#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#self: &CodeCompletionInStructMethods1#})[#(Int) -> Void#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#CodeCompletionInStructMethods1.NestedStruct#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#CodeCompletionInStructMethods1.NestedClass#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#CodeCompletionInStructMethods1.NestedEnum#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[StaticMethod]/CurrNominal:   staticFunc0()[#Void#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[StaticMethod]/CurrNominal:   staticFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1: End completions
  }

  init() {
    #^IN_STRUCT_CONSTRUCTOR_1^#
// IN_STRUCT_CONSTRUCTOR_1: Begin completions
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInStructMethods1#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#CodeCompletionInStructMethods1.NestedStruct#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#CodeCompletionInStructMethods1.NestedClass#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#CodeCompletionInStructMethods1.NestedEnum#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1: End completions
  }
}

//===---
//===--- Test that code completion works in non-toplevel nominal type decls.
//===---

struct NestedOuter1 {
  mutating
  func testInstanceFunc() {
    struct NestedInnerA {
      mutating
      func aTestInstaceFunc() {
        #^NESTED_NOMINAL_DECL_A_1^#
// NESTED_NOMINAL_DECL_A_1: Begin completions
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[LocalVar]/Local:             self[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceMethod]/CurrNominal: aTestInstaceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceVar]/CurrNominal:    aInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceMethod]/CurrNominal: aInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Struct]/CurrNominal:         NestedInnerAStruct[#NestedInnerA.NestedInnerAStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Class]/CurrNominal:          NestedInnerAClass[#NestedInnerA.NestedInnerAClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Enum]/CurrNominal:           NestedInnerAEnum[#NestedInnerA.NestedInnerAEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerATypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Struct]/Local:               NestedInnerA[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// FIXME: the following decls are wrong.
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[LocalVar]/Local:             self[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceMethod]/OutNominal:  testInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceVar]/OutNominal:     outerInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceMethod]/OutNominal:  outerInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1: End completions
      }
      static func aTestStaticFunc() {
        #^NESTED_NOMINAL_DECL_A_2^#
// NESTED_NOMINAL_DECL_A_2: Begin completions
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[LocalVar]/Local:             self[#NestedInnerA.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[InstanceMethod]/CurrNominal: aTestInstaceFunc({#self: &NestedInnerA#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[StaticMethod]/CurrNominal:   aTestStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[InstanceMethod]/CurrNominal: aInstanceFunc({#self: &NestedInnerA#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[StaticVar]/CurrNominal:      aStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[StaticMethod]/CurrNominal:   aStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Struct]/CurrNominal:         NestedInnerAStruct[#NestedInnerA.NestedInnerAStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Class]/CurrNominal:          NestedInnerAClass[#NestedInnerA.NestedInnerAClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Enum]/CurrNominal:           NestedInnerAEnum[#NestedInnerA.NestedInnerAEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerATypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Struct]/Local:               NestedInnerA[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// FIXME: the following decls are wrong.
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[LocalVar]/Local:             self[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[InstanceMethod]/OutNominal:  testInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[InstanceVar]/OutNominal:     outerInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[InstanceMethod]/OutNominal:  outerInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2: End completions
      }

      typealias ATestTypealias = #^NESTED_NOMINAL_DECL_A_3^#
// NESTED_NOMINAL_DECL_A_3: Begin completions
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Struct]/CurrNominal:    NestedInnerAStruct[#NestedInnerA.NestedInnerAStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Class]/CurrNominal:     NestedInnerAClass[#NestedInnerA.NestedInnerAClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Enum]/CurrNominal:      NestedInnerAEnum[#NestedInnerA.NestedInnerAEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[TypeAlias]/CurrNominal: NestedInnerATypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Struct]/Local:          NestedInnerA[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[TypeAlias]/OutNominal:  OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Struct]/CurrModule:     NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3: End completions

      // Put these decls after code completion points to ensure that delayed
      // parsing works.
      var aInstanceVar: Int
      mutating
      func aInstanceFunc() {}
      static var aStaticVar: Int = 42
      static func aStaticFunc() {}
      subscript(i: Int) -> Double {
        get {
          return Double(i)
        }
        set(v) {
          instanceVar = i
        }
      }
      struct NestedInnerAStruct {}
      class NestedInnerAClass {}
      enum NestedInnerAEnum {}
      typealias NestedInnerATypealias = Int
    } // end NestedInnerA

    #^NESTED_NOMINAL_DECL_A_4^#
// NESTED_NOMINAL_DECL_A_4: Begin completions
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[Struct]/Local:               NestedInnerA[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[LocalVar]/Local:             self[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[InstanceMethod]/CurrNominal: testInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[InstanceVar]/CurrNominal:    outerInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[InstanceMethod]/CurrNominal: outerInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[TypeAlias]/CurrNominal:      OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4: End completions

    NestedInnerA()#^NESTED_NOMINAL_DECL_A_5^#
// NESTED_NOMINAL_DECL_A_5: Begin completions, 4 items
// NESTED_NOMINAL_DECL_A_5-NEXT: Decl[InstanceMethod]/CurrNominal: .aTestInstaceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_5-NEXT: Decl[InstanceVar]/CurrNominal:    .aInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_5-NEXT: Decl[InstanceMethod]/CurrNominal: .aInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_5-NEXT: Decl[Subscript]/CurrNominal:      [{#Int#}][#Double#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_5-NEXT: End completions
  }

  static func testStaticFunc() {
    struct NestedInnerB {
      mutating
      func bTestInstaceFunc() {
        #^NESTED_NOMINAL_DECL_B_1^#
// NESTED_NOMINAL_DECL_B_1: Begin completions
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[LocalVar]/Local:             self[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[InstanceMethod]/CurrNominal: bTestInstaceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[InstanceVar]/CurrNominal:    bInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[InstanceMethod]/CurrNominal: bInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Struct]/CurrNominal:         NestedInnerBStruct[#NestedInnerB.NestedInnerBStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Class]/CurrNominal:          NestedInnerBClass[#NestedInnerB.NestedInnerBClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Enum]/CurrNominal:           NestedInnerBEnum[#NestedInnerB.NestedInnerBEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerBTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Struct]/Local:               NestedInnerB[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// FIXME: the following decls are wrong.
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[LocalVar]/Local:             self[#NestedOuter1.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[InstanceMethod]/OutNominal:  testInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[StaticMethod]/OutNominal:    testStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[InstanceMethod]/OutNominal:  outerInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[StaticVar]/OutNominal:       outerStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[StaticMethod]/OutNominal:    outerStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[TypeAlias]/OutNominal:       OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1: End completions
      }
      static func bTestStaticFunc() {
        #^NESTED_NOMINAL_DECL_B_2^#
// NESTED_NOMINAL_DECL_B_2: Begin completions
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[LocalVar]/Local:             self[#NestedInnerB.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[InstanceMethod]/CurrNominal: bTestInstaceFunc({#self: &NestedInnerB#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticMethod]/CurrNominal:   bTestStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[InstanceMethod]/CurrNominal: bInstanceFunc({#self: &NestedInnerB#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticVar]/CurrNominal:      bStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticMethod]/CurrNominal:   bStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Struct]/CurrNominal:         NestedInnerBStruct[#NestedInnerB.NestedInnerBStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Class]/CurrNominal:          NestedInnerBClass[#NestedInnerB.NestedInnerBClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Enum]/CurrNominal:           NestedInnerBEnum[#NestedInnerB.NestedInnerBEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerBTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Struct]/Local:               NestedInnerB[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// FIXME: the following decls are wrong.
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[LocalVar]/Local:             self[#NestedOuter1.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[InstanceMethod]/OutNominal:  testInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticMethod]/OutNominal:    testStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[InstanceMethod]/OutNominal:  outerInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticVar]/OutNominal:       outerStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticMethod]/OutNominal:    outerStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[TypeAlias]/OutNominal:       OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2: End completions
      }

      typealias BTestTypealias = #^NESTED_NOMINAL_DECL_B_3^#
// NESTED_NOMINAL_DECL_B_3: Begin completions
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Struct]/CurrNominal:    NestedInnerBStruct[#NestedInnerB.NestedInnerBStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Class]/CurrNominal:     NestedInnerBClass[#NestedInnerB.NestedInnerBClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Enum]/CurrNominal:      NestedInnerBEnum[#NestedInnerB.NestedInnerBEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[TypeAlias]/CurrNominal: NestedInnerBTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Struct]/Local:          NestedInnerB[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[TypeAlias]/OutNominal:  OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Struct]/CurrModule:     NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3: End completions

      // Put these decls after code completion points to ensure that delayed
      // parsing works.
      var bInstanceVar: Int
      mutating
      func bInstanceFunc() {}
      static var bStaticVar: Int = 17
      static func bStaticFunc() {}
      subscript(i: Int) -> Double {
        get {
          return Double(i)
        }
        set(v) {
          instanceVar = i
        }
      }
      struct NestedInnerBStruct {}
      class NestedInnerBClass {}
      enum NestedInnerBEnum {}
      typealias NestedInnerBTypealias = Int
    } // end NestedInnerB

    #^NESTED_NOMINAL_DECL_B_4^#
// NESTED_NOMINAL_DECL_B_4: Begin completions
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[Struct]/Local:               NestedInnerB[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[LocalVar]/Local:             self[#NestedOuter1.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[InstanceMethod]/CurrNominal: testInstanceFunc({#self: &NestedOuter1#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[StaticMethod]/CurrNominal:   testStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[InstanceMethod]/CurrNominal: outerInstanceFunc({#self: &NestedOuter1#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[StaticVar]/CurrNominal:      outerStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[StaticMethod]/CurrNominal:   outerStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[TypeAlias]/CurrNominal:      OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4: End completions

    NestedInnerB()#^NESTED_NOMINAL_DECL_B_5^#
// NESTED_NOMINAL_DECL_B_5: Begin completions, 4 items
// NESTED_NOMINAL_DECL_B_5-DAG: Decl[InstanceMethod]/CurrNominal: .bTestInstaceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_5-DAG: Decl[InstanceVar]/CurrNominal:    .bInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_5-DAG: Decl[InstanceMethod]/CurrNominal: .bInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_5-DAG: Decl[Subscript]/CurrNominal:      [{#Int#}][#Double#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_5: End completions
  }

  var outerInstanceVar: Int
  mutating
  func outerInstanceFunc() {}
  static var outerStaticVar: Int = 1
  static func outerStaticFunc() {}
  typealias OuterTypealias = Int
}

func testOuterC() {
  struct NestedInnerC {
    mutating
    func cTestInstaceFunc() {
      #^NESTED_NOMINAL_DECL_C_1^#
// NESTED_NOMINAL_DECL_C_1: Begin completions
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[LocalVar]/Local:             self[#NestedInnerC#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[InstanceMethod]/CurrNominal: cTestInstaceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[InstanceVar]/CurrNominal:    cInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[InstanceMethod]/CurrNominal: cInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Struct]/CurrNominal:         NestedInnerCStruct[#NestedInnerC.NestedInnerCStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Class]/CurrNominal:          NestedInnerCClass[#NestedInnerC.NestedInnerCClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Enum]/CurrNominal:           NestedInnerCEnum[#NestedInnerC.NestedInnerCEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerCTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Struct]/Local:               NestedInnerC[#NestedInnerC#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1: End completions
    }
    static func cTestStaticFunc() {
      #^NESTED_NOMINAL_DECL_C_2^#
// NESTED_NOMINAL_DECL_C_2: Begin completions
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[LocalVar]/Local:             self[#NestedInnerC.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[InstanceMethod]/CurrNominal: cTestInstaceFunc({#self: &NestedInnerC#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[StaticMethod]/CurrNominal:   cTestStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[InstanceMethod]/CurrNominal: cInstanceFunc({#self: &NestedInnerC#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[StaticVar]/CurrNominal:      cStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[StaticMethod]/CurrNominal:   cStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Struct]/CurrNominal:         NestedInnerCStruct[#NestedInnerC.NestedInnerCStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Class]/CurrNominal:          NestedInnerCClass[#NestedInnerC.NestedInnerCClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Enum]/CurrNominal:           NestedInnerCEnum[#NestedInnerC.NestedInnerCEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerCTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Struct]/Local:               NestedInnerC[#NestedInnerC#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2: End completions
    }

    typealias CTestTypealias = #^NESTED_NOMINAL_DECL_C_3^#
// NESTED_NOMINAL_DECL_C_3: Begin completions
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[Struct]/CurrNominal:    NestedInnerCStruct[#NestedInnerC.NestedInnerCStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[Class]/CurrNominal:     NestedInnerCClass[#NestedInnerC.NestedInnerCClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[Enum]/CurrNominal:      NestedInnerCEnum[#NestedInnerC.NestedInnerCEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[TypeAlias]/CurrNominal: NestedInnerCTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[Struct]/Local:          NestedInnerC[#NestedInnerC#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_3: End completions

    // Put these decls after code completion points to ensure that delayed
    // parsing works.
    var cInstanceVar: Int
    mutating
    func cInstanceFunc() {}
    static var cStaticVar: Int = 1
    static func cStaticFunc() {}
    subscript(i: Int) -> Double {
      get {
        return Double(i)
      }
      set(v) {
        instanceVar = i
      }
    }
    struct NestedInnerCStruct {}
    class NestedInnerCClass {}
    enum NestedInnerCEnum {}
    typealias NestedInnerCTypealias = Int
  } // end NestedInnerC

  #^NESTED_NOMINAL_DECL_C_4^#
// NESTED_NOMINAL_DECL_C_4: Begin completions
// NESTED_NOMINAL_DECL_C_4-DAG: Decl[Struct]/Local: NestedInnerC[#NestedInnerC#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_4: End completions

  NestedInnerC()#^NESTED_NOMINAL_DECL_C_5^#
// NESTED_NOMINAL_DECL_C_5: Begin completions, 4 items
// NESTED_NOMINAL_DECL_C_5-NEXT: Decl[InstanceMethod]/CurrNominal: .cTestInstaceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_5-NEXT: Decl[InstanceVar]/CurrNominal:    .cInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_5-NEXT: Decl[InstanceMethod]/CurrNominal: .cInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_5-NEXT: Decl[Subscript]/CurrNominal:      [{#Int#}][#Double#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_5-NEXT: End completions
}

func testOuterD() {
  func dFunc1() {}
  func foo() {
    func dFunc2() {}
    struct Nested1 {
      struct Nested2 {
        func bar() {
          func dFunc4() {}
          #^NESTED_NOMINAL_DECL_D_1^#
        }
        func dFunc3() {}
      }
      func dFunc2() {}
    }
  }
}
// NESTED_NOMINAL_DECL_D_1: Begin completions
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[LocalVar]/Local:             self[#Nested1.Nested2#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[FreeFunction]/Local:         dFunc4()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[InstanceMethod]/CurrNominal: dFunc3()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[FreeFunction]/Local:         dFunc2()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[FreeFunction]/Local:         dFunc2()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[FreeFunction]/Local:         dFunc1()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1: End completions

func testOuterE() {
  var c1 = {
    func dFunc1() {}
    var c2 = {
      func dFunc2() {}
      struct Nested1 {
        struct Nested2 {
          func bar() {
            func dFunc4() {}
            var c3 = {
              func dFunc5() {}
              #^NESTED_NOMINAL_DECL_E_1^#
            }
          }
          func dFunc3() {}
        }
        func dFunc2() {}
      }
    } // end c2
  } // end c1
}
// NESTED_NOMINAL_DECL_E_1: Begin completions
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[LocalVar]/Local:            self[#Nested1.Nested2#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[FreeFunction]/Local:        dFunc5[#<<error type>>#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[FreeFunction]/Local:        dFunc4[#<<error type>>#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[InstanceMethod]/OutNominal: dFunc3[#<<error type>>#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[InstanceMethod]/OutNominal: dFunc2[#<<error type>>#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[FreeFunction]/Local:        dFunc2[#<<error type>>#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[FreeFunction]/Local:        dFunc1[#<<error type>>#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_E_1: End completions
