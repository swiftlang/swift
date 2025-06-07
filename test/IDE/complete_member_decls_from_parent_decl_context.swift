// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_INSTANCE_METHOD_1 | %FileCheck %s -check-prefix=IN_CLASS_INSTANCE_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_STATIC_METHOD_1 | %FileCheck %s -check-prefix=IN_CLASS_STATIC_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_STATIC_METHOD_1 | %FileCheck %s -check-prefix=IN_CLASS_STATIC_METHOD_1_NEGATIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_CONSTRUCTOR_1 | %FileCheck %s -check-prefix=IN_CLASS_CONSTRUCTOR_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_DESTRUCTOR_1 | %FileCheck %s -check-prefix=IN_CLASS_DESTRUCTOR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_INSTANCE_METHOD_1 | %FileCheck %s -check-prefix=IN_STRUCT_INSTANCE_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_STATIC_METHOD_1 | %FileCheck %s -check-prefix=IN_STRUCT_STATIC_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_STATIC_METHOD_1 | %FileCheck %s -check-prefix=IN_STRUCT_STATIC_METHOD_1_NEGATIVE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_CONSTRUCTOR_1 | %FileCheck %s -check-prefix=IN_STRUCT_CONSTRUCTOR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_1 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_2 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_3 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_4 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_A_5 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_A_5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_1 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_2 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_3 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_4 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_B_5 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_B_5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_1 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_2 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_3 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_4 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_C_5 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_C_5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_D_1 | %FileCheck %s -allow-deprecated-dag-overlap -check-prefix=NESTED_NOMINAL_DECL_D_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL_DECL_E_1 | %FileCheck %s -check-prefix=NESTED_NOMINAL_DECL_E_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ISSUE_43244_SUBCLASS | %FileCheck %s -check-prefix=ISSUE_43244_SUBCLASS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ISSUE_43244_SUB_SUBCLASS | %FileCheck %s -check-prefix=ISSUE_43244_SUB_SUBCLASS

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
  func instanceFunc1(_ a: Int) {}

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
  // Cannot declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  class var staticVar: Int

  class func staticFunc0() {}
  class func staticFunc1(_ a: Int) {}

  /// @} Members.

  /// @{ Tests.

  func instanceTest1() {
    #^IN_CLASS_INSTANCE_METHOD_1^#
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInClassMethods1#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#NestedStruct#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#NestedClass#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#NestedEnum#]{{; name=.+$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
  }

  class func staticTest1() {
    #^IN_CLASS_STATIC_METHOD_1^#
// Negative tests.
// IN_CLASS_STATIC_METHOD_1_NEGATIVE-NOT: instanceVar
//
// Positive tests.
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInClassMethods1.Type#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0({#(self): CodeCompletionInClassMethods1#})[#() -> Void#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(self): CodeCompletionInClassMethods1#})[#(Int) -> Void#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#NestedStruct#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#NestedClass#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#NestedEnum#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[StaticMethod]/CurrNominal:   staticFunc0()[#Void#]{{; name=.+$}}
// IN_CLASS_STATIC_METHOD_1-DAG: Decl[StaticMethod]/CurrNominal:   staticFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
  }

  init() {
    #^IN_CLASS_CONSTRUCTOR_1^#
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInClassMethods1#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#NestedStruct#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#NestedClass#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#NestedEnum#]{{; name=.+$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
  }

  deinit {
    #^IN_CLASS_DESTRUCTOR_1^#
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInClassMethods1#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#NestedStruct#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#NestedClass#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#NestedEnum#]{{; name=.+$}}
// IN_CLASS_DESTRUCTOR_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
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
  func instanceFunc1(_ a: Int) {}

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
  // Cannot declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  static var staticVar: Int

  static func staticFunc0() {}
  static func staticFunc1(_ a: Int) {}

  /// @} Members.

  func instanceTest1() {
    #^IN_STRUCT_INSTANCE_METHOD_1^#
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInStructMethods1#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#NestedStruct#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#NestedClass#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#NestedEnum#]{{; name=.+$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
  }

  static func staticTest1() {
    #^IN_STRUCT_STATIC_METHOD_1^#
// Negative tests.
// IN_STRUCT_STATIC_METHOD_1_NEGATIVE-NOT: instanceVar
//
// Positive tests.
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInStructMethods1.Type#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0({#(self): &CodeCompletionInStructMethods1#})[#() -> Void#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(self): &CodeCompletionInStructMethods1#})[#(Int) -> Void#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#NestedStruct#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#NestedClass#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#NestedEnum#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[StaticMethod]/CurrNominal:   staticFunc0()[#Void#]{{; name=.+$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: Decl[StaticMethod]/CurrNominal:   staticFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
  }

  init() {
    #^IN_STRUCT_CONSTRUCTOR_1^#
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[LocalVar]/Local:             self[#CodeCompletionInStructMethods1#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc1({#(a): Int#})[#Void#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[Struct]/CurrNominal:         NestedStruct[#NestedStruct#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[Class]/CurrNominal:          NestedClass[#NestedClass#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[Enum]/CurrNominal:           NestedEnum[#NestedEnum#]{{; name=.+$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedTypealias[#Int#]{{; name=.+$}}
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
      func aTestInstanceFunc() {
        #^NESTED_NOMINAL_DECL_A_1^#
// NESTED_NOMINAL_DECL_A_1-NOT: self[#NestedOuter1#]
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[LocalVar]/Local:             self[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceMethod]/CurrNominal: aTestInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceVar]/CurrNominal:    aInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[InstanceMethod]/CurrNominal: aInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Struct]/CurrNominal:         NestedInnerAStruct[#NestedInnerAStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Class]/CurrNominal:          NestedInnerAClass[#NestedInnerAClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Enum]/CurrNominal:           NestedInnerAEnum[#NestedInnerAEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerATypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Struct]/Local:               NestedInnerA[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_1-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}

// NESTED_NOMINAL_DECL_A_1-NOT: self[#NestedOuter1#]
// NESTED_NOMINAL_DECL_A_1-NOT: testInstanceFunc()
// NESTED_NOMINAL_DECL_A_1-NOT: outerInstanceVar
// NESTED_NOMINAL_DECL_A_1-NOT: outerInstanceFunc()
      }
      static func aTestStaticFunc() {
        #^NESTED_NOMINAL_DECL_A_2^#
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[LocalVar]/Local:             self[#NestedInnerA.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[InstanceMethod]/CurrNominal: aTestInstanceFunc({#(self): &NestedInnerA#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[StaticMethod]/CurrNominal:   aTestStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[InstanceMethod]/CurrNominal: aInstanceFunc({#(self): &NestedInnerA#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[StaticVar]/CurrNominal:      aStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[StaticMethod]/CurrNominal:   aStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Struct]/CurrNominal:         NestedInnerAStruct[#NestedInnerAStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Class]/CurrNominal:          NestedInnerAClass[#NestedInnerAClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Enum]/CurrNominal:           NestedInnerAEnum[#NestedInnerAEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerATypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Struct]/Local:               NestedInnerA[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_2-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}

// NESTED_NOMINAL_DECL_A_2-NOT: self[#NestedOuter1#]
// NESTED_NOMINAL_DECL_A_2-NOT: testInstanceFunc()
// NESTED_NOMINAL_DECL_A_2-NOT: outerInstanceVar[#Int#]
// NESTED_NOMINAL_DECL_A_2-NOT: outerInstanceFunc()
      }

      typealias ATestTypealias = #^NESTED_NOMINAL_DECL_A_3^#
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Struct]/CurrNominal:    NestedInnerAStruct[#NestedInnerAStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Class]/CurrNominal:     NestedInnerAClass[#NestedInnerAClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Enum]/CurrNominal:      NestedInnerAEnum[#NestedInnerAEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[TypeAlias]/CurrNominal: NestedInnerATypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Struct]/Local:          NestedInnerA[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[TypeAlias]/OutNominal:  OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_3-DAG: Decl[Struct]/CurrModule:     NestedOuter1[#NestedOuter1#]{{; name=.+$}}

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
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[Struct]/Local:               NestedInnerA[#NestedInnerA#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[LocalVar]/Local:             self[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[InstanceMethod]/CurrNominal: testInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[InstanceVar]/CurrNominal:    outerInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[InstanceMethod]/CurrNominal: outerInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[TypeAlias]/CurrNominal:      OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_4-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}

    NestedInnerA(aInstanceVar: 42)#^NESTED_NOMINAL_DECL_A_5^#
// NESTED_NOMINAL_DECL_A_5: Begin completions, 5 items
// NESTED_NOMINAL_DECL_A_5-DAG: Decl[InstanceMethod]/CurrNominal: .aTestInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_5-DAG: Decl[InstanceVar]/CurrNominal:    .aInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_5-DAG: Decl[InstanceMethod]/CurrNominal: .aInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_5-DAG: Decl[Subscript]/CurrNominal:      [{#(i): Int#}][#Double#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_A_5-DAG: Keyword[self]/CurrNominal:        .self[#NestedInnerA#]; name=self
  }

  static func testStaticFunc() {
    struct NestedInnerB {
      mutating
      func bTestInstanceFunc() {
        #^NESTED_NOMINAL_DECL_B_1^#
// NESTED_NOMINAL_DECL_B_1-NOT: self[#NestedOuter1.Type#]
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[LocalVar]/Local:             self[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[InstanceMethod]/CurrNominal: bTestInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[InstanceVar]/CurrNominal:    bInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[InstanceMethod]/CurrNominal: bInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Struct]/CurrNominal:         NestedInnerBStruct[#NestedInnerBStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Class]/CurrNominal:          NestedInnerBClass[#NestedInnerBClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Enum]/CurrNominal:           NestedInnerBEnum[#NestedInnerBEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerBTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Struct]/Local:               NestedInnerB[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_1-DAG: Decl[TypeAlias]/OutNominal:       OuterTypealias[#Int#]{{; name=.+$}}

// NESTED_NOMINAL_DECL_B_1-NOT: self[#NestedOuter1.Type#]
// NESTED_NOMINAL_DECL_B_1-NOT: testInstanceFunc()
// NESTED_NOMINAL_DECL_B_1-NOT: testStaticFunc()
// NESTED_NOMINAL_DECL_B_1-NOT: outerInstanceFunc()
// NESTED_NOMINAL_DECL_B_1-NOT: outerStaticVar
// NESTED_NOMINAL_DECL_B_1-NOT: outerStaticFunc()

      }
      static func bTestStaticFunc() {
        #^NESTED_NOMINAL_DECL_B_2^#
// NESTED_NOMINAL_DECL_B_2-NOT: self[#NestedOuter1.Type#]
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[LocalVar]/Local:             self[#NestedInnerB.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[InstanceMethod]/CurrNominal: bTestInstanceFunc({#(self): &NestedInnerB#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticMethod]/CurrNominal:   bTestStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[InstanceMethod]/CurrNominal: bInstanceFunc({#(self): &NestedInnerB#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticVar]/CurrNominal:      bStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticMethod]/CurrNominal:   bStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Struct]/CurrNominal:         NestedInnerBStruct[#NestedInnerBStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Class]/CurrNominal:          NestedInnerBClass[#NestedInnerBClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Enum]/CurrNominal:           NestedInnerBEnum[#NestedInnerBEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerBTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Struct]/Local:               NestedInnerB[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[TypeAlias]/OutNominal:       OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticMethod]/OutNominal:    testStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticVar]/OutNominal:       outerStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_2-DAG: Decl[StaticMethod]/OutNominal:    outerStaticFunc()[#Void#]{{; name=.+$}}

// NESTED_NOMINAL_DECL_B_2-NOT: self[#NestedOuter1.Type#]
// NESTED_NOMINAL_DECL_B_2-NOT: testInstanceFunc()
// NESTED_NOMINAL_DECL_B_2-NOT: outerInstanceFunc()

      }

      typealias BTestTypealias = #^NESTED_NOMINAL_DECL_B_3^#
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Struct]/CurrNominal:    NestedInnerBStruct[#NestedInnerBStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Class]/CurrNominal:     NestedInnerBClass[#NestedInnerBClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Enum]/CurrNominal:      NestedInnerBEnum[#NestedInnerBEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[TypeAlias]/CurrNominal: NestedInnerBTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Struct]/Local:          NestedInnerB[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[TypeAlias]/OutNominal:  OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_3-DAG: Decl[Struct]/CurrModule:     NestedOuter1[#NestedOuter1#]{{; name=.+$}}

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
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[Struct]/Local:               NestedInnerB[#NestedInnerB#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[LocalVar]/Local:             self[#NestedOuter1.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[InstanceMethod]/CurrNominal: testInstanceFunc({#(self): &NestedOuter1#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[StaticMethod]/CurrNominal:   testStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[InstanceMethod]/CurrNominal: outerInstanceFunc({#(self): &NestedOuter1#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[StaticVar]/CurrNominal:      outerStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[StaticMethod]/CurrNominal:   outerStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[TypeAlias]/CurrNominal:      OuterTypealias[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_4-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}

    NestedInnerB(bInstanceVar: 42)#^NESTED_NOMINAL_DECL_B_5^#
// NESTED_NOMINAL_DECL_B_5: Begin completions, 5 items
// NESTED_NOMINAL_DECL_B_5-DAG: Decl[InstanceMethod]/CurrNominal: .bTestInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_5-DAG: Decl[InstanceVar]/CurrNominal:    .bInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_5-DAG: Decl[InstanceMethod]/CurrNominal: .bInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_5-DAG: Decl[Subscript]/CurrNominal:      [{#(i): Int#}][#Double#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_B_5-DAG: Keyword[self]/CurrNominal:        .self[#NestedInnerB#]; name=self
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
    func cTestInstanceFunc() {
      #^NESTED_NOMINAL_DECL_C_1^#
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[LocalVar]/Local:             self[#NestedInnerC#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[InstanceMethod]/CurrNominal: cTestInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[InstanceVar]/CurrNominal:    cInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[InstanceMethod]/CurrNominal: cInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Struct]/CurrNominal:         NestedInnerCStruct[#NestedInnerCStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Class]/CurrNominal:          NestedInnerCClass[#NestedInnerCClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Enum]/CurrNominal:           NestedInnerCEnum[#NestedInnerCEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerCTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Struct]/Local:               NestedInnerC[#NestedInnerC#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_1-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
    }
    static func cTestStaticFunc() {
      #^NESTED_NOMINAL_DECL_C_2^#
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[LocalVar]/Local:             self[#NestedInnerC.Type#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[InstanceMethod]/CurrNominal: cTestInstanceFunc({#(self): &NestedInnerC#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[StaticMethod]/CurrNominal:   cTestStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[InstanceMethod]/CurrNominal: cInstanceFunc({#(self): &NestedInnerC#})[#() -> Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[StaticVar]/CurrNominal:      cStaticVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[StaticMethod]/CurrNominal:   cStaticFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Struct]/CurrNominal:         NestedInnerCStruct[#NestedInnerCStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Class]/CurrNominal:          NestedInnerCClass[#NestedInnerCClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Enum]/CurrNominal:           NestedInnerCEnum[#NestedInnerCEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[TypeAlias]/CurrNominal:      NestedInnerCTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Struct]/Local:               NestedInnerC[#NestedInnerC#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_2-DAG: Decl[Struct]/CurrModule:          NestedOuter1[#NestedOuter1#]{{; name=.+$}}
    }

    typealias CTestTypealias = #^NESTED_NOMINAL_DECL_C_3^#
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[Struct]/CurrNominal:   NestedInnerCStruct[#NestedInnerCStruct#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[Class]/CurrNominal:    NestedInnerCClass[#NestedInnerCClass#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[Enum]/CurrNominal:     NestedInnerCEnum[#NestedInnerCEnum#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[TypeAlias]/CurrNominal: NestedInnerCTypealias[#Int#]{{; name=.+$}}
// FIXME: should this really come as Local?
// NESTED_NOMINAL_DECL_C_3-DAG: Decl[Struct]/Local:          NestedInnerC[#NestedInnerC#]{{; name=.+$}}

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
// NESTED_NOMINAL_DECL_C_4-DAG: Decl[Struct]/Local: NestedInnerC[#NestedInnerC#]{{; name=.+$}}

  NestedInnerC(cInstanceVar: 42)#^NESTED_NOMINAL_DECL_C_5^#
// NESTED_NOMINAL_DECL_C_5: Begin completions, 5 items
// NESTED_NOMINAL_DECL_C_5-DAG: Decl[InstanceMethod]/CurrNominal: .cTestInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_5-DAG: Decl[InstanceVar]/CurrNominal:    .cInstanceVar[#Int#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_5-DAG: Decl[InstanceMethod]/CurrNominal: .cInstanceFunc()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_5-DAG: Decl[Subscript]/CurrNominal:      [{#(i): Int#}][#Double#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_C_5-DAG: Keyword[self]/CurrNominal:        .self[#NestedInnerC#]; name=self
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
      func dFunc5() {}
    }
  }
}
// NESTED_NOMINAL_DECL_D_1-NOT: dFunc5()
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[LocalVar]/Local:             self[#Nested1.Nested2#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[FreeFunction]/Local:         dFunc4()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[InstanceMethod]/CurrNominal: dFunc3()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[FreeFunction]/Local:         dFunc2()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-DAG: Decl[FreeFunction]/Local:         dFunc1()[#Void#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_D_1-NOT: dFunc5()

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
        func dFunc6() {}
      }
    } // end c2
  } // end c1
}
// NESTED_NOMINAL_DECL_E_1-NOT: dFunc6()
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[LocalVar]/Local:            self[#Nested1.Nested2#]{{; name=.+$}}
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[FreeFunction]/Local:        dFunc5()[#Void#]; name=dFunc5()
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[FreeFunction]/Local:        dFunc4()[#Void#]; name=dFunc4()
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[InstanceMethod]/CurrNominal: dFunc3()[#Void#]; name=dFunc3()
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[FreeFunction]/Local:        dFunc2()[#Void#]; name=dFunc2()
// NESTED_NOMINAL_DECL_E_1-DAG: Decl[FreeFunction]/Local:        dFunc1()[#Void#]; name=dFunc1()
// NESTED_NOMINAL_DECL_E_1-NOT: dFunc6()

// https://github.com/apple/swift/issues/43244
do {
  class BaseClass<T> {
    func myFunction(_ x: T) -> T? {}
  }

  class Subclass: BaseClass<String> {
    #^ISSUE_43244_SUBCLASS^#
  // ISSUE_43244_SUBCLASS-DAG: Decl[InstanceMethod]/Super:         override func myFunction(_ x: String) -> String? {|}; name=myFunction(:)
  }

  class SubSubclass: Subclass {
    #^ISSUE_43244_SUB_SUBCLASS^#
    // ISSUE_43244_SUB_SUBCLASS-DAG: Decl[InstanceMethod]/Super:         override func myFunction(_ x: String) -> String? {|}; name=myFunction(:)
  }
}
