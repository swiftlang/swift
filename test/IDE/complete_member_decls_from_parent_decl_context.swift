// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_INSTANCE_METHOD_1 | FileCheck %s -check-prefix=IN_CLASS_INSTANCE_METHOD_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_CLASS_STATIC_METHOD_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_CLASS_STATIC_METHOD_1_NEGATIVE
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_CONSTRUCTOR_1 | FileCheck %s -check-prefix=IN_CLASS_CONSTRUCTOR_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLASS_DESTRUCTOR_1 | FileCheck %s -check-prefix=IN_CLASS_DESTRUCTOR_1

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_INSTANCE_METHOD_1 | FileCheck %s -check-prefix=IN_STRUCT_INSTANCE_METHOD_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_STRUCT_STATIC_METHOD_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_STATIC_METHOD_1 | FileCheck %s -check-prefix=IN_STRUCT_STATIC_METHOD_1_NEGATIVE
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_STRUCT_CONSTRUCTOR_1 | FileCheck %s -check-prefix=IN_STRUCT_CONSTRUCTOR_1


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
  get:
    return Double(i)
  set(val):
    instanceVar = i
  }

  struct NestedStruct {}
  class NestedClass {}
  enum NestedEnum {}
  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  // FIXME: uncomment when we have static vars.
  // static var staticVar: Int

  static func staticFunc0() {}
  static func staticFunc1(a: Int) {}

  /// @} Members.

  /// @{ Tests.

  func instanceTest1() {
    #^IN_CLASS_INSTANCE_METHOD_1^#
// IN_CLASS_INSTANCE_METHOD_1: Begin completions
// IN_CLASS_INSTANCE_METHOD_1-DAG: SwiftDecl: self[#CodeCompletionInClassMethods1#]{{$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceFunc1({#a: Int#})[#Void#]{{$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInClassMethods1.NestedStruct.metatype#]{{$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInClassMethods1.NestedClass.metatype#]{{$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedEnum[#CodeCompletionInClassMethods1.NestedEnum.metatype#]{{$}}
// IN_CLASS_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// IN_CLASS_INSTANCE_METHOD_1: End completions
  }

  static func staticTest1() {
    #^IN_CLASS_STATIC_METHOD_1^#
// Negative tests.
// IN_CLASS_STATIC_METHOD_1_NEGATIVE-NOT: SwiftDecl: instanceVar
//
// Positive tests.
// IN_CLASS_STATIC_METHOD_1: Begin completions
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: self[#CodeCompletionInClassMethods1.metatype#]{{$}}
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: instanceFunc0({#self: CodeCompletionInClassMethods1#})[#() -> Void#]{{$}}
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: instanceFunc1({#self: CodeCompletionInClassMethods1#})[#(a: Int) -> Void#]{{$}}
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInClassMethods1.NestedStruct.metatype#]{{$}}
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInClassMethods1.NestedClass.metatype#]{{$}}
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: NestedEnum[#CodeCompletionInClassMethods1.NestedEnum.metatype#]{{$}}
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: staticFunc0()[#Void#]{{$}}
// IN_CLASS_STATIC_METHOD_1-DAG: SwiftDecl: staticFunc1({#a: Int#})[#Void#]{{$}}
// IN_CLASS_STATIC_METHOD_1: End completions
  }

  init() {
    #^IN_CLASS_CONSTRUCTOR_1^#
// IN_CLASS_CONSTRUCTOR_1: Begin completions
// IN_CLASS_CONSTRUCTOR_1-DAG: SwiftDecl: self[#CodeCompletionInClassMethods1#]{{$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: SwiftDecl: instanceFunc1({#a: Int#})[#Void#]{{$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInClassMethods1.NestedStruct.metatype#]{{$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInClassMethods1.NestedClass.metatype#]{{$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: SwiftDecl: NestedEnum[#CodeCompletionInClassMethods1.NestedEnum.metatype#]{{$}}
// IN_CLASS_CONSTRUCTOR_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// IN_CLASS_CONSTRUCTOR_1: End completions
  }

  destructor() {
    #^IN_CLASS_DESTRUCTOR_1^#
// IN_CLASS_DESTRUCTOR_1: Begin completions
// IN_CLASS_DESTRUCTOR_1-DAG: SwiftDecl: self[#CodeCompletionInClassMethods1#]{{$}}
// IN_CLASS_DESTRUCTOR_1-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// IN_CLASS_DESTRUCTOR_1-DAG: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// IN_CLASS_DESTRUCTOR_1-DAG: SwiftDecl: instanceFunc1({#a: Int#})[#Void#]{{$}}
// IN_CLASS_DESTRUCTOR_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInClassMethods1.NestedStruct.metatype#]{{$}}
// IN_CLASS_DESTRUCTOR_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInClassMethods1.NestedClass.metatype#]{{$}}
// IN_CLASS_DESTRUCTOR_1-DAG: SwiftDecl: NestedEnum[#CodeCompletionInClassMethods1.NestedEnum.metatype#]{{$}}
// IN_CLASS_DESTRUCTOR_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
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

  func instanceFunc0() {}
  func instanceFunc1(a: Int) {}

  subscript(i: Int) -> Double {
  get:
    return Double(i)
  set(val):
    instanceVar = i
  }

  struct NestedStruct {}
  class NestedClass {}
  enum NestedEnum {}
  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int

  // FIXME: uncomment when we have static vars.
  // static var staticVar: Int

  static func staticFunc0() {}
  static func staticFunc1(a: Int) {}

  /// @} Members.

  func instanceTest1() {
    #^IN_STRUCT_INSTANCE_METHOD_1^#
// IN_STRUCT_INSTANCE_METHOD_1: Begin completions
// IN_STRUCT_INSTANCE_METHOD_1-DAG: SwiftDecl: self[#CodeCompletionInStructMethods1#]{{$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: SwiftDecl: instanceFunc1({#a: Int#})[#Void#]{{$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInStructMethods1.NestedStruct.metatype#]{{$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInStructMethods1.NestedClass.metatype#]{{$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedEnum[#CodeCompletionInStructMethods1.NestedEnum.metatype#]{{$}}
// IN_STRUCT_INSTANCE_METHOD_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// IN_STRUCT_INSTANCE_METHOD_1: End completions
  }

  static func staticTest1() {
    #^IN_STRUCT_STATIC_METHOD_1^#
// Negative tests.
// IN_STRUCT_STATIC_METHOD_1_NEGATIVE-NOT: SwiftDecl: instanceVar
//
// Positive tests.
// IN_STRUCT_STATIC_METHOD_1: Begin completions
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: self[#CodeCompletionInStructMethods1.metatype#]{{$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: instanceFunc0({#self: [inout] CodeCompletionInStructMethods1#})[#() -> Void#]{{$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: instanceFunc1({#self: [inout] CodeCompletionInStructMethods1#})[#(a: Int) -> Void#]{{$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInStructMethods1.NestedStruct.metatype#]{{$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInStructMethods1.NestedClass.metatype#]{{$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: NestedEnum[#CodeCompletionInStructMethods1.NestedEnum.metatype#]{{$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: staticFunc0()[#Void#]{{$}}
// IN_STRUCT_STATIC_METHOD_1-DAG: SwiftDecl: staticFunc1({#a: Int#})[#Void#]{{$}}
// IN_STRUCT_STATIC_METHOD_1: End completions
  }

  init() {
    #^IN_STRUCT_CONSTRUCTOR_1^#
// IN_STRUCT_CONSTRUCTOR_1: Begin completions
// IN_STRUCT_CONSTRUCTOR_1-DAG: SwiftDecl: self[#CodeCompletionInStructMethods1#]{{$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: SwiftDecl: instanceVar[#Int#]{{$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: SwiftDecl: instanceFunc1({#a: Int#})[#Void#]{{$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: SwiftDecl: NestedStruct[#CodeCompletionInStructMethods1.NestedStruct.metatype#]{{$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: SwiftDecl: NestedClass[#CodeCompletionInStructMethods1.NestedClass.metatype#]{{$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: SwiftDecl: NestedEnum[#CodeCompletionInStructMethods1.NestedEnum.metatype#]{{$}}
// IN_STRUCT_CONSTRUCTOR_1-DAG: SwiftDecl: NestedTypealias[#Int.metatype#]{{$}}
// IN_STRUCT_CONSTRUCTOR_1: End completions
  }
}


