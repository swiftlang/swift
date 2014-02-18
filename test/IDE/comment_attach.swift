// NOTE: This file is sensitive to line numbers.  Thus RUN and CHECK lines come
// below the code.
//
// NOTE: Please don't change this file to use FileCheck's feature to match
// relative line numbers: those lines are comments and we don't want to see
// anything extra in a test for documentation comments.

//===--- Check that we attach comments to all kinds of declarations.

// Ignore comments above.
func eatComments1() {}

// decl_func_1 Aaa.
func decl_func_1() {}

// decl_func_2 Aaa.
func decl_func_2<T>(t: T) {}

// decl_func_3 Aaa.
@unknown_attribute
func decl_func_3() {}


// decl_var_1 Aaa.
var decl_var_1: Int

// decl_var_2 decl_var_3 Aaa.
var (decl_var_2, decl_var_3): (Int, Float)

// decl_var_4 Aaa.
@unknown_attribute
var decl_var_4: Int

// decl_var_5 Aaa.
var decl_var_5: Int {
get: return 0
set:
}

var decl_var_6: Int {
// decl_var_6_get Aaa.
get: return 0
// decl_var_6_set Aaa.
set:
}

// decl_var_7 Aaa.
var decl_var_7: Int = 0 {
willSet:
didSet:
}

var decl_var_8: Int = 0 {
// decl_var_8_get Aaa.
willSet:
// decl_var_8_set Aaa.
didSet:
}


// decl_val_1 Aaa.
val decl_val_1: Int = 0

// decl_val_2 decl_val_3 Aaa.
val (decl_val_2, decl_val_3): (Int, Float) = (0, 0.0)

// decl_val_4 Aaa.
@unknown_attribute
val decl_val_4: Int = 0


// decl_typealias_1 Aaa.
typealias decl_typealias_1 = Int

// decl_struct_1 Aaa.
struct decl_struct_1 {
  // instanceVar1 Aaa.
  var instanceVar1: Int

  // instanceVar2 instanceVar3 Aaa.
  var (instanceVar2, instanceVar3): (Int, Float)

  // instanceVar4 Aaa.
  @unknown_attribute
  var instanceVar4: Int

  // instanceVar5 Aaa.
  var instanceVar5: Int {
  get: return 0
  set:
  }

  var instanceVar6: Int {
  // instanceVar6_get Aaa.
  get: return 0
  // instanceVar6_set Aaa.
  set:
  }

  // instanceVar7 Aaa.
  var instanceVar7: Int = 0 {
  willSet:
  didSet:
  }

  var instanceVar8: Int = 0 {
  // instanceVar8_get Aaa.
  willSet:
  // instanceVar8_set Aaa.
  didSet:
  }


  // instanceFunc1 Aaa.
  func instanceFunc1() {}

  // instanceFunc2 Aaa.
  @mutable
  func instanceFunc2() {}

  // instanceFunc3 Aaa.
  func instanceFunc3<T>(t: T) {}

  // instanceFunc4 Aaa.
  @unknown_attribute
  func instanceFunc4() {}


  // subscript Aaa.
  subscript(i: Int) -> Double { return 0.0 }

  // NestedStruct Aaa.
  struct NestedStruct {}

  // NestedClass Aaa.
  class NestedClass {}

  // NestedEnum Aaa.
  enum NestedEnum {}

  // Can not declare a nested protocol.
  // protocol NestedProtocol {}
  func eatComments2() {}

  // NestedTypealias Aaa.
  typealias NestedTypealias = Int

  // staticVar Aaa.
  static var staticVar: Int = 4

  // staticFunc1 Aaa.
  static func staticFunc1() {}
}

// decl_enum_1 Aaa.
enum decl_enum_1 {
  // Case1 Aaa.
  case Case1

  // Case2 Aaa.
  case Case2(Int)

  // Case3 Aaa.
  case Case3(Int, Float)

  // Case4 Case5 Aaa.
  case Case4, Case5
}

// decl_class_1 Aaa.
class decl_class_1 {
}

// decl_protocol_1 Aaa.
protocol decl_protocol_1 {
  // NestedTypealias Aaa.
  typealias NestedTypealias
}

// FIXME: While there is nothing stopping us from attaching comments to
// extensions, how would we use those comments?

// decl_extension_1 Aaa.
extension decl_extension_1 {
}

//===--- Check how we merge consecutive comments.

// RUN: %swift-ide-test -print-comments -source-filename %s | FileCheck %s

// CHECK: comment_attach.swift:11:6: Func/eatComments1 RawComment=[{{.*}}]
// CHECK-NEXT: comment_attach.swift:14:6: Func/decl_func_1 RawComment=[// decl_func_1 Aaa.]
// CHECK-NEXT: comment_attach.swift:17:6: Func/decl_func_2 RawComment=[// decl_func_2 Aaa.]
// CHECK-NEXT: comment_attach.swift:17:21: Var/t RawComment=none
// CHECK-NEXT: comment_attach.swift:21:6: Func/decl_func_3 RawComment=[// decl_func_3 Aaa.]
// CHECK-NEXT: comment_attach.swift:25:5: Var/decl_var_1 RawComment=[// decl_var_1 Aaa.]
// CHECK-NEXT: comment_attach.swift:28:6: Var/decl_var_2 RawComment=[// decl_var_2 decl_var_3 Aaa.]
// CHECK-NEXT: comment_attach.swift:28:18: Var/decl_var_3 RawComment=[// decl_var_2 decl_var_3 Aaa.]
// CHECK-NEXT: comment_attach.swift:32:5: Var/decl_var_4 RawComment=[// decl_var_4 Aaa.]
// CHECK-NEXT: comment_attach.swift:35:5: Var/decl_var_5 RawComment=[// decl_var_5 Aaa.]
// CHECK-NEXT: comment_attach.swift:36:1: Func/<getter for decl_var_5> RawComment=none
// CHECK-NEXT: comment_attach.swift:37:1: Func/<setter for decl_var_5> RawComment=none
// CHECK-NEXT: comment_attach.swift:40:5: Var/decl_var_6 RawComment=none
// CHECK-NEXT: comment_attach.swift:42:1: Func/<getter for decl_var_6> RawComment=none
// CHECK-NEXT: comment_attach.swift:44:1: Func/<setter for decl_var_6> RawComment=none
// CHECK-NEXT: comment_attach.swift:48:5: Var/decl_var_7 RawComment=[// decl_var_7 Aaa.]
// CHECK-NEXT: comment_attach.swift:49:1: Func/<willSet for decl_var_7> RawComment=none
// CHECK-NEXT: comment_attach.swift:50:1: Func/<didSet for decl_var_7> RawComment=none
// CHECK-NEXT: comment_attach.swift:53:5: Var/decl_var_8 RawComment=none
// CHECK-NEXT: comment_attach.swift:55:1: Func/<willSet for decl_var_8> RawComment=none
// CHECK-NEXT: comment_attach.swift:57:1: Func/<didSet for decl_var_8> RawComment=none
// CHECK-NEXT: comment_attach.swift:62:5: Var/decl_val_1 RawComment=[// decl_val_1 Aaa.]
// CHECK-NEXT: comment_attach.swift:65:6: Var/decl_val_2 RawComment=[// decl_val_2 decl_val_3 Aaa.]
// CHECK-NEXT: comment_attach.swift:65:18: Var/decl_val_3 RawComment=[// decl_val_2 decl_val_3 Aaa.]
// CHECK-NEXT: comment_attach.swift:69:5: Var/decl_val_4 RawComment=[// decl_val_4 Aaa.]
// CHECK-NEXT: comment_attach.swift:73:11: TypeAlias/decl_typealias_1 RawComment=[// decl_typealias_1 Aaa.]
// CHECK-NEXT: comment_attach.swift:76:8: Struct/decl_struct_1 RawComment=[// decl_struct_1 Aaa.]
// CHECK-NEXT: comment_attach.swift:78:7: Var/instanceVar1 RawComment=[// instanceVar1 Aaa.]
// CHECK-NEXT: comment_attach.swift:81:8: Var/instanceVar2 RawComment=[// instanceVar2 instanceVar3 Aaa.]
// CHECK-NEXT: comment_attach.swift:81:22: Var/instanceVar3 RawComment=[// instanceVar2 instanceVar3 Aaa.]
// CHECK-NEXT: comment_attach.swift:85:7: Var/instanceVar4 RawComment=[// instanceVar4 Aaa.]
// CHECK-NEXT: comment_attach.swift:88:7: Var/instanceVar5 RawComment=[// instanceVar5 Aaa.]
// CHECK-NEXT: comment_attach.swift:89:3: Func/<getter for instanceVar5> RawComment=none
// CHECK-NEXT: comment_attach.swift:90:3: Func/<setter for instanceVar5> RawComment=none
// CHECK-NEXT: comment_attach.swift:93:7: Var/instanceVar6 RawComment=none
// CHECK-NEXT: comment_attach.swift:95:3: Func/<getter for instanceVar6> RawComment=none
// CHECK-NEXT: comment_attach.swift:97:3: Func/<setter for instanceVar6> RawComment=none
// CHECK-NEXT: comment_attach.swift:101:7: Var/instanceVar7 RawComment=[// instanceVar7 Aaa.]
// CHECK-NEXT: comment_attach.swift:102:3: Func/<willSet for instanceVar7> RawComment=none
// CHECK-NEXT: comment_attach.swift:103:3: Func/<didSet for instanceVar7> RawComment=none
// CHECK-NEXT: comment_attach.swift:106:7: Var/instanceVar8 RawComment=none
// CHECK-NEXT: comment_attach.swift:108:3: Func/<willSet for instanceVar8> RawComment=none
// CHECK-NEXT: comment_attach.swift:110:3: Func/<didSet for instanceVar8> RawComment=none
// CHECK-NEXT: comment_attach.swift:115:8: Func/instanceFunc1 RawComment=[// instanceFunc1 Aaa.]
// CHECK-NEXT: comment_attach.swift:119:8: Func/instanceFunc2 RawComment=[// instanceFunc2 Aaa.]
// CHECK-NEXT: comment_attach.swift:122:8: Func/instanceFunc3 RawComment=[// instanceFunc3 Aaa.]
// CHECK-NEXT: comment_attach.swift:122:25: Var/t RawComment=none
// CHECK-NEXT: comment_attach.swift:126:8: Func/instanceFunc4 RawComment=[// instanceFunc4 Aaa.]
// CHECK-NEXT: comment_attach.swift:130:3: Subscript/subscript RawComment=[// subscript Aaa.]
// CHECK-NEXT: comment_attach.swift:130:13: Var/i RawComment=none
// CHECK-NEXT: comment_attach.swift:130:33: Func/<getter for subscript> RawComment=none
// CHECK-NEXT: comment_attach.swift:133:10: Struct/NestedStruct RawComment=[// NestedStruct Aaa.]
// CHECK-NEXT: comment_attach.swift:136:9: Class/NestedClass RawComment=[// NestedClass Aaa.]
// CHECK-NEXT: comment_attach.swift:139:8: Enum/NestedEnum RawComment=[// NestedEnum Aaa.]
// CHECK-NEXT: comment_attach.swift:143:8: Func/eatComments2 RawComment=[{{.*}}]
// CHECK-NEXT: comment_attach.swift:146:13: TypeAlias/NestedTypealias RawComment=[// NestedTypealias Aaa.]
// CHECK-NEXT: comment_attach.swift:149:14: Var/staticVar RawComment=[// staticVar Aaa.]
// CHECK-NEXT: comment_attach.swift:152:15: Func/staticFunc1 RawComment=[// staticFunc1 Aaa.]
// CHECK-NEXT: comment_attach.swift:76:8: Var/instanceVar1 RawComment=none
// CHECK-NEXT: comment_attach.swift:76:8: Var/instanceVar2 RawComment=none
// CHECK-NEXT: comment_attach.swift:76:8: Var/instanceVar3 RawComment=none
// CHECK-NEXT: comment_attach.swift:76:8: Var/instanceVar4 RawComment=none
// CHECK-NEXT: comment_attach.swift:76:8: Var/instanceVar7 RawComment=none
// CHECK-NEXT: comment_attach.swift:76:8: Var/instanceVar8 RawComment=none
// CHECK-NEXT: comment_attach.swift:156:6: Enum/decl_enum_1 RawComment=[// decl_enum_1 Aaa.]
// CHECK-NEXT: comment_attach.swift:158:8: EnumElement/Case1 RawComment=[// Case1 Aaa.]
// CHECK-NEXT: comment_attach.swift:161:8: EnumElement/Case2 RawComment=[// Case2 Aaa.]
// CHECK-NEXT: comment_attach.swift:164:8: EnumElement/Case3 RawComment=[// Case3 Aaa.]
// CHECK-NEXT: comment_attach.swift:167:8: EnumElement/Case4 RawComment=[// Case4 Case5 Aaa.]
// CHECK-NEXT: comment_attach.swift:167:15: EnumElement/Case5 RawComment=[// Case4 Case5 Aaa.]
// CHECK-NEXT: comment_attach.swift:171:7: Class/decl_class_1 RawComment=[// decl_class_1 Aaa.]
// CHECK-NEXT: comment_attach.swift:175:10: Protocol/decl_protocol_1 RawComment=[// decl_protocol_1 Aaa.]
// CHECK-NEXT: comment_attach.swift:177:13: AssociatedType/NestedTypealias RawComment=[// NestedTypealias Aaa.]

