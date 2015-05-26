// NOTE: This file is sensitive to line numbers.  Thus RUN and CHECK lines come
// below the code.
//
// NOTE: Please don't change this file to use FileCheck's feature to match
// relative line numbers: those lines are comments and we don't want to see
// anything extra in a test for documentation comments.




//===--- Check that we attach comments to all kinds of declarations.

/// decl_func_1 Aaa.
func decl_func_1() {}

/// decl_func_2 Aaa.
func decl_func_2<T>(t: T) {}

/// decl_func_3 Aaa.
@unknown_attribute
func decl_func_3() {}

@unknown_attribute
/// decl_func_4 Aaa.  IS_DOC_NOT_ATTACHED
func decl_func_4() {}

@unknown_attribute
func
/// decl_func_5 Aaa.  IS_DOC_NOT_ATTACHED
  decl_func_5() {}


/// decl_var_1 Aaa.
var decl_var_1: Int

/// decl_var_2 decl_var_3 Aaa.
var (decl_var_2, decl_var_3): (Int, Float)

/// decl_var_4 Aaa.
@unknown_attribute
var decl_var_4: Int

/// decl_var_5 Aaa.
var decl_var_5: Int {
  get {}
  set {}
}

var decl_var_6: Int {
  /// decl_var_6_get Aaa.
  get {}
/// decl_var_6_set Aaa.
  set {}
}

/// decl_var_7 Aaa.
var decl_var_7: Int = 0 {
  willSet {}
  didSet {}
}

var decl_var_8: Int = 0 {
  /// decl_var_8_get Aaa.
  willSet {}
  /// decl_var_8_set Aaa.
  didSet {}
}


/// decl_val_1 Aaa.
let decl_val_1: Int = 0

/// decl_val_2 decl_val_3 Aaa.
let (decl_val_2, decl_val_3): (Int, Float) = (0, 0.0)

/// decl_val_4 Aaa.
@unknown_attribute
let decl_val_4: Int = 0


/// decl_typealias_1 Aaa.
typealias decl_typealias_1 = Int

/// decl_struct_1 Aaa.
struct decl_struct_1 {
  /// instanceVar1 Aaa.
  var instanceVar1: Int

  /// instanceVar2 instanceVar3 Aaa.
  var (instanceVar2, instanceVar3): (Int, Float)

  /// instanceVar4 Aaa.
  @unknown_attribute
  var instanceVar4: Int

  /// instanceVar5 Aaa.
  var instanceVar5: Int {
    get {}
    set {}
  }

  var instanceVar6: Int {
    /// instanceVar6_get Aaa.
    get {}
    /// instanceVar6_set Aaa.
    set {}
  }

  /// instanceVar7 Aaa.
  var instanceVar7: Int = 0 {
    willSet {}
    didSet {}
  }

  var instanceVar8: Int = 0 {
    /// instanceVar8_get Aaa.
    willSet {}
    /// instanceVar8_set Aaa.
    didSet {}
  }


  /// instanceFunc1 Aaa.
  func instanceFunc1() {}

  /// instanceFunc2 Aaa.
  @mutable
  func instanceFunc2() {}

  /// instanceFunc3 Aaa.
  func instanceFunc3<T>(t: T) {}

  /// instanceFunc4 Aaa.
  @unknown_attribute
  func instanceFunc4() {}

  /// init().  Aaa.
  init() {}

  /// subscript Aaa.
  subscript(i: Int) -> Double { return 0.0 }

  /// NestedStruct Aaa.
  struct NestedStruct {}

  /// NestedClass Aaa.
  class NestedClass {}

  /// NestedEnum Aaa.
  enum NestedEnum {}

  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  /// NestedTypealias Aaa.
  typealias NestedTypealias = Int

  /// staticVar Aaa.
  static var staticVar: Int = 4

  /// staticFunc1 Aaa.
  static func staticFunc1() {}
}

/// decl_enum_1 Aaa.
enum decl_enum_1 {
  /// Case1 Aaa.
  case Case1

  /// Case2 Aaa.
  case Case2(Int)

  /// Case3 Aaa.
  case Case3(Int, Float)

  /// Case4 Case5 Aaa.
  case Case4, Case5
}

/// decl_class_1 Aaa.
class decl_class_1 {
}

/// decl_protocol_1 Aaa.
protocol decl_protocol_1 {
  /// NestedTypealias Aaa.
  typealias NestedTypealias

  /// instanceFunc1 Aaa.
  func instanceFunc1()

  /// propertyWithGet Aaa.
  var propertyWithGet: Int { get }

  /// propertyWithGetSet Aaa.
  var propertyWithGetSet: Int { get set }
}

// FIXME: While there is nothing stopping us from attaching comments to
// extensions, how would we use those comments?

/// decl_extension_1 Aaa.
extension decl_extension_1 {
}

/***/
func emptyBlockDocComment() {}

/**/
func weirdBlockDocComment() {}

/**
func unterminatedBlockDocComment() {}

// RUN: %target-swift-ide-test -print-comments -source-filename %s > %t.txt
// RUN: FileCheck %s -check-prefix=WRONG < %t.txt
// RUN: FileCheck %s < %t.txt

// Some comments are not attached to anything.
// WRONG-NOT: IS_DOC_NOT_ATTACHED

// CHECK: comment_attach.swift:14:6: Func/decl_func_1 RawComment=[/// decl_func_1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:17:6: Func/decl_func_2 RawComment=[/// decl_func_2 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:17:21: Param/t RawComment=none
// CHECK-NEXT: comment_attach.swift:21:6: Func/decl_func_3 RawComment=[/// decl_func_3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:25:6: Func/decl_func_4 RawComment=none
// CHECK-NEXT: comment_attach.swift:30:3: Func/decl_func_5 RawComment=none
// CHECK-NEXT: comment_attach.swift:34:5: Var/decl_var_1 RawComment=[/// decl_var_1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:37:6: Var/decl_var_2 RawComment=[/// decl_var_2 decl_var_3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:37:18: Var/decl_var_3 RawComment=[/// decl_var_2 decl_var_3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:41:5: Var/decl_var_4 RawComment=[/// decl_var_4 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:44:5: Var/decl_var_5 RawComment=[/// decl_var_5 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:45:3: Func/<getter for decl_var_5> RawComment=none
// CHECK-NEXT: comment_attach.swift:46:3: Func/<setter for decl_var_5> RawComment=none
// CHECK-NEXT: comment_attach.swift:49:5: Var/decl_var_6 RawComment=none
// CHECK-NEXT: comment_attach.swift:51:3: Func/<getter for decl_var_6> RawComment=none
// CHECK-NEXT: comment_attach.swift:53:3: Func/<setter for decl_var_6> RawComment=none
// CHECK-NEXT: comment_attach.swift:57:5: Var/decl_var_7 RawComment=[/// decl_var_7 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:58:3: Func/<willSet for decl_var_7> RawComment=none
// CHECK-NEXT: comment_attach.swift:59:3: Func/<didSet for decl_var_7> RawComment=none
// CHECK-NEXT: comment_attach.swift:62:5: Var/decl_var_8 RawComment=none
// CHECK-NEXT: comment_attach.swift:64:3: Func/<willSet for decl_var_8> RawComment=none
// CHECK-NEXT: comment_attach.swift:66:3: Func/<didSet for decl_var_8> RawComment=none
// CHECK-NEXT: comment_attach.swift:71:5: Var/decl_val_1 RawComment=[/// decl_val_1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:74:6: Var/decl_val_2 RawComment=[/// decl_val_2 decl_val_3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:74:18: Var/decl_val_3 RawComment=[/// decl_val_2 decl_val_3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:78:5: Var/decl_val_4 RawComment=[/// decl_val_4 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:82:11: TypeAlias/decl_typealias_1 RawComment=[/// decl_typealias_1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:85:8: Struct/decl_struct_1 RawComment=[/// decl_struct_1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:87:7: Var/decl_struct_1.instanceVar1 RawComment=[/// instanceVar1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:90:8: Var/decl_struct_1.instanceVar2 RawComment=[/// instanceVar2 instanceVar3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:90:22: Var/decl_struct_1.instanceVar3 RawComment=[/// instanceVar2 instanceVar3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:94:7: Var/decl_struct_1.instanceVar4 RawComment=[/// instanceVar4 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:97:7: Var/decl_struct_1.instanceVar5 RawComment=[/// instanceVar5 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:98:5: Func/decl_struct_1.<getter for decl_struct_1.instanceVar5> RawComment=none
// CHECK-NEXT: comment_attach.swift:99:5: Func/decl_struct_1.<setter for decl_struct_1.instanceVar5> RawComment=none
// CHECK-NEXT: comment_attach.swift:102:7: Var/decl_struct_1.instanceVar6 RawComment=none
// CHECK-NEXT: comment_attach.swift:104:5: Func/decl_struct_1.<getter for decl_struct_1.instanceVar6> RawComment=none
// CHECK-NEXT: comment_attach.swift:106:5: Func/decl_struct_1.<setter for decl_struct_1.instanceVar6> RawComment=none
// CHECK-NEXT: comment_attach.swift:110:7: Var/decl_struct_1.instanceVar7 RawComment=[/// instanceVar7 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:111:5: Func/decl_struct_1.<willSet for decl_struct_1.instanceVar7> RawComment=none
// CHECK-NEXT: comment_attach.swift:112:5: Func/decl_struct_1.<didSet for decl_struct_1.instanceVar7> RawComment=none
// CHECK-NEXT: comment_attach.swift:115:7: Var/decl_struct_1.instanceVar8 RawComment=none
// CHECK-NEXT: comment_attach.swift:117:5: Func/decl_struct_1.<willSet for decl_struct_1.instanceVar8> RawComment=none
// CHECK-NEXT: comment_attach.swift:119:5: Func/decl_struct_1.<didSet for decl_struct_1.instanceVar8> RawComment=none
// CHECK-NEXT: comment_attach.swift:124:8: Func/decl_struct_1.instanceFunc1 RawComment=[/// instanceFunc1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:128:8: Func/decl_struct_1.instanceFunc2 RawComment=[/// instanceFunc2 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:131:8: Func/decl_struct_1.instanceFunc3 RawComment=[/// instanceFunc3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:131:25: Param/t RawComment=none
// CHECK-NEXT: comment_attach.swift:135:8: Func/decl_struct_1.instanceFunc4 RawComment=[/// instanceFunc4 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:138:3: Constructor/decl_struct_1.init RawComment=[/// init().  Aaa.\n] BriefComment=[init().  Aaa.]
// CHECK-NEXT: comment_attach.swift:141:3: Subscript/decl_struct_1.subscript RawComment=[/// subscript Aaa.\n]
// CHECK-NEXT: comment_attach.swift:141:13: Param/decl_struct_1.i RawComment=none
// CHECK-NEXT: comment_attach.swift:141:31: Func/decl_struct_1.<getter for decl_struct_1.subscript> RawComment=none
// CHECK-NEXT: comment_attach.swift:144:10: Struct/decl_struct_1.NestedStruct RawComment=[/// NestedStruct Aaa.\n]
// CHECK-NEXT: comment_attach.swift:147:9: Class/decl_struct_1.NestedClass RawComment=[/// NestedClass Aaa.\n]
// CHECK-NEXT: comment_attach.swift:150:8: Enum/decl_struct_1.NestedEnum RawComment=[/// NestedEnum Aaa.\n]
// CHECK-NEXT: comment_attach.swift:156:13: TypeAlias/decl_struct_1.NestedTypealias RawComment=[/// NestedTypealias Aaa.\n]
// CHECK-NEXT: comment_attach.swift:159:14: Var/decl_struct_1.staticVar RawComment=[/// staticVar Aaa.\n]
// CHECK-NEXT: comment_attach.swift:162:15: Func/decl_struct_1.staticFunc1 RawComment=[/// staticFunc1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:166:6: Enum/decl_enum_1 RawComment=[/// decl_enum_1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:168:8: EnumElement/decl_enum_1.Case1 RawComment=[/// Case1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:171:8: EnumElement/decl_enum_1.Case2 RawComment=[/// Case2 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:174:8: EnumElement/decl_enum_1.Case3 RawComment=[/// Case3 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:177:8: EnumElement/decl_enum_1.Case4 RawComment=[/// Case4 Case5 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:177:15: EnumElement/decl_enum_1.Case5 RawComment=[/// Case4 Case5 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:181:7: Class/decl_class_1 RawComment=[/// decl_class_1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:185:10: Protocol/decl_protocol_1 RawComment=[/// decl_protocol_1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:187:13: AssociatedType/decl_protocol_1.NestedTypealias RawComment=[/// NestedTypealias Aaa.\n]
// CHECK-NEXT: comment_attach.swift:190:8: Func/decl_protocol_1.instanceFunc1 RawComment=[/// instanceFunc1 Aaa.\n]
// CHECK-NEXT: comment_attach.swift:193:7: Var/decl_protocol_1.propertyWithGet RawComment=[/// propertyWithGet Aaa.\n]
// CHECK-NEXT: comment_attach.swift:193:30: Func/decl_protocol_1.<getter for decl_protocol_1.propertyWithGet> RawComment=none
// CHECK-NEXT: comment_attach.swift:196:7: Var/decl_protocol_1.propertyWithGetSet RawComment=[/// propertyWithGetSet Aaa.\n]
// CHECK-NEXT: comment_attach.swift:196:33: Func/decl_protocol_1.<getter for decl_protocol_1.propertyWithGetSet> RawComment=none
// CHECK-NEXT: comment_attach.swift:196:37: Func/decl_protocol_1.<setter for decl_protocol_1.propertyWithGetSet> RawComment=none
// CHECK-NEXT: comment_attach.swift:207:6: Func/emptyBlockDocComment RawComment=[/***/]
// CHECK-NEXT: comment_attach.swift:210:6: Func/weirdBlockDocComment RawComment=[/**/]
