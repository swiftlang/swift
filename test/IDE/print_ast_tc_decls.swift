// RUN: rm -rf %t
// RUN: mkdir %t
//
// Build swift modules this test depends on.
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/foo_swift_module.swift
//
// This file should not have any syntax or type checker errors.
// RUN: %target-swift-frontend -parse -verify -I %t %clang-importer-sdk -F %S/Inputs/mock-sdk %s
//
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -I %t %clang-importer-sdk -F %S/Inputs/mock-sdk -function-definitions=false -prefer-type-repr=false -print-implicit-attrs=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_COMMON -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_PRINT_AST -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_RW_PROP_GET_SET -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_2200 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_2500 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_ONE_LINE -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_ONE_LINE_TYPE -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PREFER_TYPE_PRINTING -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_QUAL_UNQUAL -strict-whitespace < %t.printed.txt
//
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -I %t %clang-importer-sdk -F %S/Inputs/mock-sdk -function-definitions=false -prefer-type-repr=true -print-implicit-attrs=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_COMMON -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_PRINT_AST -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_RW_PROP_GET_SET -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_2200 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_2500 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_ONE_LINE -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_ONE_LINE_TYPEREPR -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PREFER_TYPE_REPR_PRINTING -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_QUAL_UNQUAL -strict-whitespace < %t.printed.txt
//
// RUN: %target-swift-frontend -emit-module -o %t -I %t %clang-importer-sdk -F %S/Inputs/mock-sdk %s
// RUN: %target-swift-ide-test -skip-deinit=false -print-module -source-filename %s %clang-importer-sdk -F %S/Inputs/mock-sdk -I %t -module-to-print=print_ast_tc_decls -print-implicit-attrs=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_COMMON -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_PRINT_MODULE_INTERFACE -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_RW_PROP_NO_GET_SET -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_2200_DESERIALIZED -strict-whitespace < %t.printed.txt
// FIXME: rdar://15167697
// FIXME: FileCheck %s -check-prefix=PASS_2500 -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_ONE_LINE -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PREFER_TYPE_PRINTING -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_QUAL_UNQUAL -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_EXPLODE_PATTERN -strict-whitespace < %t.printed.txt
//
// RUN: %target-swift-ide-test -skip-deinit=false -print-module -source-filename %s %clang-importer-sdk -F %S/Inputs/mock-sdk -I %t -module-to-print=print_ast_tc_decls -synthesize-sugar-on-types=true -print-implicit-attrs=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_PRINT_MODULE_INTERFACE -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_QUAL_UNQUAL -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=SYNTHESIZE_SUGAR_ON_TYPES -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_EXPLODE_PATTERN -strict-whitespace < %t.printed.txt

// RUN: %target-swift-ide-test -skip-deinit=false -print-module -source-filename %s %clang-importer-sdk -F %S/Inputs/mock-sdk -I %t -module-to-print=print_ast_tc_decls -synthesize-sugar-on-types=true -fully-qualified-types-if-ambiguous=true -print-implicit-attrs=true > %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_PRINT_MODULE_INTERFACE -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=PASS_QUAL_IF_AMBIGUOUS -strict-whitespace < %t.printed.txt
// RUN: FileCheck %s -check-prefix=SYNTHESIZE_SUGAR_ON_TYPES -strict-whitespace < %t.printed.txt
// FIXME: FileCheck %s -check-prefix=PASS_EXPLODE_PATTERN -strict-whitespace < %t.printed.txt

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import Bar
import ObjectiveC
import class Foo.FooClassBase
import func Foo.fooFunc1
@exported import FooHelper
import struct ctypes.FooStruct1
import foo_swift_module

// FIXME: enum tests
//import enum FooClangModule.FooEnum1

// PASS_COMMON: {{^}}import Bar{{$}}
// PASS_COMMON: {{^}}import class Foo.FooClassBase{{$}}
// PASS_COMMON: {{^}}import func Foo.fooFunc1{{$}}
// PASS_COMMON: {{^}}@exported import FooHelper{{$}}
// PASS_COMMON: {{^}}import struct ctypes.FooStruct1{{$}}
// PASS_COMMON: {{^}}import foo_swift_module{{$}}

//===---
//===--- Helper types.
//===---

struct FooStruct {}

class FooClass {}
class BarClass {}

protocol FooProtocol {}
protocol BarProtocol {}
protocol BazProtocol { func baz() }
protocol QuxProtocol {
  typealias Qux
}

class FooProtocolImpl : FooProtocol {}
class FooBarProtocolImpl : FooProtocol, BarProtocol {}
class BazProtocolImpl : BazProtocol { func baz() {} }

//===---
//===--- Basic smoketest.
//===---

struct d0100_FooStruct {
// PASS_COMMON-LABEL: {{^}}struct d0100_FooStruct {{{$}}

  var instanceVar1: Int = 0
// PASS_COMMON-NEXT: {{^}}  var instanceVar1: Int{{$}}

  var computedProp1: Int {
    get {
      return 42
    }
  }
// PASS_COMMON-NEXT: {{^}}  var computedProp1: Int { get }{{$}}

  func instanceFunc0() {}
// PASS_COMMON-NEXT: {{^}}  func instanceFunc0(){{$}}

  func instanceFunc1(a: Int) {}
// PASS_COMMON-NEXT: {{^}}  func instanceFunc1(a: Int){{$}}

  func instanceFunc2(a: Int, inout b: Double) {}
// PASS_COMMON-NEXT: {{^}}  func instanceFunc2(a: Int, inout b: Double){{$}}

  func instanceFunc3(var a: Int, let b: Double) {}
// PASS_COMMON-NEXT: {{^}}  func instanceFunc3(a: Int, b: Double){{$}}

  func instanceFuncWithDefaultArg1(a: Int = 0) {}
// PASS_COMMON-NEXT: {{^}}  func instanceFuncWithDefaultArg1(a: Int = default){{$}}

  func instanceFuncWithDefaultArg2(a: Int = 0, b: Double = 0) {}
// PASS_COMMON-NEXT: {{^}}  func instanceFuncWithDefaultArg2(a: Int = default, b: Double = default){{$}}

  func varargInstanceFunc0(v: Int...) {}
// PASS_COMMON-NEXT: {{^}}  func varargInstanceFunc0(v: Int...){{$}}

  func varargInstanceFunc1(a: Float, v: Int...) {}
// PASS_COMMON-NEXT: {{^}}  func varargInstanceFunc1(a: Float, v: Int...){{$}}

  func varargInstanceFunc2(a: Float, b: Double, v: Int...) {}
// PASS_COMMON-NEXT: {{^}}  func varargInstanceFunc2(a: Float, b: Double, v: Int...){{$}}

  func overloadedInstanceFunc1() -> Int { return 0; }
// PASS_COMMON-NEXT: {{^}}  func overloadedInstanceFunc1() -> Int{{$}}

  func overloadedInstanceFunc1() -> Double { return 0.0; }
// PASS_COMMON-NEXT: {{^}}  func overloadedInstanceFunc1() -> Double{{$}}

  func overloadedInstanceFunc2(x: Int) -> Int { return 0; }
// PASS_COMMON-NEXT: {{^}}  func overloadedInstanceFunc2(x: Int) -> Int{{$}}

  func overloadedInstanceFunc2(x: Double) -> Int { return 0; }
// PASS_COMMON-NEXT: {{^}}  func overloadedInstanceFunc2(x: Double) -> Int{{$}}

  func builderFunc1(a: Int) -> d0100_FooStruct { return d0100_FooStruct(); }
// PASS_COMMON-NEXT: {{^}}  func builderFunc1(a: Int) -> d0100_FooStruct{{$}}

  subscript(i: Int) -> Double {
    get {
      return Double(i)
    }
  }
// PASS_COMMON-NEXT: {{^}}  subscript (i: Int) -> Double { get }{{$}}

  subscript(i: Int, j: Int) -> Double {
    get {
      return Double(i + j)
    }
  }
// PASS_COMMON-NEXT: {{^}}  subscript (i: Int, j: Int) -> Double { get }{{$}}

  func curriedVoidFunc1()() {}
// PASS_COMMON-NEXT: {{^}}  func curriedVoidFunc1()(){{$}}

  func curriedVoidFunc2()(a: Int) {}
// PASS_COMMON-NEXT: {{^}}  func curriedVoidFunc2()(a: Int){{$}}

  func curriedVoidFunc3(a: Int)() {}
// PASS_COMMON-NEXT: {{^}}  func curriedVoidFunc3(a: Int)(){{$}}

  func curriedVoidFunc4(a: Int)(b: Int) {}
// PASS_COMMON-NEXT: {{^}}  func curriedVoidFunc4(a: Int)(b: Int){{$}}

  func curriedStringFunc1()() -> String { return "" }
// PASS_COMMON-NEXT: {{^}}  func curriedStringFunc1()() -> String{{$}}

  func curriedStringFunc2()(a: Int) -> String { return "" }
// PASS_COMMON-NEXT: {{^}}  func curriedStringFunc2()(a: Int) -> String{{$}}

  func curriedStringFunc3(a: Int)() -> String { return "" }
// PASS_COMMON-NEXT: {{^}}  func curriedStringFunc3(a: Int)() -> String{{$}}

  func curriedStringFunc4(a: Int)(b: Int) -> String { return "" }
// PASS_COMMON-NEXT: {{^}}  func curriedStringFunc4(a: Int)(b: Int) -> String{{$}}

  func bodyNameVoidFunc1(a: Int, b x: Float) {}
// PASS_COMMON-NEXT: {{^}}  func bodyNameVoidFunc1(a: Int, b x: Float){{$}}

  func bodyNameVoidFunc2(a: Int, b x: Float, c y: Double) {}
// PASS_COMMON-NEXT: {{^}}  func bodyNameVoidFunc2(a: Int, b x: Float, c y: Double){{$}}

  func bodyNameStringFunc1(a: Int, b x: Float) -> String { return "" }
// PASS_COMMON-NEXT: {{^}}  func bodyNameStringFunc1(a: Int, b x: Float) -> String{{$}}

  func bodyNameStringFunc2(a: Int, b x: Float, c y: Double) -> String { return "" }
// PASS_COMMON-NEXT: {{^}}  func bodyNameStringFunc2(a: Int, b x: Float, c y: Double) -> String{{$}}

  struct NestedStruct {}
// PASS_COMMON-NEXT: {{^}}  struct NestedStruct {{{$}}
// PASS_COMMON-NEXT: {{^}}    init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  class NestedClass {}
// PASS_COMMON-NEXT: {{^}}  class NestedClass {{{$}}
// PASS_COMMON-NEXT: {{^}}    @objc deinit {{$}}
// PASS_COMMON-NEXT: {{^}}    init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  enum NestedEnum {}
// PASS_COMMON-NEXT: {{^}}  enum NestedEnum {{{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  // Can not declare a nested protocol.
  // protocol NestedProtocol {}

  typealias NestedTypealias = Int
// PASS_COMMON-NEXT: {{^}}  typealias NestedTypealias = Int{{$}}

  static var staticVar1: Int = 42
// PASS_COMMON-NEXT: {{^}}  static var staticVar1: Int{{$}}

  static var computedStaticProp1: Int {
    get {
      return 42
    }
  }
// PASS_COMMON-NEXT: {{^}}  static var computedStaticProp1: Int { get }{{$}}

  static func staticFunc0() {}
// PASS_COMMON-NEXT: {{^}}  static func staticFunc0(){{$}}

  static func staticFunc1(a: Int) {}
// PASS_COMMON-NEXT: {{^}}  static func staticFunc1(a: Int){{$}}

  static func overloadedStaticFunc1() -> Int { return 0 }
// PASS_COMMON-NEXT: {{^}}  static func overloadedStaticFunc1() -> Int{{$}}

  static func overloadedStaticFunc1() -> Double { return 0.0 }
// PASS_COMMON-NEXT: {{^}}  static func overloadedStaticFunc1() -> Double{{$}}

  static func overloadedStaticFunc2(x: Int) -> Int { return 0 }
// PASS_COMMON-NEXT: {{^}}  static func overloadedStaticFunc2(x: Int) -> Int{{$}}

  static func overloadedStaticFunc2(x: Double) -> Int { return 0 }
// PASS_COMMON-NEXT: {{^}}  static func overloadedStaticFunc2(x: Double) -> Int{{$}}
}
// PASS_COMMON-NEXT: {{^}}  init(instanceVar1: Int){{$}}
// PASS_COMMON-NEXT: {{^}}  init(){{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}

extension d0100_FooStruct {
// PASS_COMMON-LABEL: {{^}}extension d0100_FooStruct {{{$}}

  var extProp: Int {
    get {
      return 42
    }
  }
// PASS_COMMON-NEXT: {{^}}  var extProp: Int { get }{{$}}

  func extFunc0() {}
// PASS_COMMON-NEXT: {{^}}  func extFunc0(){{$}}

  static var extStaticProp: Int {
    get {
      return 42
    }
  }
// PASS_COMMON-NEXT: {{^}}  static var extStaticProp: Int { get }{{$}}

  static func extStaticFunc0() {}
// PASS_COMMON-NEXT: {{^}}  static func extStaticFunc0(){{$}}

  struct ExtNestedStruct {}
// PASS_COMMON-NEXT: {{^}}  struct ExtNestedStruct {{{$}}
// PASS_COMMON-NEXT: {{^}}    init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  class ExtNestedClass {}
// PASS_COMMON-NEXT: {{^}}  class ExtNestedClass {{{$}}
// PASS_COMMON-NEXT: {{^}}    @objc deinit {{$}}
// PASS_COMMON-NEXT: {{^}}    init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  enum ExtNestedEnum {
    case ExtEnumX(Int)
  }
// PASS_COMMON-NEXT: {{^}}  enum ExtNestedEnum {{{$}}
// PASS_COMMON-NEXT: {{^}}    case ExtEnumX(Int){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  typealias ExtNestedTypealias = Int
// PASS_COMMON-NEXT: {{^}}  typealias ExtNestedTypealias = Int{{$}}
}
// PASS_COMMON-NEXT: {{^}}}{{$}}

extension d0100_FooStruct.NestedStruct {
// PASS_COMMON-LABEL: {{^}}extension d0100_FooStruct.NestedStruct {{{$}}
  struct ExtNestedStruct2 {}
// PASS_COMMON-NEXT: {{^}}  struct ExtNestedStruct2 {{{$}}
// PASS_COMMON-NEXT: {{^}}    init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}
}

extension d0100_FooStruct.ExtNestedStruct {
// PASS_COMMON-LABEL: {{^}}extension d0100_FooStruct.ExtNestedStruct {{{$}}
  struct ExtNestedStruct3 {}
// PASS_COMMON-NEXT: {{^}}  struct ExtNestedStruct3 {{{$}}
// PASS_COMMON-NEXT: {{^}}    init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}
}

// PASS_COMMON-NEXT: {{^}}}{{$}}

var fooObject: d0100_FooStruct = d0100_FooStruct()
// PASS_ONE_LINE-DAG: {{^}}var fooObject: d0100_FooStruct{{$}}

struct d0110_ReadWriteProperties {
// PASS_RW_PROP_GET_SET-LABEL:    {{^}}struct d0110_ReadWriteProperties {{{$}}
// PASS_RW_PROP_NO_GET_SET-LABEL: {{^}}struct d0110_ReadWriteProperties {{{$}}

  var computedProp1: Int {
    get {
      return 42
    }
    set {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  var computedProp1: Int { get set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  var computedProp1: Int{{$}}

  subscript(i: Int) -> Int {
    get {
      return 42
    }
    set {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  subscript (i: Int) -> Int { get set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  subscript (i: Int) -> Int{{$}}

  static var computedStaticProp1: Int {
    get {
      return 42
    }
    set {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  static var computedStaticProp1: Int { get set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  static var computedStaticProp1: Int{{$}}

  var computedProp2: Int {
    mutating get {
      return 42
    }
    set {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  var computedProp2: Int { mutating get set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  var computedProp2: Int { mutating get set }{{$}}

  var computedProp3: Int {
    get {
      return 42
    }
    nonmutating set {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  var computedProp3: Int { get nonmutating set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  var computedProp3: Int { get nonmutating set }{{$}}

  var computedProp4: Int {
    mutating get {
      return 42
    }
    nonmutating set {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  var computedProp4: Int { mutating get nonmutating set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  var computedProp4: Int { mutating get nonmutating set }{{$}}

  subscript(i: Float) -> Int {
    get {
      return 42
    }
    nonmutating set {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  subscript (i: Float) -> Int { get nonmutating set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  subscript (i: Float) -> Int { get nonmutating set }{{$}}
}
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  init(){{$}}
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}}{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  init(){{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}}{{$}}

extension d0110_ReadWriteProperties {
// PASS_RW_PROP_GET_SET-LABEL:    {{^}}extension d0110_ReadWriteProperties {{{$}}
// PASS_RW_PROP_NO_GET_SET-LABEL: {{^}}extension d0110_ReadWriteProperties {{{$}}

  var extProp: Int {
    get {
      return 42
    }
    set(v) {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  var extProp: Int { get set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  var extProp: Int{{$}}

  static var extStaticProp: Int {
    get {
      return 42
    }
    set(v) {}
  }
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}  static var extStaticProp: Int { get set }{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}  static var extStaticProp: Int{{$}}
}
// PASS_RW_PROP_GET_SET-NEXT:    {{^}}}{{$}}
// PASS_RW_PROP_NO_GET_SET-NEXT: {{^}}}{{$}}

class d0120_TestClassBase {
// PASS_COMMON-LABEL: {{^}}class d0120_TestClassBase {{{$}}

  required init() {}
// PASS_COMMON-NEXT: {{^}}  required init(){{$}}

  // FIXME: Add these once we can SILGen them reasonable.
  // init?(fail: String) { }
  // init!(iuoFail: String) { }

  final func baseFunc1() {}
// PASS_COMMON-NEXT: {{^}}  final func baseFunc1(){{$}}

  func baseFunc2() {}
// PASS_COMMON-NEXT: {{^}}  func baseFunc2(){{$}}

  subscript(i: Int) -> Int {
    return 0
  }
// PASS_COMMON-NEXT: {{^}}  subscript (i: Int) -> Int { get }{{$}}
}

class d0121_TestClassDerived : d0120_TestClassBase {
// PASS_COMMON-LABEL: {{^}}class d0121_TestClassDerived : d0120_TestClassBase {{{$}}

  required init() { super.init() }
// PASS_COMMON-NEXT: {{^}}  required init(){{$}}

  final override func baseFunc2() {}
// PASS_COMMON-NEXT: {{^}}  {{(override |final )+}}func baseFunc2(){{$}}

  override final subscript(i: Int) -> Int {
    return 0
  }
// PASS_COMMON-NEXT: {{^}}  override final subscript (i: Int) -> Int { get }{{$}}
}

protocol d0130_TestProtocol {
// PASS_COMMON-LABEL: {{^}}protocol d0130_TestProtocol {{{$}}

  typealias NestedTypealias
// PASS_COMMON-NEXT: {{^}}  typealias NestedTypealias{{$}}

  var property1: Int { get }
// PASS_COMMON-NEXT: {{^}}  var property1: Int { get }{{$}}

  var property2: Int { get set }
// PASS_COMMON-NEXT: {{^}}  var property2: Int { get set }{{$}}

  func protocolFunc1()
// PASS_COMMON-NEXT: {{^}}  func protocolFunc1(){{$}}
}

@objc protocol d0140_TestObjCProtocol {
// PASS_COMMON-LABEL: {{^}}@objc protocol d0140_TestObjCProtocol {{{$}}

  optional var property1: Int { get }
// PASS_COMMON-NEXT: {{^}}  @objc optional var property1: Int { get }{{$}}

  optional func protocolFunc1()
// PASS_COMMON-NEXT: {{^}}  @objc optional func protocolFunc1(){{$}}
}

protocol d0150_TestClassProtocol : class {}
// PASS_COMMON-LABEL: {{^}}protocol d0150_TestClassProtocol : class {{{$}}

@objc protocol d0151_TestClassProtocol {}
// PASS_COMMON-LABEL: {{^}}@objc protocol d0151_TestClassProtocol {{{$}}


@noreturn @asmname("exit") func d0160_testNoReturn()
// PASS_COMMON-LABEL: {{^}}@asmname("exit"){{$}}
// PASS_COMMON-NEXT: {{^}}@noreturn func d0160_testNoReturn(){{$}}

@noreturn func d0161_testNoReturn() { d0160_testNoReturn() }
// PASS_COMMON-LABEL: {{^}}@noreturn func d0161_testNoReturn(){{$}}

class d0162_TestNoReturn {
// PASS_COMMON-LABEL: {{^}}class d0162_TestNoReturn {{{$}}

  @noreturn func instanceFunc() { d0160_testNoReturn() }
// PASS_COMMON-NEXT: {{^}}  @noreturn func instanceFunc(){{$}}

  @noreturn func classFunc() {d0160_testNoReturn() }
// PASS_COMMON-NEXT: {{^}}  @noreturn func classFunc(){{$}}
}


class d0170_TestAvailability {
// PASS_COMMON-LABEL: {{^}}class d0170_TestAvailability {{{$}}

  @availability(*, unavailable)
  func f1() {}
// PASS_COMMON-NEXT: {{^}}  @availability(*, unavailable){{$}}
// PASS_COMMON-NEXT: {{^}}  func f1(){{$}}

  @availability(*, unavailable, message="aaa \"bbb\" ccc\nddd\0eee")
  func f2() {}
// PASS_COMMON-NEXT: {{^}}  @availability(*, unavailable, message="aaa \"bbb\" ccc\nddd\0eee"){{$}}
// PASS_COMMON-NEXT: {{^}}  func f2(){{$}}

  @availability(iOS, unavailable)
  @availability(OSX, unavailable)
  func f3() {}
// PASS_COMMON-NEXT: {{^}}  @availability(iOS, unavailable){{$}}
// PASS_COMMON-NEXT: {{^}}  @availability(OSX, unavailable){{$}}
// PASS_COMMON-NEXT: {{^}}  func f3(){{$}}
}

@objc class d0180_TestIBAttrs {
// PASS_COMMON-LABEL: {{^}}@objc class d0180_TestIBAttrs {{{$}}

  @IBAction func anAction(_: AnyObject) {}
// PASS_COMMON-NEXT: {{^}}  @IBAction @objc func anAction(_: AnyObject){{$}}

  @IBDesignable
  class ADesignableClass {}
// PASS_COMMON-NEXT: {{^}}  @IBDesignable class ADesignableClass {{{$}}

}

@objc class d0181_TestIBAttrs {
// PASS_EXPLODE_PATTERN-LABEL: {{^}}@objc class d0181_TestIBAttrs {{{$}}

  @IBOutlet weak var anOutlet: d0181_TestIBAttrs!
// PASS_EXPLODE_PATTERN-NEXT: {{^}}  @IBOutlet @objc weak var anOutlet: @sil_weak d0181_TestIBAttrs!{{$}}

  @IBInspectable var inspectableProp: Int = 0
// PASS_EXPLODE_PATTERN-NEXT: {{^}}  @IBInspectable @objc var inspectableProp: Int{{$}}
}

struct d0190_LetVarDecls {
// PASS_PRINT_AST-LABEL: {{^}}struct d0190_LetVarDecls {{{$}}
// PASS_PRINT_MODULE_INTERFACE-LABEL: {{^}}struct d0190_LetVarDecls {{{$}}

  let instanceVar1: Int = 0
// PASS_PRINT_AST-NEXT: {{^}}  let instanceVar1: Int{{$}}
// PASS_PRINT_MODULE_INTERFACE-NEXT: {{^}}  let instanceVar1: Int{{$}}

  let instanceVar2 = 0
  // FIXME: PRINTED_WITHOUT_TYPE
// PASS_PRINT_AST-NEXT: {{^}}  let instanceVar2{{$}}
// PASS_PRINT_MODULE_INTERFACE-NEXT: {{^}}  let instanceVar2: Int{{$}}

  static let staticVar1: Int = 42
// PASS_PRINT_AST-NEXT: {{^}}  static let staticVar1: Int{{$}}
// PASS_PRINT_MODULE_INTERFACE-NEXT: {{^}}  static let staticVar1: Int{{$}}

  static let staticVar2 = 42
  // FIXME: PRINTED_WITHOUT_TYPE
// PASS_PRINT_AST-NEXT: {{^}}  static let staticVar2{{$}}
// PASS_PRINT_MODULE_INTERFACE-NEXT: {{^}}  static let staticVar2: Int{{$}}
}

struct d0200_EscapedIdentifiers {
// PASS_COMMON-LABEL: {{^}}struct d0200_EscapedIdentifiers {{{$}}

  struct `struct` {}
// PASS_COMMON-NEXT: {{^}}  struct `struct` {{{$}}
// PASS_COMMON-NEXT: {{^}}    init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  enum `enum` {
    case `case`
  }
// PASS_COMMON-NEXT: {{^}}  enum `enum` {{{$}}
// PASS_COMMON-NEXT: {{^}}    case `case`{{$}}
// PASS_COMMON-NEXT: {{^}}    var hashValue: Int { get }{{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  class `class` {}
// PASS_COMMON-NEXT: {{^}}  class `class` {{{$}}
// PASS_COMMON-NEXT: {{^}}    @objc deinit {{$}}
// PASS_COMMON-NEXT: {{^}}    init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  typealias `protocol` = `class`
// PASS_ONE_LINE_TYPE-DAG: {{^}}  typealias `protocol` = d0200_EscapedIdentifiers.`class`{{$}}
// PASS_ONE_LINE_TYPEREPR-DAG: {{^}}  typealias `protocol` = `class`{{$}}

  class `extension` : `class` {}
// PASS_ONE_LINE_TYPE-DAG: {{^}}  class `extension` : d0200_EscapedIdentifiers.`class` {{{$}}
// PASS_ONE_LINE_TYPEREPR-DAG: {{^}}  class `extension` : `class` {{{$}}
// PASS_COMMON: {{^}}    @objc deinit {{$}}
// PASS_COMMON-NEXT: {{^}}    {{(override )?}}init(){{$}}
// PASS_COMMON-NEXT: {{^}}  }{{$}}

  func `func`<`let`: `protocol`, `where` where `where` : `protocol`>(
      `class`: Int, `struct`: `protocol`, `foo`: `let`) {}
// PASS_COMMON-NEXT: {{^}}  func `func`<`let` : `protocol`, `where` where where : `protocol`>(`class`: Int, `struct`: `protocol`, foo: let){{$}}

  var `var`: `struct` = `struct`()
// PASS_COMMON-NEXT: {{^}}  var `var`: {{(d0200_EscapedIdentifiers.)?}}`struct`{{$}}

  var tupleType: (`var`: Int, `let`: `struct`)
// PASS_COMMON-NEXT: {{^}}  var tupleType: (`var`: Int, `let`: {{(d0200_EscapedIdentifiers.)?}}`struct`){{$}}

  var accessors1: Int {
    get { return 0 }
    set(`let`) {}
  }
// PASS_COMMON-NEXT: {{^}}  var accessors1: Int{{( { get set })?}}{{$}}

  static func `static`(`protocol`: Int) {}
// PASS_COMMON-NEXT: {{^}}  static func `static`(`protocol`: Int){{$}}

// PASS_COMMON-NEXT: {{^}}  init(`var`: {{(d0200_EscapedIdentifiers.)?}}`struct`, tupleType: (`var`: Int, `let`: {{(d0200_EscapedIdentifiers.)?}}`struct`)){{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}
}

struct d0210_Qualifications {
// PASS_QUAL_UNQUAL:       {{^}}struct d0210_Qualifications {{{$}}
// PASS_QUAL_IF_AMBIGUOUS: {{^}}struct d0210_Qualifications {{{$}}

  var propFromStdlib1: Int = 0
// PASS_QUAL_UNQUAL-NEXT:       {{^}}  var propFromStdlib1: Int{{$}}
// PASS_QUAL_IF_AMBIGUOUS-NEXT: {{^}}  var propFromStdlib1: Int{{$}}

  var propFromSwift1: FooSwiftStruct = FooSwiftStruct()
// PASS_QUAL_UNQUAL-NEXT:       {{^}}  var propFromSwift1: FooSwiftStruct{{$}}
// PASS_QUAL_IF_AMBIGUOUS-NEXT: {{^}}  var propFromSwift1: foo_swift_module.FooSwiftStruct{{$}}

  var propFromClang1: FooStruct1 = FooStruct1(x: 0, y: 0.0)
// PASS_QUAL_UNQUAL-NEXT:       {{^}}  var propFromClang1: FooStruct1{{$}}
// PASS_QUAL_IF_AMBIGUOUS-NEXT: {{^}}  var propFromClang1: FooStruct1{{$}}


  func instanceFuncFromStdlib1(a: Int) -> Float {
    return 0.0
  }
// PASS_QUAL_UNQUAL-NEXT:       {{^}}  func instanceFuncFromStdlib1(a: Int) -> Float{{$}}
// PASS_QUAL_IF_AMBIGUOUS-NEXT: {{^}}  func instanceFuncFromStdlib1(a: Int) -> Float{{$}}

  func instanceFuncFromStdlib2(a: ObjCBool) {}
// PASS_QUAL_UNQUAL-NEXT:       {{^}}  func instanceFuncFromStdlib2(a: ObjCBool){{$}}
// PASS_QUAL_IF_AMBIGUOUS-NEXT: {{^}}  func instanceFuncFromStdlib2(a: ObjCBool){{$}}

  func instanceFuncFromSwift1(a: FooSwiftStruct) -> FooSwiftStruct {
    return FooSwiftStruct()
  }
// PASS_QUAL_UNQUAL-NEXT:       {{^}}  func instanceFuncFromSwift1(a: FooSwiftStruct) -> FooSwiftStruct{{$}}
// PASS_QUAL_IF_AMBIGUOUS-NEXT: {{^}}  func instanceFuncFromSwift1(a: foo_swift_module.FooSwiftStruct) -> foo_swift_module.FooSwiftStruct{{$}}

  func instanceFuncFromClang1(a: FooStruct1) -> FooStruct1 {
    return FooStruct1(x: 0, y: 0.0)
  }
// PASS_QUAL_UNQUAL-NEXT:       {{^}}  func instanceFuncFromClang1(a: FooStruct1) -> FooStruct1{{$}}
// PASS_QUAL_IF_AMBIGUOUS-NEXT: {{^}}  func instanceFuncFromClang1(a: FooStruct1) -> FooStruct1{{$}}
}

// FIXME: this should be printed reasonably in case we use
// -prefer-type-repr=true.  Either we should print the types we inferred, or we
// should print the initializers.
class d0250_ExplodePattern {
// PASS_EXPLODE_PATTERN-LABEL: {{^}}class d0250_ExplodePattern {{{$}}
  var instanceVar1 = 0
  var instanceVar2 = 0.0
  var instanceVar3 = ""
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar1: Int{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar2: Double{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar3: String{{$}}

  var instanceVar4 = FooStruct()
  var (instanceVar5, instanceVar6) = (FooStruct(), FooStruct())
  var (instanceVar7, instanceVar8) = (FooStruct(), FooStruct())
  var (instanceVar9, instanceVar10) : (FooStruct, FooStruct) = (FooStruct(), FooStruct())
  final var (instanceVar11, instanceVar12) = (FooStruct(), FooStruct())
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar4: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar5: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar6: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar7: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar8: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar9: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  var instanceVar10: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final var instanceVar11: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final var instanceVar12: FooStruct{{$}}

  let instanceLet1 = 0
  let instanceLet2 = 0.0
  let instanceLet3 = ""
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet1: Int{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet2: Double{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet3: String{{$}}

  let instanceLet4 = FooStruct()
  let (instanceLet5, instanceLet6) = (FooStruct(), FooStruct())
  let (instanceLet7, instanceLet8) = (FooStruct(), FooStruct())
  let (instanceLet9, instanceLet10) : (FooStruct, FooStruct) = (FooStruct(), FooStruct())
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet4: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet5: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet6: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet7: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet8: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet9: FooStruct{{$}}
// PASS_EXPLODE_PATTERN: {{^}}  final let instanceLet10: FooStruct{{$}}
}

class d0260_ExplodePattern_TestClassBase {
// PASS_EXPLODE_PATTERN-LABEL: {{^}}class d0260_ExplodePattern_TestClassBase {{{$}}

  init() {
    baseProp1 = 0
  }
// PASS_EXPLODE_PATTERN-NEXT: {{^}}  init(){{$}}

  final var baseProp1: Int
// PASS_EXPLODE_PATTERN-NEXT: {{^}}  final var baseProp1: Int{{$}}

  var baseProp2: Int {
    get {
      return 0
    }
    set {}
  }
// PASS_EXPLODE_PATTERN-NEXT: {{^}}  var baseProp2: Int{{$}}
}

class d0261_ExplodePattern_TestClassDerived : d0260_ExplodePattern_TestClassBase {
// PASS_EXPLODE_PATTERN-LABEL: {{^}}class d0261_ExplodePattern_TestClassDerived : d0260_ExplodePattern_TestClassBase {{{$}}

  override final var baseProp2: Int {
    get {
      return 0
    }
    set {}
  }
// PASS_EXPLODE_PATTERN-NEXT: {{^}}  override final var baseProp2: Int{{$}}
}

//===---
//===--- Inheritance list in structs.
//===---

struct StructWithoutInheritance1 {}
// PASS_ONE_LINE-DAG: {{^}}struct StructWithoutInheritance1 {{{$}}

struct StructWithInheritance1 : FooProtocol {}
// PASS_ONE_LINE-DAG: {{^}}struct StructWithInheritance1 : FooProtocol {{{$}}

struct StructWithInheritance2 : FooProtocol, BarProtocol {}
// PASS_ONE_LINE-DAG: {{^}}struct StructWithInheritance2 : FooProtocol, BarProtocol {{{$}}

//===---
//===--- Inheritance list in classes.
//===---

class ClassWithoutInheritance1 {}
// PASS_ONE_LINE-DAG: {{^}}class ClassWithoutInheritance1 {{{$}}

class ClassWithInheritance1 : FooProtocol {}
// PASS_ONE_LINE-DAG: {{^}}class ClassWithInheritance1 : FooProtocol {{{$}}

class ClassWithInheritance2 : FooProtocol, BarProtocol {}
// PASS_ONE_LINE-DAG: {{^}}class ClassWithInheritance2 : FooProtocol, BarProtocol {{{$}}

class ClassWithInheritance3 : FooClass {}
// PASS_ONE_LINE-DAG: {{^}}class ClassWithInheritance3 : FooClass {{{$}}

class ClassWithInheritance4 : FooClass, FooProtocol {}
// PASS_ONE_LINE-DAG: {{^}}class ClassWithInheritance4 : FooClass, FooProtocol {{{$}}

class ClassWithInheritance5 : FooClass, FooProtocol, BarProtocol {}
// PASS_ONE_LINE-DAG: {{^}}class ClassWithInheritance5 : FooClass, FooProtocol, BarProtocol {{{$}}

//===---
//===--- Inheritance list in enums.
//===---

enum EnumWithoutInheritance1 {}
// PASS_ONE_LINE-DAG: {{^}}enum EnumWithoutInheritance1 {{{$}}

enum EnumWithInheritance1 : FooProtocol {}
// PASS_ONE_LINE-DAG: {{^}}enum EnumWithInheritance1 : FooProtocol {{{$}}

enum EnumWithInheritance2 : FooProtocol, BarProtocol {}
// PASS_ONE_LINE-DAG: {{^}}enum EnumWithInheritance2 : FooProtocol, BarProtocol {{{$}}

enum EnumDeclWithUnderlyingType1 : Int { case X }
// PASS_ONE_LINE-DAG: {{^}}enum EnumDeclWithUnderlyingType1 : Int {{{$}}

enum EnumDeclWithUnderlyingType2 : Int, FooProtocol { case X }
// PASS_ONE_LINE-DAG: {{^}}enum EnumDeclWithUnderlyingType2 : Int, FooProtocol {{{$}}

//===---
//===--- Inheritance list in protocols.
//===---

protocol ProtocolWithoutInheritance1 {}
// PASS_ONE_LINE-DAG: {{^}}protocol ProtocolWithoutInheritance1 {{{$}}

protocol ProtocolWithInheritance1 : FooProtocol {}
// PASS_ONE_LINE-DAG: {{^}}protocol ProtocolWithInheritance1 : FooProtocol {{{$}}

protocol ProtocolWithInheritance2 : FooProtocol, BarProtocol {}
// PASS_ONE_LINE-DAG: {{^}}protocol ProtocolWithInheritance2 : FooProtocol, BarProtocol {{{$}}

//===---
//===--- Typealias printing.
//===---

// Normal typealiases.

typealias SimpleTypealias1 = FooProtocol
// PASS_ONE_LINE-DAG: {{^}}typealias SimpleTypealias1 = FooProtocol{{$}}

// Associated types.

protocol AssociatedType1 {
  typealias AssociatedTypeDecl1 = Int
// PASS_ONE_LINE-DAG: {{^}}  typealias AssociatedTypeDecl1 = Int{{$}}

  typealias AssociatedTypeDecl2 : FooProtocol
// PASS_ONE_LINE-DAG: {{^}}  typealias AssociatedTypeDecl2 : FooProtocol{{$}}

  typealias AssociatedTypeDecl3 : FooProtocol, BarProtocol
// PASS_ONE_LINE-DAG: {{^}}  typealias AssociatedTypeDecl3 : FooProtocol, BarProtocol{{$}}
}

//===---
//===--- Variable declaration printing.
//===---

var d0300_topLevelVar1: Int = 42
// PASS_COMMON: {{^}}var d0300_topLevelVar1: Int{{$}}
// PASS_COMMON-NOT: d0300_topLevelVar1

var d0400_topLevelVar2: Int = 42
// PASS_COMMON: {{^}}var d0400_topLevelVar2: Int{{$}}
// PASS_COMMON-NOT: d0400_topLevelVar2

var d0500_topLevelVar2: Int {
  get {
    return 42
  }
}
// PASS_COMMON: {{^}}var d0500_topLevelVar2: Int { get }{{$}}
// PASS_COMMON-NOT: d0500_topLevelVar2

class d0600_InClassVar1 {
// PASS_O600-LABEL: d0600_InClassVar1

  var instanceVar1: Int
// PASS_COMMON: {{^}}  var instanceVar1: Int{{$}}
// PASS_COMMON-NOT: instanceVar1

  var instanceVar2: Int = 42
// PASS_COMMON: {{^}}  var instanceVar2: Int{{$}}
// PASS_COMMON-NOT: instanceVar2

  // FIXME: this is sometimes printed without a type, see PASS_EXPLODE_PATTERN.
  // FIXME: PRINTED_WITHOUT_TYPE
  var instanceVar3 = 42
// PASS_COMMON: {{^}}  var instanceVar3
// PASS_COMMON-NOT: instanceVar3

  var instanceVar4: Int {
    get {
      return 42
    }
  }
// PASS_COMMON: {{^}}  var instanceVar4: Int { get }{{$}}
// PASS_COMMON-NOT: instanceVar4

  // FIXME: uncomment when we have static vars.
  // static var staticVar1: Int

  init() {
    instanceVar1 = 10
  }
}

//===---
//===--- Subscript declaration printing.
//===---

class d0700_InClassSubscript1 {
// PASS_COMMON-LABEL: d0700_InClassSubscript1
  subscript(i: Int) -> Int {
    get {
      return 42
    }
  }
// PASS_COMMON: {{^}}  subscript (i: Int) -> Int { get }{{$}}
// PASS_COMMON-NOT: subscript
}
// PASS_COMMON: {{^}}}{{$}}


//===---
//===--- Constructor declaration printing.
//===---

struct d0800_ExplicitConstructors1 {
// PASS_COMMON-LABEL: d0800_ExplicitConstructors1

  init() {}
// PASS_COMMON: {{^}}  init(){{$}}

  init(a: Int) {}
// PASS_COMMON: {{^}}  init(a: Int){{$}}
}

struct d0900_ExplicitConstructorsSelector1 {
// PASS_COMMON-LABEL: d0900_ExplicitConstructorsSelector1

  init(int a: Int) {}
// PASS_COMMON: {{^}}  init(int a: Int){{$}}

  init(int a: Int, andFloat b: Float) {}
// PASS_COMMON: {{^}}  init(int a: Int, andFloat b: Float){{$}}
}

struct d1000_ExplicitConstructorsSelector2 {
// PASS_COMMON-LABEL: d1000_ExplicitConstructorsSelector2

  init(noArgs _: ()) {}
// PASS_COMMON: {{^}}  init(noArgs _: ()){{$}}

  init(_ a: Int) {}
// PASS_COMMON: {{^}}  init(_ a: Int){{$}}

  init(_ a: Int, withFloat b: Float) {}
// PASS_COMMON: {{^}}  init(_ a: Int, withFloat b: Float){{$}}

  init(int a: Int, _ b: Float) {}
// PASS_COMMON: {{^}}  init(int a: Int, _ b: Float){{$}}
}

//===---
//===--- Destructor declaration printing.
//===---

class d1100_ExplicitDestructor1 {
// PASS_COMMON-LABEL: d1100_ExplicitDestructor1

  deinit {}
// PASS_COMMON: {{^}}  @objc deinit {{$}}
}

//===---
//===--- Enum declaration printing.
//===---

enum d2000_EnumDecl1 {
  case ED1_First
  case ED1_Second
}
// PASS_COMMON: {{^}}enum d2000_EnumDecl1 {{{$}}
// PASS_COMMON-NEXT: {{^}}  case ED1_First{{$}}
// PASS_COMMON-NEXT: {{^}}  case ED1_Second{{$}}
// PASS_COMMON-NEXT: {{^}}  var hashValue: Int { get }{{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}

enum d2100_EnumDecl2 {
  case ED2_A(Int)
  case ED2_B(Float)
  case ED2_C(Int, Float)
  case ED2_D(x: Int, y: Float)
  case ED2_E(x: Int, y: (Float, Double))
  case ED2_F(x: Int, (y: Float, z: Double))
}
// PASS_COMMON: {{^}}enum d2100_EnumDecl2 {{{$}}
// PASS_COMMON-NEXT: {{^}}  case ED2_A(Int){{$}}
// PASS_COMMON-NEXT: {{^}}  case ED2_B(Float){{$}}
// PASS_COMMON-NEXT: {{^}}  case ED2_C(Int, Float){{$}}
// PASS_COMMON-NEXT: {{^}}  case ED2_D(x: Int, y: Float){{$}}
// PASS_COMMON-NEXT: {{^}}  case ED2_E(x: Int, y: (Float, Double)){{$}}
// PASS_COMMON-NEXT: {{^}}  case ED2_F(x: Int, (y: Float, z: Double)){{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}

enum d2200_EnumDecl3 {
  case ED3_A, ED3_B
  case ED3_C(Int), ED3_D
  case ED3_E, ED3_F(Int)
  case ED3_G(Int), ED3_H(Int)
  case ED3_I(Int), ED3_J(Int), ED3_K
}
// PASS_2200: {{^}}enum d2200_EnumDecl3 {{{$}}
// PASS_2200-NEXT: {{^}}  case ED3_A, ED3_B{{$}}
// PASS_2200-NEXT: {{^}}  case ED3_C(Int), ED3_D{{$}}
// PASS_2200-NEXT: {{^}}  case ED3_E, ED3_F(Int){{$}}
// PASS_2200-NEXT: {{^}}  case ED3_G(Int), ED3_H(Int){{$}}
// PASS_2200-NEXT: {{^}}  case ED3_I(Int), ED3_J(Int), ED3_K{{$}}
// PASS_2200-NEXT: {{^}}}{{$}}

// PASS_2200_DESERIALIZED: {{^}}enum d2200_EnumDecl3 {{{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_A{{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_B{{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_C(Int){{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_D{{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_E{{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_F(Int){{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_G(Int){{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_H(Int){{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_I(Int){{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_J(Int){{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}  case ED3_K{{$}}
// PASS_2200_DESERIALIZED-NEXT: {{^}}}{{$}}

enum d2300_EnumDeclWithValues1 : Int {
  case EDV2_First = 10
  case EDV2_Second
}
// PASS_COMMON: {{^}}enum d2300_EnumDeclWithValues1 : Int {{{$}}
// PASS_COMMON-NEXT: {{^}}  case EDV2_First{{$}}
// PASS_COMMON-NEXT: {{^}}  case EDV2_Second{{$}}
// PASS_COMMON-NEXT: {{^}}  var hashValue: Int { get }{{$}}
// PASS_COMMON-NEXT: {{^}}  init?(rawValue: Int){{$}}
// PASS_COMMON-NEXT: {{^}}  var rawValue: Int { get }{{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}

enum d2400_EnumDeclWithValues2 : Double {
  case EDV3_First = 10
  case EDV3_Second
}
// PASS_COMMON: {{^}}enum d2400_EnumDeclWithValues2 : Double {{{$}}
// PASS_COMMON-NEXT: {{^}}  case EDV3_First{{$}}
// PASS_COMMON-NEXT: {{^}}  case EDV3_Second{{$}}
// PASS_COMMON-NEXT: {{^}}  var hashValue: Int { get }{{$}}
// PASS_COMMON-NEXT: {{^}}  init?(rawValue: Double){{$}}
// PASS_COMMON-NEXT: {{^}}  var rawValue: Double { get }{{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}

//===---
//===--- Custom operator printing.
//===---

postfix operator <*> {}

// PASS_2500-LABEL: {{^}}postfix operator <*> {{{$}}
// PASS_2500-NEXT: {{^}}}{{$}}

protocol d2600_ProtocolWithOperator1 {
  postfix func <*>(_: Int)
}
// PASS_2500: {{^}}protocol d2600_ProtocolWithOperator1 {{{$}}
// PASS_2500-NEXT: {{^}}  postfix func <*>(_: Int){{$}}
// PASS_2500-NEXT: {{^}}}{{$}}

struct d2601_TestMutating {}
infix operator %%% { }
func %%%(inout lhs: d2601_TestMutating, rhs: d2601_TestMutating) -> Int {
  return 0
}
// PASS_2500-LABEL: {{^}}infix operator %%% {
// PASS_2500-NOT: associativity
// PASS_2500-NOT: precedence
// PASS_2500-NOT: mutating
// PASS_2500: {{^}}func %%%(inout lhs: d2601_TestMutating, rhs: d2601_TestMutating) -> Int{{$}}

infix operator %%< {
// PASS_2500-LABEL: {{^}}infix operator %%< {{{$}}
  associativity left
// PASS_2500-NEXT: {{^}}  associativity left{{$}}
  precedence 47
// PASS_2500-NEXT: {{^}}  precedence 47{{$}}
// PASS_2500-NOT:         mutating
}

infix operator %%> {
// PASS_2500-LABEL: {{^}}infix operator %%> {{{$}}
  associativity right
// PASS_2500-NEXT: {{^}}  associativity right{{$}}
// PASS_2500-NOT: precedence
// PASS_2500-NOT: mutating
}

infix operator %%<> {
// PASS_2500-LABEL: {{^}}infix operator %%<> {{{$}}
  precedence 47
  mutating
// PASS_2500-NEXT: {{^}}  precedence 47{{$}}
// PASS_2500-NEXT: {{^}}  mutating{{$}}
// PASS_2500-NOT: associativity
}
// PASS_2500: {{^}}}{{$}}

//===---
//===--- Printing of deduced associated types.
//===---

protocol d2700_ProtocolWithAssociatedType1 {
  typealias TA1
  func returnsTA1() -> TA1
}

// PASS_COMMON: {{^}}protocol d2700_ProtocolWithAssociatedType1 {{{$}}
// PASS_COMMON-NEXT: {{^}}  typealias TA1{{$}}
// PASS_COMMON-NEXT: {{^}}  func returnsTA1() -> Self.TA1{{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}

struct d2800_ProtocolWithAssociatedType1Impl : d2700_ProtocolWithAssociatedType1 {
  func returnsTA1() -> Int {
    return 42
  }
}

// FIXME: Should we print the deduced associated type TA1?
// FIXME: rdar://15168378
// PASS_COMMON: {{^}}struct d2800_ProtocolWithAssociatedType1Impl : d2700_ProtocolWithAssociatedType1 {{{$}}
// PASS_COMMON-NEXT: {{^}}  func returnsTA1() -> Int{{$}}
// PASS_COMMON-NEXT: {{^}}  init(){{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}

//===---
//===--- Generic parameter list printing.
//===---

struct GenericParams1<
    StructGenericFoo : FooProtocol,
    StructGenericFooX : FooClass,
    StructGenericBar : protocol<FooProtocol, BarProtocol>,
    StructGenericBaz> {
// PASS_ONE_LINE_TYPE-DAG: {{^}}struct GenericParams1<StructGenericFoo : FooProtocol, StructGenericFooX : FooClass, StructGenericBar : protocol<BarProtocol, FooProtocol>, StructGenericBaz> {{{$}}
// FIXME: in protocol compositions protocols are listed in reverse order.
//
// PASS_ONE_LINE_TYPEREPR-DAG: {{^}}struct GenericParams1<StructGenericFoo : FooProtocol, StructGenericFooX : FooClass, StructGenericBar : protocol<FooProtocol, BarProtocol>, StructGenericBaz> {{{$}}
  init<
      GenericFoo : FooProtocol,
      GenericFooX : FooClass,
      GenericBar : protocol<FooProtocol, BarProtocol>,
      GenericBaz>(a: StructGenericFoo, b: StructGenericBar, c: StructGenericBaz,
                  d: GenericFoo, e: GenericBar, f: GenericBaz)
  {}
// PASS_ONE_LINE_TYPE-DAG: {{^}}  init<GenericFoo : FooProtocol, GenericFooX : FooClass, GenericBar : protocol<BarProtocol, FooProtocol>, GenericBaz>(a: StructGenericFoo, b: StructGenericBar, c: StructGenericBaz, d: GenericFoo, e: GenericBar, f: GenericBaz){{$}}
// FIXME: in protocol compositions protocols are listed in reverse order.
//
// PASS_ONE_LINE_TYPEREPR-DAG: {{^}}  init<GenericFoo : FooProtocol, GenericFooX : FooClass, GenericBar : protocol<FooProtocol, BarProtocol>, GenericBaz>(a: StructGenericFoo, b: StructGenericBar, c: StructGenericBaz, d: GenericFoo, e: GenericBar, f: GenericBaz){{$}}

  func genericParams1<
      GenericFoo : FooProtocol,
      GenericFooX : FooClass,
      GenericBar : protocol<FooProtocol, BarProtocol>,
      GenericBaz>(a: StructGenericFoo, b: StructGenericBar, c: StructGenericBaz,
                  d: GenericFoo, e: GenericBar, f: GenericBaz)
  {}
// PASS_ONE_LINE_TYPE-DAG: {{^}}  func genericParams1<GenericFoo : FooProtocol, GenericFooX : FooClass, GenericBar : protocol<BarProtocol, FooProtocol>, GenericBaz>(a: StructGenericFoo, b: StructGenericBar, c: StructGenericBaz, d: GenericFoo, e: GenericBar, f: GenericBaz){{$}}
// FIXME: in protocol compositions protocols are listed in reverse order.
//
// PASS_ONE_LINE_TYPEREPR-DAG: {{^}}  func genericParams1<GenericFoo : FooProtocol, GenericFooX : FooClass, GenericBar : protocol<FooProtocol, BarProtocol>, GenericBaz>(a: StructGenericFoo, b: StructGenericBar, c: StructGenericBaz, d: GenericFoo, e: GenericBar, f: GenericBaz){{$}}
}

struct GenericParams2<T : FooProtocol where T : BarProtocol> {}
// PASS_ONE_LINE-DAG: {{^}}struct GenericParams2<T : FooProtocol where T : BarProtocol> {{{$}}

struct GenericParams3<T : FooProtocol where T : BarProtocol, T : QuxProtocol> {}
// PASS_ONE_LINE-DAG: {{^}}struct GenericParams3<T : FooProtocol where T : BarProtocol, T : QuxProtocol> {{{$}}

struct GenericParams4<T : QuxProtocol where T.Qux : FooProtocol> {}
// PASS_ONE_LINE-DAG: {{^}}struct GenericParams4<T : QuxProtocol where T.Qux : FooProtocol> {{{$}}

struct GenericParams5<T : QuxProtocol where T.Qux : protocol<FooProtocol, BarProtocol>> {}
// PREFER_TYPE_PRINTING: {{^}}struct GenericParams5<T : QuxProtocol where T.Qux : protocol<BarProtocol, FooProtocol>> {{{$}}
// FIXME: in protocol compositions protocols are listed in reverse order.
//
// PREFER_TYPE_REPR_PRINTING: {{^}}struct GenericParams5<T : QuxProtocol where T.Qux : protocol<FooProtocol, BarProtocol>> {{{$}}

struct GenericParams6<T : QuxProtocol, U : QuxProtocol where T.Qux == U.Qux> {}
// Because of the same type conformance, 'T.Qux' and 'U.Qux' types are
// identical, so they are printed exactly the same way.  Printing a TypeRepr
// allows us to recover the original spelling.
//
// PREFER_TYPE_PRINTING: {{^}}struct GenericParams6<T : QuxProtocol, U : QuxProtocol where T.Qux == T.Qux> {{{$}}
// PREFER_TYPE_REPR_PRINTING: {{^}}struct GenericParams6<T : QuxProtocol, U : QuxProtocol where T.Qux == U.Qux> {{{$}}

struct GenericParams7<T : QuxProtocol, U : QuxProtocol where T.Qux : QuxProtocol, U.Qux : QuxProtocol, T.Qux.Qux == U.Qux.Qux> {}
// PREFER_TYPE_PRINTING: {{^}}struct GenericParams7<T : QuxProtocol, U : QuxProtocol where T.Qux : QuxProtocol, U.Qux : QuxProtocol, T.Qux.Qux == T.Qux.Qux> {{{$}}
// PREFER_TYPE_REPR_PRINTING: {{^}}struct GenericParams7<T : QuxProtocol, U : QuxProtocol where T.Qux : QuxProtocol, U.Qux : QuxProtocol, T.Qux.Qux == U.Qux.Qux> {{{$}}

//===---
//===--- Tupe sugar for library types.
//===---

struct d2900_TypeSugar1 {
// PASS_COMMON-LABEL: {{^}}struct d2900_TypeSugar1 {{{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-LABEL: {{^}}struct d2900_TypeSugar1 {{{$}}

  func f1(x: [Int]) {}
// PASS_COMMON-NEXT: {{^}}  func f1(x: [Int]){{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-NEXT: {{^}}  func f1(x: [Int]){{$}}

  func f2(x: Array<Int>) {}
// PASS_COMMON-NEXT: {{^}}  func f2(x: Array<Int>){{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-NEXT: {{^}}  func f2(x: [Int]){{$}}

  func f3(x: Int?) {}
// PASS_COMMON-NEXT: {{^}}  func f3(x: Int?){{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-NEXT: {{^}}  func f3(x: Int?){{$}}

  func f4(x: Optional<Int>) {}
// PASS_COMMON-NEXT: {{^}}  func f4(x: Optional<Int>){{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-NEXT: {{^}}  func f4(x: Int?){{$}}

  func f5(x: [Int]...) {}
// PASS_COMMON-NEXT: {{^}}  func f5(x: [Int]...){{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-NEXT: {{^}}  func f5(x: [Int]...){{$}}

  func f6(x: Array<Int>...) {}
// PASS_COMMON-NEXT: {{^}}  func f6(x: Array<Int>...){{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-NEXT: {{^}}  func f6(x: [Int]...){{$}}

  func f7(x: [Int : Int]...) {}
// PASS_COMMON-NEXT: {{^}}  func f7(x: [Int : Int]...){{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-NEXT: {{^}}  func f7(x: [Int : Int]...){{$}}

  func f8(x: Dictionary<String, Int>...) {}
// PASS_COMMON-NEXT: {{^}}  func f8(x: Dictionary<String, Int>...){{$}}
// SYNTHESIZE_SUGAR_ON_TYPES-NEXT: {{^}}  func f8(x: [String : Int]...){{$}}
}
// PASS_COMMON-NEXT: {{^}}  init(){{$}}
// PASS_COMMON-NEXT: {{^}}}{{$}}


// Parameter Attributes.


// <rdar://problem/19775868> Swift 1.2b1: Header gen puts @autoclosure in the wrong place
// PASS_PRINT_AST: public func ParamAttrs1(@autoclosure a: () -> ())
public func ParamAttrs1(@autoclosure a : () -> ()) {
  a()
}

// PASS_PRINT_AST: public func ParamAttrs2(@autoclosure(escaping) a: () -> ())
public func ParamAttrs2(@autoclosure(escaping) a : () -> ()) {
  a()
}

// PASS_PRINT_AST: public func ParamAttrs3(@noescape a: () -> ())
public func ParamAttrs3(@noescape a : () -> ()) {
  a()
}

