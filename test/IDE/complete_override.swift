// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -parse -verify -disable-objc-attr-requires-foundation-module %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PA -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PA_EXT_1 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PA_EXT_2 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PB -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_PB < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PB < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PA_PB -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_PA_PB < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PB < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BA -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BA_PA -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BA_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BA_PA_EXT1 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BA_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BA_PA_EXT2 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BA_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BA_PB -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BA_PB < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PB < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BB -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BB < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BB < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BE -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BE < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BE_PA -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BE_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BE_PA_PE -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BE_PA_PE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BE_PA_PE_EXT1 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BE_PA_PE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BE_PA_PE_EXT2 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BE_PA_PE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PA < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PEI_PE -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_PEI_PE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_PEI < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_NOMINAL -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=NESTED_NOMINAL < %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OMIT_KEYWORD1 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=OMIT_KEYWORD1< %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OMIT_KEYWORD2 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=OMIT_KEYWORD2< %t.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OMIT_KEYWORD3 -code-completion-keywords=false > %t.txt
// RUN: FileCheck %s -check-prefix=OMIT_KEYWORD3< %t.txt

@objc
class TagPA {}
@objc
protocol ProtocolA {
  init(fromProtocolA: Int)

  func protoAFunc()
  optional func protoAFuncOptional()

  @noreturn
  func protoAFuncWithAttr()

  subscript(a: TagPA) -> Int { get }

  var protoAVarRW: Int { get set }
  var protoAVarRO: Int { get }
}
// WITH_PA: Begin completions
// WITH_PA-DAG: Decl[Constructor]/Super:    init(fromProtocolA: Int) {|}{{; name=.+$}}
// WITH_PA-DAG: Decl[InstanceMethod]/Super: func protoAFunc() {|}{{; name=.+$}}
// WITH_PA-DAG: Decl[InstanceMethod]/Super: func protoAFuncOptional() {|}{{; name=.+$}}
// WITH_PA-DAG: Decl[InstanceMethod]/Super: @noreturn func protoAFuncWithAttr() {|}{{; name=.+$}}
// WITH_PA: End completions

struct TagPB {}
protocol ProtocolB : ProtocolA {
  init(fromProtocolB: Int)

  func protoBFunc()

  subscript(a: TagPB) -> Int { get }

  var protoBVarRW: Int { get set }
  var protoBVarRO: Int { get }
}
// WITH_PB: Begin completions
// WITH_PB-DAG: Decl[Constructor]/Super:    init(fromProtocolA: Int) {|}{{; name=.+$}}
// WITH_PB-DAG: Decl[InstanceMethod]/Super: func protoAFunc() {|}{{; name=.+$}}
// WITH_PB-DAG: Decl[InstanceMethod]/Super: @noreturn func protoAFuncWithAttr() {|}{{; name=.+$}}
// WITH_PB-DAG: Decl[Constructor]/Super:    init(fromProtocolB: Int) {|}{{; name=.+$}}
// WITH_PB-DAG: Decl[InstanceMethod]/Super: func protoBFunc() {|}{{; name=.+$}}
// WITH_PB: End completions

struct TagPE {}
protocol ProtocolE {
  init(fromProtocolE: Int)

  func protoEFunc()

  subscript(a: TagPE) -> Int { get }

  var protoEVarRW: Int { get set }
  var protoEVarRO: Int { get }
}
// WITH_PE: Begin completions
// WITH_PE-DAG: Decl[Constructor]/Super:    init(fromProtocolE: Int) {|}{{; name=.+$}}
// WITH_PE-DAG: Decl[InstanceMethod]/Super: func protoEFunc() {|}{{; name=.+$}}
// WITH_PE: End completions

@noreturn @asmname("exit")
func exit()

class BaseA {
  init(fromBaseA: Int) {}
  init(fromBaseAWithParamName foo: Int, withOther bar: Double) {}
  convenience init(convenienceFromBaseA: Double) {
    self.init(fromBaseA: 0)
  }

  func baseAFunc(foo x: Int) {}
  func baseAFunc2(foo x: Int) {}

  @noreturn
  func baseAFuncWithAttr() {
    exit()
  }

  var baseAVarRW: Int { get { return 0 } set {} }
  var baseAVarRO: Int { return 0 }
}
// WITH_BA: Begin completions
// WITH_BA-DAG: Decl[Constructor]/Super:    init(fromBaseA: Int) {|}{{; name=.+$}}
// WITH_BA-DAG: Decl[Constructor]/Super:    init(fromBaseAWithParamName foo: Int, withOther bar: Double) {|}{{; name=.+$}}
// WITH_BA-DAG: Decl[InstanceMethod]/Super: override func baseAFunc(foo x: Int) {|}{{; name=.+$}}
// WITH_BA-DAG: Decl[InstanceMethod]/Super: override func baseAFunc2(foo x: Int) {|}{{; name=.+$}}
// WITH_BA-DAG: Decl[InstanceMethod]/Super: override @noreturn func baseAFuncWithAttr() {|}{{; name=.+$}}
// WITH_BA: End completions

class BaseB : BaseA {
  override func baseAFunc2(foo x: Int) {}

  init(fromBaseB: Int) {}
  convenience init(convenienceFromBaseB: Double) {
    self.init(fromBaseB: 0)
  }

  func baseBFunc() {}

  var baseBVarRW: Int { get { return 0 } set {} }
  var baseBVarRO: Int { return 0 }
}
// WITH_BB: Begin completions
// WITH_BB-DAG: Decl[InstanceMethod]/Super: override func baseAFunc(foo x: Int) {|}{{; name=.+$}}
// WITH_BB-DAG: Decl[InstanceMethod]/Super: override func baseAFunc2(foo x: Int) {|}{{; name=.+$}}
// WITH_BB-DAG: Decl[InstanceMethod]/Super: override @noreturn func baseAFuncWithAttr() {|}{{; name=.+$}}
// WITH_BB-DAG: Decl[Constructor]/Super:    init(fromBaseB: Int) {|}{{; name=.+$}}
// WITH_BB-DAG: Decl[InstanceMethod]/Super: override func baseBFunc() {|}{{; name=.+$}}
// WITH_BB: End completions

class BaseE : ProtocolE {
  required init(fromProtocolE: Int) {}

  func protoEFunc() {}

  subscript(a: TagPE) -> Int { return 0 }

  var protoEVarRW: Int { get { return 0 } set {} }
  var protoEVarRO: Int { return 0 }

  init(fromBaseE: Int) {}

  func baseEFunc() {}

  var baseEVarRW: Int { get { return 0 } set {} }
  var baseEVarRO: Int { return 0 }
}
// WITH_BE: Begin completions
// WITH_BE-DAG: Decl[Constructor]/Super:    init(fromProtocolE: Int) {|}{{; name=.+$}}
// WITH_BE-DAG: Decl[InstanceMethod]/Super: override func protoEFunc() {|}{{; name=.+$}}
// WITH_BE-DAG: Decl[Constructor]/Super:    init(fromBaseE: Int) {|}{{; name=.+$}}
// WITH_BE-DAG: Decl[InstanceMethod]/Super: override func baseEFunc() {|}{{; name=.+$}}
// WITH_BE: End completions

class ProtocolEImpl /* : ProtocolE but does not implement the protocol */ {
  init(fromProtocolE: Int) {}

  func protoEFunc() {}

  subscript(a: TagPE) -> Int { return 0 }

  var protoEVarRW: Int { get { return 0 } set {} }
  var protoEVarRO: Int { return 0 }
}
// WITH_PEI: Begin completions
// WITH_PEI-DAG: Decl[Constructor]/Super:    init(fromProtocolE: Int) {|}{{; name=.+$}}
// WITH_PEI-DAG: Decl[InstanceMethod]/Super: override func protoEFunc() {|}{{; name=.+$}}
// WITH_PEI: End completions

// NO_ERRORS_UP_TO_HERE

class TestClass_PA : ProtocolA {
  func ERROR() {}

  #^CLASS_PA^#
}
// CLASS_PA: Begin completions, 4 items

class TestClass_PA_Ext {
  func ERROR1() {}
  #^CLASS_PA_EXT_1^#
}
extension TestClass_PA_Ext : ProtocolA {
  func ERROR2() {}
  #^CLASS_PA_EXT_2^#
}

class TestClass_PB : ProtocolB {
  #^CLASS_PB^#
}
// CLASS_PB: Begin completions, 6 items

class TestClass_PA_PB : ProtocolA, ProtocolB {
  #^CLASS_PA_PB^#
}
// CLASS_PA_PB: Begin completions, 6 items

class TestClass_BA : BaseA {
  #^CLASS_BA^#
}
// CLASS_BA: Begin completions, 5 items

class TestClass_BA_PA : BaseA, ProtocolA {
  #^CLASS_BA_PA^#
}
// CLASS_BA_PA: Begin completions, 9 items

class TestClass_BA_PA_Ext : BaseA {
  #^CLASS_BA_PA_EXT1^#
}

extension TestClass_BA_PA_Ext : ProtocolA {
  #^CLASS_BA_PA_EXT2^#
}

class TestClass_BA_PB : BaseA, ProtocolB {
  #^CLASS_BA_PB^#
}
// CLASS_BA_PB: Begin completions, 11 items

class TestClass_BB : BaseB {
  #^CLASS_BB^#
}
// CLASS_BB: Begin completions, 5 items

class TestClass_BE : BaseE {
  #^CLASS_BE^#
}
// CLASS_BE: Begin completions, 4 items

class TestClass_BE_PA : BaseE, ProtocolA {
  #^CLASS_BE_PA^#
}
// CLASS_BE_PA: Begin completions, 8 items

class TestClass_BE_PA_PE : BaseE, ProtocolA, ProtocolE {
  #^CLASS_BE_PA_PE^#
}
// CLASS_BE_PA_PE: Begin completions, 8 items

class TestClass_BE_PA_PE_Ext : BaseE {
  #^CLASS_BE_PA_PE_EXT1^#
}
extension TestClass_BE_PA_PE_Ext : ProtocolA, ProtocolE {
  #^CLASS_BE_PA_PE_EXT2^#
}

class TestClass_PEI_PE : ProtocolEImpl, ProtocolE {
  #^CLASS_PEI_PE^#
}
// CLASS_PEI_PE: Begin completions, 2 items

class OuterNominal : ProtocolA {
  class Inner {
    #^NESTED_NOMINAL^#
  }
}
// NESTED_NOMINAL: found code completion token
// NESTED_NOMINAL-NOT: Begin completions

class OmitKW1 : ProtocolA {
  override#^OMIT_KEYWORD1^#
}

//OMIT_KEYWORD1:         Begin completions
//OMIT_KEYWORD1-DAG:     Decl[Constructor]/Super:            init(fromProtocolA: Int) {|}; name=init(fromProtocolA: Int){{$}}
//OMIT_KEYWORD1-DAG:     Decl[InstanceMethod]/Super:         func protoAFunc() {|}; name=protoAFunc(){{$}}
//OMIT_KEYWORD1-DAG:     Decl[InstanceMethod]/Super:         func protoAFuncOptional() {|}; name=protoAFuncOptional(){{$}}
//OMIT_KEYWORD1-DAG:     Decl[InstanceMethod]/Super:         @noreturn func protoAFuncWithAttr() {|}; name=protoAFuncWithAttr(){{$}}

class OmitKW2 : ProtocolA {
  override func#^OMIT_KEYWORD2^#
}

//OMIT_KEYWORD2:         Begin completions
//OMIT_KEYWORD2-DAG:     Decl[Constructor]/Super:            init(fromProtocolA: Int) {|}; name=init(fromProtocolA: Int){{$}}
//OMIT_KEYWORD2-DAG:     Decl[InstanceMethod]/Super:         protoAFunc() {|}; name=protoAFunc(){{$}}
//OMIT_KEYWORD2-DAG:     Decl[InstanceMethod]/Super:         protoAFuncOptional() {|}; name=protoAFuncOptional(){{$}}
//OMIT_KEYWORD2-DAG:     Decl[InstanceMethod]/Super:         protoAFuncWithAttr() {|}; name=protoAFuncWithAttr(){{$}}

class OmitKW3 : ProtocolA {
  func#^OMIT_KEYWORD3^#
}

//OMIT_KEYWORD3:         Begin completions
//OMIT_KEYWORD3-DAG:     Decl[Constructor]/Super:            init(fromProtocolA: Int) {|}; name=init(fromProtocolA: Int){{$}}
//OMIT_KEYWORD3-DAG:     Decl[InstanceMethod]/Super:         protoAFunc() {|}; name=protoAFunc(){{$}}
//OMIT_KEYWORD3-DAG:     Decl[InstanceMethod]/Super:         protoAFuncOptional() {|}; name=protoAFuncOptional(){{$}}
//OMIT_KEYWORD3-DAG:     Decl[InstanceMethod]/Super:         protoAFuncWithAttr() {|}; name=protoAFuncWithAttr(){{$}}