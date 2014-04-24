// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %swift -verify -parse %t_no_errors.swift

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PA | FileCheck %s -check-prefix=CLASS_PA
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PB | FileCheck %s -check-prefix=CLASS_PB
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PA_PB | FileCheck %s -check-prefix=CLASS_PA_PB

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BA > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BA < %t.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BA_PA > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BA_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BA < %t.txt
// FIXME: WITH_PA

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BA_PB > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BA_PB < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BA < %t.txt
// FIXME: WITH_PB

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BB > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BB < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BB < %t.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BE > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BE < %t.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BE_PA > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BE_PA < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BE < %t.txt
// FIXME: WITH_PA

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_BE_PA_PE > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_BE_PA_PE < %t.txt
// RUN: FileCheck %s -check-prefix=WITH_BE < %t.txt
// FIXME: WITH_PA
// FIXME: WITH_PE

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_PEI_PE > %t.txt
// RUN: FileCheck %s -check-prefix=CLASS_PEI_PE < %t.txt
// FIXME: WITH_PE

struct TagPA {}
protocol ProtocolA {
  init(fromProtocolA: Int)

  func protoAFunc()

  subscript(a: TagPA) -> Int { get }

  var protoAVarRW: Int { get set }
  var protoAVarRO: Int { get }
}

struct TagPB {}
protocol ProtocolB : ProtocolA {
  init(fromProtocolB: Int)

  func protoBFunc()

  subscript(a: TagPB) -> Int { get }

  var protoBVarRW: Int { get set }
  var protoBVarRO: Int { get }
}

struct TagPE {}
protocol ProtocolE {
  init(fromProtocolE: Int)

  func protoEFunc()

  subscript(a: TagPB) -> Int { get }

  var protoEVarRW: Int { get set }
  var protoEVarRO: Int { get }
}

class BaseA {
  init(fromBaseA: Int) {}

  func baseAFunc() {}

  var baseAVarRW: Int { get { return 0 } set {} }
  var baseAVarRO: Int { return 0 }
}
// WITH_BA: Begin completions
// WITH_BA-DAG: Decl[InstanceMethod]/Super: override func baseAFunc() {\n{#|#}\n}{{$}}
// WITH_BA: End completions

class BaseB : BaseA {
  init(fromBaseB: Int) {}

  func baseBFunc() {}

  var baseBVarRW: Int { get { return 0 } set {} }
  var baseBVarRO: Int { return 0 }
}
// WITH_BB: Begin completions
// WITH_BB-DAG: Decl[InstanceMethod]/Super: override func baseAFunc() {\n{#|#}\n}{{$}}
// WITH_BB-DAG: Decl[InstanceMethod]/Super: override func baseBFunc() {\n{#|#}\n}{{$}}
// WITH_BB: End completions

class BaseE : ProtocolE {
  init(fromProtocolE: Int) {}

  func protoEFunc() {}

  subscript(a: TagPB) -> Int { return 0 }

  var protoEVarRW: Int { get { return 0 } set {} }
  var protoEVarRO: Int { return 0 }

  init(fromBaseE: Int) {}

  func baseEFunc() {}

  var baseEVarRW: Int { get { return 0 } set {} }
  var baseEVarRO: Int { return 0 }
}
// WITH_BE: Begin completions
// WITH_BE-DAG: Decl[InstanceMethod]/Super: override func protoEFunc() {\n{#|#}\n}{{$}}
// WITH_BE-DAG: Decl[InstanceMethod]/Super: override func baseEFunc() {\n{#|#}\n}{{$}}
// WITH_BE: End completions

class ProtocolEImpl /* : ProtocolE but does not implement the protocol */ {
  init(fromProtocolE: Int) {}

  func protoEFunc() {}

  subscript(a: TagPB) -> Int { return 0 }

  var protoEVarRW: Int { get { return 0 } set {} }
  var protoEVarRO: Int { return 0 }
}

// NO_ERRORS_UP_TO_HERE

class TestClass_PA : ProtocolA {
  #^CLASS_PA^#
}
// FIXME
// CLASS_PA-NOT: Begin

class TestClass_PB : ProtocolB {
  #^CLASS_PB^#
}
// FIXME
// CLASS_PB-NOT: Begin

class TestClass_PA_PB : ProtocolA, ProtocolB {
  #^CLASS_PA_PB^#
}
// FIXME
// CLASS_PA_PB-NOT: Begin

class TestClass_BA : BaseA {
  #^CLASS_BA^#
}
// CLASS_BA: Begin completions, 1 items

class TestClass_BA_PA : BaseA, ProtocolA {
  #^CLASS_BA_PA^#
}
// CLASS_BA_PA: Begin completions, 1 items

class TestClass_BA_PB : BaseA, ProtocolB {
  #^CLASS_BA_PB^#
}
// CLASS_BA_PB: Begin completions, 1 items

class TestClass_BB : BaseB {
  #^CLASS_BB^#
}
// CLASS_BB: Begin completions, 2 items

class TestClass_BE : BaseE {
  #^CLASS_BE^#
}
// CLASS_BE: Begin completions, 2 items

class TestClass_BE_PA : BaseE, ProtocolA {
  #^CLASS_BE_PA^#
}
// CLASS_BE_PA: Begin completions, 2 items

class TestClass_BE_PA_PE : BaseE, ProtocolA, ProtocolE {
  #^CLASS_BE_PA_PE^#
}
// CLASS_BE_PA_PE: Begin completions, 2 items

class TestClass_PEI_PE : ProtocolEImpl, ProtocolE {
  #^CLASS_PEI_PE^#
}
// CLASS_PEI_PE: Begin completions, 1 items

