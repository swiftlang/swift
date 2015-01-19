// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -parse -verify %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_AS_TYPE > %t.types.txt
// RUN: FileCheck %s -check-prefix=STRUCT_TYPE_COUNT < %t.types.txt
// RUN: FileCheck %s -check-prefix=STRUCT_TYPES < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_AS_EXPR > %t.types.txt
// RUN: FileCheck %s -check-prefix=STRUCT_TYPES < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INSTANCE > %t.types.txt
// RUN: FileCheck %s -check-prefix=STRUCT_INSTANCE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSOCIATED_TYPES_UNQUAL_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=ASSOCIATED_TYPES_UNQUAL < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ASSOCIATED_TYPES_UNQUAL_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=ASSOCIATED_TYPES_UNQUAL < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BROKEN_CONFORMANCE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=BROKEN_CONFORMANCE_1 < %t.types.txt

// FIXME: extensions that introduce conformances?

protocol FooBaseProtocolWithAssociatedTypes {
  typealias DefaultedTypeCommonA = Int
  typealias DefaultedTypeCommonB = Int
  typealias DefaultedTypeCommonC = Int
  typealias DefaultedTypeCommonD = Int

  typealias FooBaseDefaultedTypeA = Int
  typealias FooBaseDefaultedTypeB = Int
  typealias FooBaseDefaultedTypeC = Int

  typealias DeducedTypeCommonA
  typealias DeducedTypeCommonB
  typealias DeducedTypeCommonC
  typealias DeducedTypeCommonD
  func deduceCommonA() -> DeducedTypeCommonA
  func deduceCommonB() -> DeducedTypeCommonB
  func deduceCommonC() -> DeducedTypeCommonC
  func deduceCommonD() -> DeducedTypeCommonD

  typealias FooBaseDeducedTypeA
  typealias FooBaseDeducedTypeB
  typealias FooBaseDeducedTypeC
  typealias FooBaseDeducedTypeD
  func deduceFooBaseA() -> FooBaseDeducedTypeA
  func deduceFooBaseB() -> FooBaseDeducedTypeB
  func deduceFooBaseC() -> FooBaseDeducedTypeC
  func deduceFooBaseD() -> FooBaseDeducedTypeD
}
protocol FooProtocolWithAssociatedTypes : FooBaseProtocolWithAssociatedTypes {
  // From FooBase.
  typealias DefaultedTypeCommonA = Int
  typealias DefaultedTypeCommonB = Int

  typealias FooBaseDefaultedTypeB = Double

  typealias DeducedTypeCommonA
  typealias DeducedTypeCommonB
  func deduceCommonA() -> DeducedTypeCommonA
  func deduceCommonB() -> DeducedTypeCommonB

  func deduceFooBaseB() -> Int

  // New decls.
  typealias FooDefaultedType = Int

  typealias FooDeducedTypeB
  typealias FooDeducedTypeC
  typealias FooDeducedTypeD
  func deduceFooB() -> FooDeducedTypeB
  func deduceFooC() -> FooDeducedTypeC
  func deduceFooD() -> FooDeducedTypeD
}
protocol BarBaseProtocolWithAssociatedTypes {
  // From FooBase.
  typealias DefaultedTypeCommonA = Int
  typealias DefaultedTypeCommonC = Int

  typealias DeducedTypeCommonA
  typealias DeducedTypeCommonC
  func deduceCommonA() -> DeducedTypeCommonA
  func deduceCommonC() -> DeducedTypeCommonC

  func deduceFooBaseC() -> Int

  // From Foo.
  func deduceFooC() -> Int

  // New decls.
  typealias BarBaseDefaultedType = Int

  typealias BarBaseDeducedTypeC
  typealias BarBaseDeducedTypeD
  func deduceBarBaseC() -> BarBaseDeducedTypeC
  func deduceBarBaseD() -> BarBaseDeducedTypeD
}
protocol BarProtocolWithAssociatedTypes : BarBaseProtocolWithAssociatedTypes {
  // From FooBase.
  typealias DefaultedTypeCommonA = Int
  typealias DefaultedTypeCommonD = Int

  typealias DeducedTypeCommonA
  typealias DeducedTypeCommonD
  func deduceCommonA() -> DeducedTypeCommonA
  func deduceCommonD() -> DeducedTypeCommonD

  func deduceFooBaseD() -> Int

  // From Foo.
  func deduceFooD() -> Int

  // From BarBase.
  func deduceBarBaseD() -> Int

  // New decls.
  typealias BarDefaultedTypeA = Int

  typealias BarDeducedTypeD
  func deduceBarD() -> BarDeducedTypeD
}

struct StructWithAssociatedTypes : FooProtocolWithAssociatedTypes, BarProtocolWithAssociatedTypes {
  typealias FooBaseDefaultedTypeC = Double

  func deduceCommonA() -> Int { return 0 }
  func deduceCommonB() -> Int { return 0 }
  func deduceCommonC() -> Int { return 0 }
  func deduceCommonD() -> Int { return 0 }

  func deduceFooBaseA() -> Int { return 0 }
  func deduceFooBaseB() -> Int { return 0 }
  func deduceFooBaseC() -> Int { return 0 }
  func deduceFooBaseD() -> Int { return 0 }
  func deduceFooB() -> Int { return 0 }
  func deduceFooC() -> Int { return 0 }
  func deduceFooD() -> Int { return 0 }

  func deduceBarBaseC() -> Int { return 0 }
  func deduceBarBaseD() -> Int { return 0 }

  func deduceBarD() -> Int { return 0 }
}

class ClassWithAssociatedTypes : FooProtocolWithAssociatedTypes, BarProtocolWithAssociatedTypes {
  typealias FooBaseDefaultedTypeC = Double

  func deduceCommonA() -> Int { return 0 }
  func deduceCommonB() -> Int { return 0 }
  func deduceCommonC() -> Int { return 0 }
  func deduceCommonD() -> Int { return 0 }

  func deduceFooBaseA() -> Int { return 0 }
  func deduceFooBaseB() -> Int { return 0 }
  func deduceFooBaseC() -> Int { return 0 }
  func deduceFooBaseD() -> Int { return 0 }
  func deduceFooB() -> Int { return 0 }
  func deduceFooC() -> Int { return 0 }
  func deduceFooD() -> Int { return 0 }

  func deduceBarBaseC() -> Int { return 0 }
  func deduceBarBaseD() -> Int { return 0 }

  func deduceBarD() -> Int { return 0 }
}

// NO_ERRORS_UP_TO_HERE

func testStruct1() {
  var x: StructWithAssociatedTypes#^STRUCT_AS_TYPE^#
}
func testStruct2() {
  StructWithAssociatedTypes#^STRUCT_AS_EXPR^#
}
func testStruct3(a: StructWithAssociatedTypes) {
  a.#^STRUCT_INSTANCE^#
}
// STRUCT_TYPE_COUNT: Begin completions, 26 items

// STRUCT_INSTANCE: Begin completions, 14 items
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceCommonA()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceCommonB()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceCommonC()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceCommonD()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceFooBaseA()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceFooBaseB()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceFooBaseC()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceFooBaseD()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceFooB()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceFooC()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceFooD()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceBarBaseC()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceBarBaseD()[#Int#]
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal:   deduceBarD()[#Int#]
// STRUCT_INSTANCE: End completions

// STRUCT_TYPES: Begin completions
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .DefaultedTypeCommonA[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .DefaultedTypeCommonD[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .DeducedTypeCommonA[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .DeducedTypeCommonD[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .BarDefaultedTypeA[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .BarDeducedTypeD[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .DefaultedTypeCommonC[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .DeducedTypeCommonC[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .BarBaseDefaultedType[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .BarBaseDeducedTypeC[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .BarBaseDeducedTypeD[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .DefaultedTypeCommonB[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooBaseDefaultedTypeB[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .DeducedTypeCommonB[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooDefaultedType[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooDeducedTypeB[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooDeducedTypeC[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooDeducedTypeD[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooBaseDefaultedTypeA[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooBaseDeducedTypeA[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooBaseDeducedTypeB[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooBaseDeducedTypeC[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/Super:       .FooBaseDeducedTypeD[#Int#]{{$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal: .FooBaseDefaultedTypeC[#Double#]{{$}}
// STRUCT_TYPES: End completions

class DerivedFromClassWithAssociatedTypes : ClassWithAssociatedTypes {
  func test() {
    #^ASSOCIATED_TYPES_UNQUAL_1^#
  }
}
class MoreDerivedFromClassWithAssociatedTypes : DerivedFromClassWithAssociatedTypes {
  func test() {
    #^ASSOCIATED_TYPES_UNQUAL_2^#
  }
}
// ASSOCIATED_TYPES_UNQUAL: Begin completions
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DefaultedTypeCommonA[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DefaultedTypeCommonD[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DeducedTypeCommonA[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DeducedTypeCommonD[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarDefaultedTypeA[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarDeducedTypeD[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DefaultedTypeCommonC[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DeducedTypeCommonC[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarBaseDefaultedType[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarBaseDeducedTypeC[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarBaseDeducedTypeD[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DefaultedTypeCommonB[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDefaultedTypeB[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DeducedTypeCommonB[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooDefaultedType[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooDeducedTypeB[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooDeducedTypeC[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooDeducedTypeD[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDefaultedTypeA[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDefaultedTypeC[#Double#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDeducedTypeA[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDeducedTypeB[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDeducedTypeC[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDeducedTypeD[#Int#]{{$}}
// ASSOCIATED_TYPES_UNQUAL: End completions

struct StructWithBrokenConformance : FooProtocolWithAssociatedTypes {
  // Does not conform.
}
func testBrokenConformances1() {
  StructWithBrokenConformance.#^BROKEN_CONFORMANCE_1^#
}
// BROKEN_CONFORMANCE_1: Begin completions, 33 items
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DefaultedTypeCommonA{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DefaultedTypeCommonB{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDefaultedTypeB{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DeducedTypeCommonA{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DeducedTypeCommonB{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonA({#self: Self#})[#() -> DeducedTypeCommonA#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonB({#self: Self#})[#() -> DeducedTypeCommonB#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseB({#self: Self#})[#() -> Int#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooDefaultedType{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooDeducedTypeB{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooDeducedTypeC{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooDeducedTypeD{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooB({#self: Self#})[#() -> FooDeducedTypeB#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooC({#self: Self#})[#() -> FooDeducedTypeC#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooD({#self: Self#})[#() -> FooDeducedTypeD#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DefaultedTypeCommonC{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DefaultedTypeCommonD{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDefaultedTypeA{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDefaultedTypeC{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DeducedTypeCommonC{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DeducedTypeCommonD{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonA({#self: Self#})[#() -> DeducedTypeCommonA#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonB({#self: Self#})[#() -> DeducedTypeCommonB#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonC({#self: Self#})[#() -> DeducedTypeCommonC#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonD({#self: Self#})[#() -> DeducedTypeCommonD#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDeducedTypeA{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDeducedTypeB{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDeducedTypeC{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDeducedTypeD{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseA({#self: Self#})[#() -> FooBaseDeducedTypeA#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseB({#self: Self#})[#() -> FooBaseDeducedTypeB#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseC({#self: Self#})[#() -> FooBaseDeducedTypeC#]{{$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseD({#self: Self#})[#() -> FooBaseDeducedTypeD#]{{$}}
// BROKEN_CONFORMANCE_1: End completions

