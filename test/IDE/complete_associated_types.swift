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
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .DefaultedTypeCommonA[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .DefaultedTypeCommonD[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .DeducedTypeCommonA[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .DeducedTypeCommonD[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .BarDefaultedTypeA[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .BarDeducedTypeD[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .DefaultedTypeCommonC[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .DeducedTypeCommonC[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .BarBaseDefaultedType[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .BarBaseDeducedTypeC[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .BarBaseDeducedTypeD[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .DefaultedTypeCommonB[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooBaseDefaultedTypeB[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .DeducedTypeCommonB[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooDefaultedType[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooDeducedTypeB[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooDeducedTypeC[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooDeducedTypeD[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooBaseDefaultedTypeA[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooBaseDeducedTypeA[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooBaseDeducedTypeB[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooBaseDeducedTypeC[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal:       .FooBaseDeducedTypeD[#Int#]{{; name=.+$}}
// STRUCT_TYPES-DAG: Decl[TypeAlias]/CurrNominal: .FooBaseDefaultedTypeC[#Double#]{{; name=.+$}}
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
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DefaultedTypeCommonA[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DefaultedTypeCommonD[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DeducedTypeCommonA[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DeducedTypeCommonD[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarDefaultedTypeA[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarDeducedTypeD[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DefaultedTypeCommonC[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DeducedTypeCommonC[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarBaseDefaultedType[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarBaseDeducedTypeC[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: BarBaseDeducedTypeD[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DefaultedTypeCommonB[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDefaultedTypeB[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: DeducedTypeCommonB[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooDefaultedType[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooDeducedTypeB[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooDeducedTypeC[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooDeducedTypeD[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDefaultedTypeA[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDefaultedTypeC[#Double#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDeducedTypeA[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDeducedTypeB[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDeducedTypeC[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL-DAG: Decl[TypeAlias]/Super: FooBaseDeducedTypeD[#Int#]{{; name=.+$}}
// ASSOCIATED_TYPES_UNQUAL: End completions

struct StructWithBrokenConformance : FooProtocolWithAssociatedTypes {
  // Does not conform.
}
func testBrokenConformances1() {
  StructWithBrokenConformance.#^BROKEN_CONFORMANCE_1^#
}
// BROKEN_CONFORMANCE_1: Begin completions, 52 items
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DefaultedTypeCommonA{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DefaultedTypeCommonB{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDefaultedTypeB{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DeducedTypeCommonA{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DeducedTypeCommonB{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonA({#self: Self#})[#() -> Self.DeducedTypeCommonA#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonB({#self: Self#})[#() -> Self.DeducedTypeCommonB#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseB({#self: Self#})[#() -> Int#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooDefaultedType{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooDeducedTypeB{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooDeducedTypeC{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooDeducedTypeD{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooB({#self: Self#})[#() -> Self.FooDeducedTypeB#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooC({#self: Self#})[#() -> Self.FooDeducedTypeC#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooD({#self: Self#})[#() -> Self.FooDeducedTypeD#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DefaultedTypeCommonC{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DefaultedTypeCommonD{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDefaultedTypeA{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDefaultedTypeC{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DeducedTypeCommonC{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              DeducedTypeCommonD{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonA({#self: Self#})[#() -> Self.DeducedTypeCommonA#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonB({#self: Self#})[#() -> Self.DeducedTypeCommonB#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonC({#self: Self#})[#() -> Self.DeducedTypeCommonC#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceCommonD({#self: Self#})[#() -> Self.DeducedTypeCommonD#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDeducedTypeA{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDeducedTypeB{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDeducedTypeC{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[TypeAlias]/Super:              FooBaseDeducedTypeD{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseA({#self: Self#})[#() -> Self.FooBaseDeducedTypeA#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseB({#self: Self#})[#() -> Self.FooBaseDeducedTypeB#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseC({#self: Self#})[#() -> Self.FooBaseDeducedTypeC#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1-DAG: Decl[InstanceMethod]/Super:         deduceFooBaseD({#self: Self#})[#() -> Self.FooBaseDeducedTypeD#]{{; name=.+$}}
// BROKEN_CONFORMANCE_1: End completions
