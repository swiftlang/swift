// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=STRUCT_1 < %t.types.txt

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
func testStruct() {
  var x: StructWithAssociatedTypes#^STRUCT_1^#
}
// STRUCT_1: Begin completions, 16 items
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .DefaultedTypeCommonA[#DefaultedTypeCommonA.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .DefaultedTypeCommonB[#DefaultedTypeCommonB.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .FooBaseDefaultedTypeB[#FooBaseDefaultedTypeB.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .DeducedTypeCommonA[#DeducedTypeCommonA.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .DeducedTypeCommonB[#DeducedTypeCommonB.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .FooDefaultedType[#FooDefaultedType.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .FooDeducedTypeB[#FooDeducedTypeB.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .FooDeducedTypeC[#FooDeducedTypeC.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .FooDeducedTypeD[#FooDeducedTypeD.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .DefaultedTypeCommonA[#DefaultedTypeCommonA.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .DefaultedTypeCommonD[#DefaultedTypeCommonD.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .DeducedTypeCommonA[#DeducedTypeCommonA.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .DeducedTypeCommonD[#DeducedTypeCommonD.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .BarDefaultedTypeA[#BarDefaultedTypeA.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/Super:       .BarDeducedTypeD[#BarDeducedTypeD.metatype#]{{$}}
// STRUCT_1-DAG: Decl[TypeAlias]/CurrNominal: .FooBaseDefaultedTypeC[#Double.metatype#]{{$}}
// STRUCT_1: End completions

