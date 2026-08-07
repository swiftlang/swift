// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

// Key path getter and setter thunks are scoped inside the DICompositeType of
// the type whose member they access. LLVM's verifier requires every
// definition DISubprogram nested inside an ODR-uniqued DICompositeType to
// reference a matching declaration when ODR type uniquing is enabled, which
// is the case during LTO. Check that IRGen emits such declarations for key
// path getter and setter thunks.

// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o %t.ll
// RUN: %llvm-link -S %t.ll -o - 2>%t.err | %FileCheck --check-prefix=ODR %s
// RUN: %FileCheck --allow-empty --check-prefix=ODR-ERR %s < %t.err

// llvm-link enables ODR type uniquing like LTO does. If the emitted debug
// info violated the ODR verifier rules, llvm-link would strip it and warn.
// ODR: distinct !DISubprogram({{.*}}declaration:
// ODR-ERR-NOT: ignoring invalid debug info

public struct S {
  public var stored: Int = 0
  public var computed: Int {
    get { return stored }
    set { stored = newValue }
  }
  public subscript(i: Int) -> Int {
    get { return stored + i }
    set { stored = newValue + i }
  }
}

public func f() -> [AnyKeyPath] {
  return [\S.computed, \S.[0]]
}

// CHECK-DAG: ![[S_DBG:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "S", {{.*}}identifier: "$s4main1SVD"

// Getter and setter thunks for S.computed.
// CHECK-DAG: distinct !DISubprogram(name: "computed.get", linkageName: "$s4main1SV8computedSivpACTK", scope: ![[S_DBG]],{{.*}} spFlags: DISPFlagDefinition,{{.*}} declaration: ![[PROP_GETTER_DECL:[0-9]+]]
// CHECK-DAG: ![[PROP_GETTER_DECL]] = !DISubprogram(name: "computed.get", linkageName: "$s4main1SV8computedSivpACTK", scope: ![[S_DBG]]
// CHECK-DAG: distinct !DISubprogram(name: "computed.set", linkageName: "$s4main1SV8computedSivpACTk", scope: ![[S_DBG]],{{.*}} spFlags: DISPFlagDefinition,{{.*}} declaration: ![[PROP_SETTER_DECL:[0-9]+]]
// CHECK-DAG: ![[PROP_SETTER_DECL]] = !DISubprogram(name: "computed.set", linkageName: "$s4main1SV8computedSivpACTk", scope: ![[S_DBG]]

// Getter and setter thunks for S.[_: Int].
// CHECK-DAG: distinct !DISubprogram(name: "subscript.get", linkageName: "$s4main1SVyS2icipACTK", scope: ![[S_DBG]],{{.*}} spFlags: DISPFlagDefinition,{{.*}} declaration: ![[SUB_GETTER_DECL:[0-9]+]]
// CHECK-DAG: ![[SUB_GETTER_DECL]] = !DISubprogram(name: "subscript.get", linkageName: "$s4main1SVyS2icipACTK", scope: ![[S_DBG]]
// CHECK-DAG: distinct !DISubprogram(name: "subscript.set", linkageName: "$s4main1SVyS2icipACTk", scope: ![[S_DBG]],{{.*}} spFlags: DISPFlagDefinition,{{.*}} declaration: ![[SUB_SETTER_DECL:[0-9]+]]
// CHECK-DAG: ![[SUB_SETTER_DECL]] = !DISubprogram(name: "subscript.set", linkageName: "$s4main1SVyS2icipACTk", scope: ![[S_DBG]]

// The key path index equality and hash thunks are scoped inside the module,
// not inside a composite type, so they must not get a declaration.
// CHECK-DAG: distinct !DISubprogram(linkageName: "$sSiTH", scope: ![[MODULE:[0-9]+]], file: !{{[0-9]+}}, type: !{{[0-9]+}}, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !{{[0-9]+}})
// CHECK-DAG: distinct !DISubprogram(linkageName: "$sSiTh", scope: ![[MODULE]], file: !{{[0-9]+}}, type: !{{[0-9]+}}, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !{{[0-9]+}})
// CHECK-DAG: ![[MODULE]] = !DIModule(
