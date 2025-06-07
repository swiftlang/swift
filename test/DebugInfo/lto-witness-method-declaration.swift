// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// Verify that we added a declaration for a witness method.

// CHECK: define{{.*}}@"$s4main14PutCharPrinterCAA09CharacterD0A2aDPxycfCTW"{{.*}} !dbg ![[INIT_DEF_DBG:[0-9]+]]
// CHECK: ![[INIT_DEF_DBG]] = distinct !DISubprogram(name: "init", linkageName: "$s4main14PutCharPrinterCAA09CharacterD0A2aDPxycfCTW"
// CHECK-SAME: DISPFlagDefinition{{.*}} declaration: ![[FUNC_DEF_DBG:[0-9]+]]
// CHECK: ![[FUNC_DEF_DBG]] = !DISubprogram(name: "init", linkageName: "$s4main14PutCharPrinterCAA09CharacterD0A2aDPxycfCTW"

protocol CharacterPrinter {
    init()
}

class PutCharPrinter: CharacterPrinter {
    public required init() {}
}
