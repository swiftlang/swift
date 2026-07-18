// RUN: %target-swift-synthesize-interface -module-name TopLevelModule -I %S/Inputs -o - | %FileCheck %s
// RUN: %target-swift-synthesize-interface -module-name TopLevelModule -include-submodules -I %S/Inputs -o - | %FileCheck %s --check-prefix=IMPLICIT
// RUN: %target-swift-synthesize-interface -module-name TopLevelModule.ExplicitSubmodule -I %S/Inputs -o - | %FileCheck %s --check-prefix=EXPLICIT

// CHECK:     import TopLevelModule.ExplicitSubmodule
// CHECK-DAG: import TopLevelModule.ImplicitSubmodule
// CHECK-DAG: public struct TopLevelModuleStruct {
// CHECK-DAG:     public init()
// CHECK-DAG:     public init(value: CInt)
// CHECK-DAG:     public var value: CInt
// CHECK-DAG: }

// CHECK-NOT: ImplicitModuleStruct
// CHECK-NOT: ExplicitModuleStruct

// IMPLICIT:     import TopLevelModule.ExplicitSubmodule
// IMPLICIT-DAG: import TopLevelModule.ImplicitSubmodule
// IMPLICIT-DAG: public struct TopLevelModuleStruct {
// IMPLICIT-DAG:     public init()
// IMPLICIT-DAG:     public init(value: CInt)
// IMPLICIT-DAG:     public var value: CInt
// IMPLICIT-DAG: }
// IMPLICIT-DAG: public struct ImplicitSubmoduleStruct {
// IMPLICIT-DAG:     public init()
// IMPLICIT-DAG:     public init(value: CInt)
// IMPLICIT-DAG:     public var value: CInt
// IMPLICIT-DAG: }

// IMPLICIT-NOT: ExplicitSubmoduleStruct

// EXPLICIT:     public struct ExplicitSubmoduleStruct {
// EXPLICIT-DAG:     public init()
// EXPLICIT-DAG:     public init(value: CInt)
// EXPLICIT-DAG:     public var value: CInt
// EXPLICIT-DAG: }

// EXPLICIT-NOT: import TopLevelModule{{.*}}
// EXPLICIT-NOT: TopLevelModuleStruct
// EXPLICIT-NOT: ImplicitModuleStruct
