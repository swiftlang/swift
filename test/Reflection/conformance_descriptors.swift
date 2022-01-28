// UNSUPPORTED: windows
// RUN: %empty-directory(%t)

// RUN: %target-build-swift -Xfrontend -enable-anonymous-context-mangled-names %S/Inputs/ConcreteTypes.swift %S/Inputs/GenericTypes.swift %S/Inputs/Protocols.swift %S/Inputs/Extensions.swift %S/Inputs/Closures.swift %S/Inputs/Conformances.swift -parse-as-library -emit-module -emit-library -module-name ConformanceCheck -o %t/Conformances
// RUN: %target-swift-reflection-dump -binary-filename %t/Conformances | %FileCheck %s

// CHECK: CONFORMANCES:
// CHECK: =============
// CHECK-DAG: ConformanceCheck.StructA : ConformanceCheck.MyProto, Swift.Hashable, Swift.Equatable
// CHECK-DAG: ConformanceCheck.C4 : ConformanceCheck.P1, ConformanceCheck.P2
// CHECK-DAG: ConformanceCheck.E4 : ConformanceCheck.P1, ConformanceCheck.P2, ConformanceCheck.P3
// CHECK-DAG: ConformanceCheck.S4 : ConformanceCheck.P1, ConformanceCheck.P2
// CHECK-DAG: ConformanceCheck.foo.bar.baz.qux.quux.corge.grault.garply.waldo.fred.plugh.xyzzy.thud.SomeConformingType : ConformanceCheck.MyProto
// CHECK-DAG: ConformanceCheck.C1 : ConformanceCheck.ClassBoundP
