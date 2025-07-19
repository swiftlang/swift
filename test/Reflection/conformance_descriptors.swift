// UNSUPPORTED: OS=windows-msvc
//
// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros

// rdar://100558042
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)

// RUN: %target-build-swift -target %target-swift-5.2-abi-triple %S/Inputs/ConcreteTypes.swift %S/Inputs/GenericTypes.swift %S/Inputs/Protocols.swift %S/Inputs/Extensions.swift %S/Inputs/Closures.swift %S/Inputs/Conformances.swift -parse-as-library -emit-module -emit-library %no-fixup-chains -module-name ConformanceCheck -o %t/Conformances
// RUN: %target-swift-reflection-dump %t/Conformances %platform-module-dir/%target-library-name(swiftCore) | %FileCheck %s

// CHECK: CONFORMANCES:
// CHECK: =============
// CHECK-DAG: 16ConformanceCheck10SomeStructV{{[56]}}${{[0-9a-f]*}}yXZ0c6NestedD0{{[56]}}${{[0-9a-f]*}}LLV (ConformanceCheck.SomeStruct.(unknown context at ${{[0-9a-f]*}}).(SomeNestedStruct in ${{[0-9a-f]*}})) : ConformanceCheck.MyProto
// CHECK-DAG: 16ConformanceCheck3fooV3barV3bazV3quxV4quuxV5corgeV6graultV6garplyV5waldoV4fredV5plughV5xyzzyV4thudV18SomeConformingTypeV (ConformanceCheck.foo.bar.baz.qux.quux.corge.grault.garply.waldo.fred.plugh.xyzzy.thud.SomeConformingType) : ConformanceCheck.MyProto
// CHECK-DAG: 16ConformanceCheck7StructAV (ConformanceCheck.StructA) : ConformanceCheck.MyProto, Swift.Hashable, Swift.Equatable
// CHECK-DAG: 16ConformanceCheck2E4O (ConformanceCheck.E4) : ConformanceCheck.P1, ConformanceCheck.P2, ConformanceCheck.P3
// CHECK-DAG: 16ConformanceCheck2C4V (ConformanceCheck.C4) : ConformanceCheck.P1, ConformanceCheck.P2
// CHECK-DAG: 16ConformanceCheck2S4V (ConformanceCheck.S4) : ConformanceCheck.P1, ConformanceCheck.P2
// CHECK-DAG: 16ConformanceCheck2C1C (ConformanceCheck.C1) : ConformanceCheck.ClassBoundP
// CHECK-DAG: 16ConformanceCheck3fooV (ConformanceCheck.foo) : ConformanceCheck.MyProto
