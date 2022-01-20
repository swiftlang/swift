// RUN: %empty-directory(%t)

// RUN: %target-build-swift -Xfrontend -enable-anonymous-context-mangled-names %S/Inputs/ConcreteTypes.swift %S/Inputs/Conformances.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -o %t/Conformances
// RUN: %target-swift-reflection-dump -binary-filename %t/Conformances | %FileCheck %s

// CHECK: CONFORMANCES:
// CHECK: =============
// CHECK: StructA : MyProto, Swift.Hashable, Swift.Equatable
