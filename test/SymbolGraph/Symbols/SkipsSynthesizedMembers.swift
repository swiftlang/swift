// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SkipsSynthesizedMembers -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SkipsSynthesizedMembers -I %t -pretty-print -skip-synthesized-members -output-dir %t
// RUN: %FileCheck %s --input-file %t/SkipsSynthesizedMembers.symbols.json

// CHECK-NOT: ::SYNTHESIZED

public struct ShouldAppear: Hashable {
    public let foo: Int
}
