// REQUIRES: asserts
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations
//
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -module-name CompleteSwiftTypes -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule
// RUN: %target-build-swift -parse-as-library %t/unrelated.swift -module-name UnrelatedSwiftModule -emit-module -emit-module-path %t/UnrelatedSwiftModule.swiftmodule
// RUN: not %target-swift-frontend -parse-as-library -enable-upcoming-feature ImportObjcForwardDeclarations -enable-objc-interop -typecheck -I %S/Inputs/custom-modules/IncompleteTypes -I %t %t/extra1.swift %t/extra2.swift %t/extra3.swift %t/trigger.swift -diagnostic-style llvm -print-stats 2>&1 | %FileCheck %s

// CHECK: note: interface 'Foo' is incomplete and cannot be imported as a stub; its name conflicts with a class in module 'CompleteSwiftTypes'
// CHECK: {{[1-9][0-9]*}} Clang module importer - # of duplicate imported Swift modules skipped while resolving forward-declared ObjC types

//--- unrelated.swift
public struct Irrelevant {}

//--- extra1.swift
import UnrelatedSwiftModule

//--- extra2.swift
import UnrelatedSwiftModule

//--- extra3.swift
import UnrelatedSwiftModule

//--- trigger.swift
import CompleteSwiftTypes
import ObjCLibraryForwardDeclaringCompleteSwiftTypes

func test() {
  _ = returnAFoo()
}
