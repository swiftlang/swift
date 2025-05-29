// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weaklinked_import_helper.swiftmodule -parse-as-library %S/Inputs/weaklinked_import_helper.swift -enable-library-evolution
//
// RUN: echo 'import Foundation' > %t/intermediate_foundation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -emit-module-path %t/intermediate_foundation.swiftmodule -parse-as-library %t/intermediate_foundation.swift -I %t -enable-library-evolution
//
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -primary-file %s -I %t -emit-ir -Xcc -fmodule-map-file=%S/Inputs/weaklinked_import_helper_clang.modulemap | %FileCheck %s

@_weakLinked import intermediate_foundation
import Foundation

// ThisModule -weak imports-> intermediate_foundation -imports-> Foundation
// Because Foundation is _not_ re-exported, make sure any symbols from it are strongly referenced.
// CAUTION: Suppose you _want_ Foundation to be weak-linked. It's not enough to just `@_exported import Foundation`
//          in the intermediate_foundation module. That only gets you the Swift half of the Foundation overlay.

// CHECK-DAG: @"OBJC_CLASS_$_NSNotification" = external global %objc_class
_ = NSNotification()
