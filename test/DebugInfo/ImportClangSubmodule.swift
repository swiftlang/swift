// RUN: rm -rf %t && mkdir -p %t
// REQUIRES: OS=macosx

// RUN: %target-swift-frontend -emit-ir %s -g -o - | %FileCheck %s

// CHECK: !DIImportedEntity(
// CHECK: tag: DW_TAG_imported_module{{.*}}entity: ![[C:.*]], line: [[@LINE+1]])
import Darwin.C

let irrational = sqrt(2 as Double)

// CHECK: ![[C]] = !DIModule(scope: ![[Darwin:.*]], name: "C",
// CHECK: ![[Darwin]] = !DIModule(scope: null, name: "Darwin",
