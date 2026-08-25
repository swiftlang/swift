/*
Verify that the concurrency types a debugger presents a task through
reach the debug info even though the program never names them.

RUN: %target-swift-frontend %s -target %target-cpu-apple-macos14 -emit-ir -g -enable-experimental-feature Embedded -wmo -o - | %FileCheck %s
RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s --check-prefix=RESILIENT

REQUIRES: OS=macosx
REQUIRES: embedded_stdlib
REQUIRES: swift_feature_Embedded
REQUIRES: concurrency
REQUIRES: optimized_stdlib

A name and a size are not enough: a debugger reads the stored task pointer and
the raw priority back out of a value it built itself, by member name.

CHECK-DAG: ![[TASK:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "UnsafeCurrentTask",{{.*}} elements: ![[TASK_ELEMENTS:[0-9]+]],{{.*}} identifier: "$eSctD")
CHECK-DAG: ![[TASK_ELEMENTS]] = !{![[RAW_TASK:[0-9]+]]
CHECK-DAG: ![[RAW_TASK]] = !DIDerivedType(tag: DW_TAG_member, name: "_rawTask",
CHECK-DAG: ![[PRIORITY:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "TaskPriority",{{.*}} elements: ![[PRIORITY_ELEMENTS:[0-9]+]],{{.*}} identifier: "$eScPD")
CHECK-DAG: ![[PRIORITY_ELEMENTS]] = !{![[RAW_VALUE:[0-9]+]]
CHECK-DAG: ![[RAW_VALUE]] = !DIDerivedType(tag: DW_TAG_member, name: "rawValue",

Each one is anchored with a DW_TAG_imported_declaration; retaining the type is
not enough to survive dsymutil, since nothing in the program refers to it.

CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !{{[0-9]+}}, entity: ![[TASK]],
CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !{{[0-9]+}}, entity: ![[PRIORITY]],

Across a resilience boundary the layout is not this module's to describe, and a
debugger there has the reflection metadata to read instead.

RESILIENT-NOT: name: "UnsafeCurrentTask"
RESILIENT-NOT: name: "TaskPriority"
*/

import _Concurrency
