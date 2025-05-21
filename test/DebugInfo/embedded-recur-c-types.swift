// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend %t/Main.swift -g -target %target-cpu-apple-macos14 -import-bridging-header %t/BridgingHeader.h -enable-experimental-feature Embedded -wmo -emit-ir -o - | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

// BEGIN BridgingHeader.h
#pragma once
typedef struct S2 S2_t;
typedef struct S1 {
    struct S2 *other;
} S1_t;
typedef struct S2 {
    S1_t *producer_pool;
} S2_t;

// BEGIN Main.swift

public var v: UnsafeMutablePointer<S1_t>? = nil

// CHECK-DAG:  !DICompositeType(tag: DW_TAG_structure_type, {{.*}}name: "$eSpySo4S1_taGD"
// CHECK-DAG:  !DICompositeType(tag: DW_TAG_structure_type, {{.*}}name: "$eSpySo2S2VGD"
