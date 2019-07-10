// RUN: %target-swift-frontend -emit-ir %s -g -I %S/Inputs | %FileCheck %s
// REQUIRES: OS=macosx

import Darwin

func use<T>(_ t: T) {}

let blah = size_t(1024)
use(blah)
// CHECK: !DIDerivedType(tag: DW_TAG_typedef,
// CHECK-SAME:           scope: ![[DARWIN_MODULE:[0-9]+]],
// CHECK: ![[DARWIN_MODULE]] = !DIModule({{.*}}, name: "stddef"
