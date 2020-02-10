// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/linker_directives.tbd -previous-module-installname-map-file %S/Inputs/install-name-map.json >& %t/remark.txt
// RUN: %FileCheck %s < %t/remark.txt

// CHECK: remark: default previous install name for Foo is /System/default
// CHECK: remark: previous install name for Foo in macOS is /System/MacOS
// CHECK: remark: previous install name for Foo in iOS is /System/Others
// CHECK: remark: previous install name for Foo in tvOS is /System/Others
// CHECK: remark: previous install name for Foo in watchOS is /System/Others
