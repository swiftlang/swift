// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.50 -emit-module -parse-stdlib %S/../Inputs/empty.swift -o %t
// RUN: not %target-swift-frontend -I %t -typecheck %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -I %t -typecheck %s -disable-target-os-checking
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.50 -I %t -typecheck %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.50.1 -I %t -typecheck %s

// REQUIRES: OS=macosx

// CHECK: :[[@LINE+1]]:8: error: module file's minimum deployment target is OS X v10.50: {{.*}}empty.swiftmodule{{$}}
import empty
