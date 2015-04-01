// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -emit-module -parse-stdlib %S/../Inputs/empty.swift -o %t
// RUN: not %target-swift-frontend -target x86_64-apple-macosx10.9 -I %t -parse %s 2>&1 | FileCheck %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -I %t -parse %s -disable-target-os-checking
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10 -I %t -parse %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.10.1 -I %t -parse %s

// REQUIRES: OS=macosx

// CHECK: :[[@LINE+1]]:8: error: module file's minimum deployment target is OS X v10.10: {{.*}}empty.swiftmodule{{$}}
import empty
