// RUN: rm -rf %t && mkdir -p %t
// RUN: %swift -target x86_64-apple-macosx10.10 -emit-module -parse-stdlib %S/../Inputs/empty.swift -o %t
// RUN: not %swift -target x86_64-apple-macosx10.9 -I %t -parse %s 2>&1 | FileCheck %s
// RUN: %swift -target x86_64-apple-macosx10.10 -I %t -parse %s
// RUN: %swift -target x86_64-apple-macosx10.10.1 -I %t -parse %s

// CHECK: :[[@LINE+1]]:8: error: module file's minimum deployment target is OS X v10.10: {{.*}}empty.swiftmodule{{$}}
import empty
