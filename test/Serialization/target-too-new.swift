// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.50 -emit-module -parse-stdlib %S/../Inputs/empty.swift -o %t
// RUN: not %target-swift-frontend -I %t -target x86_64-apple-macosx10.9 -typecheck %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -I %t -target x86_64-apple-darwin13 -typecheck %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -I %t -typecheck %s -disable-target-os-checking
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.50 -I %t -typecheck %s
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.50.1 -I %t -typecheck %s

// Allow any version when built with resilience. (Really we should encode a
// "minimum supported OS", but we don't have that information today.)
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.50 -emit-module -parse-stdlib %S/../Inputs/empty.swift -enable-resilience -o %t
// RUN: %target-swift-frontend -I %t -target x86_64-apple-macosx10.9 -typecheck %s
// RUN: %target-swift-frontend -I %t -target x86_64-apple-darwin13 -typecheck %s


// REQUIRES: OS=macosx

// CHECK: :[[@LINE+1]]:8: error: compiling for OS X 10.9, but module 'empty' has a minimum deployment target of OS X 10.50: {{.*}}empty.swiftmodule{{$}}
import empty
