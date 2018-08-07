// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target x86_64-apple-ios50.50.1 -emit-module -parse-stdlib %S/../Inputs/empty.swift -o %t
// RUN: not %target-swift-frontend -parse-stdlib -target x86_64-apple-ios12 -I %t -typecheck %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-ios12 -I %t -typecheck %s -disable-target-os-checking
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-ios50.50.1 -I %t -typecheck %s
// RUN: %target-swift-frontend -parse-stdlib -target x86_64-apple-ios50.51 -I %t -typecheck %s

// REQUIRES: OS=ios

// CHECK: :[[@LINE+1]]:8: error: compiling for iOS 12.0, but module 'empty' has a minimum deployment target of iOS 50.50.1: {{.*}}empty.swiftmodule{{$}}
import empty
