// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -enable-experimental-feature DefaultGenerics -disable-availability-checking -parse-as-library -o %t
// RUN: %target-sil-opt -enable-sil-verify-all %t/default_generics.swiftmodule -o - | %FileCheck %s

// REQUIRES: swift_feature_DefaultGenerics

// CHECK: struct A<T = Int> {
struct A<T = Int> {}

protocol Allocator {}
struct SystemAllocator: Allocator {}

// CHECK: struct Vec<Element, Alloc = SystemAllocator> where Alloc : Allocator, Element : ~Copyable {
struct Vec<Element: ~Copyable, Alloc: Allocator = SystemAllocator> {}

// CHECK: func foo(_: Vec<Int, SystemAllocator>)
func foo(_: Vec<Int>) {}
