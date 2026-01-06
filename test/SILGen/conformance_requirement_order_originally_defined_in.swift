// RUN: %target-swift-emit-silgen %s -module-name main | %FileCheck %s

// REQUIRES: OS=macosx

@_originallyDefinedIn(module: "AnimalKit", macOS 10.10)
@available(macOS 10.9, *)
public protocol Ungulate {}

@_originallyDefinedIn(module: "HorseKit", macOS 10.10)
@available(macOS 10.9, *)
public protocol Horse {}

// CHECK-LABEL: sil [ossa] @$s4main39requirementOrderWithOriginallyDefinedInyyx9AnimalKit8UngulateRz05HorseI00K0RzlF : $@convention(thin) <T where T : Ungulate, T : Horse> (@in_guaranteed T) -> () {
public func requirementOrderWithOriginallyDefinedIn<T: Ungulate & Horse>(_: T) {}