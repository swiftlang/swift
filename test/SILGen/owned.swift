// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-silgen %s -disable-objc-attr-requires-foundation-module -enable-sil-ownership -enable-guaranteed-normal-arguments | %FileCheck %s

// see shared.swift for thunks/conversions between __shared and __owned.

class RefAggregate {}
struct ValueAggregate { let x = RefAggregate() }

// CHECK-LABEL: sil hidden @$S5owned0A10_arguments7trivial5value3refySin_AA14ValueAggregateVnAA03RefG0CntF : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate) -> () {
func owned_arguments(trivial : __owned Int, value : __owned ValueAggregate, ref : __owned RefAggregate) {}

struct Foo {
    var x: ValueAggregate

    // CHECK-LABEL: sil hidden @$S5owned3FooV20methodOwnedArguments7trivial5value3refySin_AA14ValueAggregateVnAA03RefJ0CntF : $@convention(method) (Int, @owned ValueAggregate, @owned RefAggregate, @guaranteed Foo) -> () {
    func methodOwnedArguments(trivial : __owned Int, value : __owned ValueAggregate, ref : __owned RefAggregate) {}
}
