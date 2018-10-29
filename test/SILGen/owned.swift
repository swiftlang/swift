// RUN: %target-swift-emit-silgen -Xllvm -sil-full-demangle %s -disable-objc-attr-requires-foundation-module -enable-sil-ownership | %FileCheck %s

// see shared.swift for thunks/conversions between __shared and __owned.

class RefAggregate {}
struct ValueAggregate { let x = RefAggregate() }

// CHECK-LABEL: sil hidden @$s5owned0A10_arguments7trivial5value3refySin_AA14ValueAggregateVnAA03RefG0CntF : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate) -> () {
func owned_arguments(trivial : __owned Int, value : __owned ValueAggregate, ref : __owned RefAggregate) {
    let t = trivial
    let v = value
    let r = ref
}

struct Foo {
    var x: ValueAggregate

    // CHECK-LABEL: sil hidden @$s5owned3FooV20methodOwnedArguments7trivial5value3refySin_AA14ValueAggregateVnAA03RefJ0CntF : $@convention(method) (Int, @owned ValueAggregate, @owned RefAggregate, @guaranteed Foo) -> () {
    func methodOwnedArguments(trivial : __owned Int, value : __owned ValueAggregate, ref : __owned RefAggregate) {
        let t = trivial
        let v = value
        let r = ref
    }
}

// rdar://problem/38390524
// CHECK-LABEL: sil hidden @$s5owned19oneUnnamedArgument1yyAA14ValueAggregateVnF : $@convention(thin) (@owned ValueAggregate) -> () {
func oneUnnamedArgument1(_: __owned ValueAggregate) {}
// CHECK-LABEL: sil hidden @$s5owned19oneUnnamedArgument2yyAA12RefAggregateCnF : $@convention(thin) (@owned RefAggregate) -> () {
func oneUnnamedArgument2(_: __owned RefAggregate) {}
