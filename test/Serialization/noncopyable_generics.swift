// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/ncgenerics.swift                      \
// RUN:     -enable-experimental-feature NoncopyableGenerics                   \
// RUN:     -emit-module -module-name ncgenerics                               \
// RUN:     -o %t

// RUN: llvm-bcanalyzer %t/ncgenerics.swiftmodule | %FileCheck %s

// *** Notice that we're checking when _not_ using NoncopyableGenerics! ***
// RUN: %target-typecheck-verify-swift -I %t

// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk)                  \
// RUN:    -enable-experimental-feature NoncopyableGenerics                    \
// RUN:    -print-module -module-to-print=ncgenerics                           \
// RUN:    -I %t -source-filename=%s                                           \
// RUN:    | %FileCheck -check-prefix=CHECK-PRINT %s

// CHECK-NOT: UnknownCode

// CHECK-PRINT-DAG: protocol Generator<Value> {
// CHECK-PRINT-DAG: enum Maybe<Wrapped> : ~Copyable where Wrapped : ~Copyable {
// CHECK-PRINT-DAG: extension Maybe : Copyable {
// CHECK-PRINT-DAG: func ncIdentity<T>(_ t: consuming T) -> T where T : ~Copyable
// CHECK-PRINT-DAG: protocol Either<Left, Right> : ~Copyable {
// CHECK-PRINT-DAG:   associatedtype Left : ~Copyable
// CHECK-PRINT-DAG:   associatedtype Right : ~Copyable

import ncgenerics

struct TestRequirements: Generator {
    func next() -> Int? { return .none }
}

struct NC: ~Copyable {}

struct RegularStruct {}

func isItCopyable<T: Copyable>(_ t: T) {}
// expected-note@-1 {{generic parameter 'T' has an implicit Copyable requirement}}

func check() {
    var tr = TestRequirements()
    advance(by: 12, &tr)

    isItCopyable(TestRequirements())
    isItCopyable(RegularStruct())
    isItCopyable(NC()) // expected-error {{noncopyable type 'NC' cannot be substituted for copyable generic parameter 'T' in 'isItCopyable'}}

    let x: Maybe<NC> = .none
}

struct Witness: Either {
    typealias Left = Maybe<RegularStruct>
    typealias Right = Maybe<NC>
}
