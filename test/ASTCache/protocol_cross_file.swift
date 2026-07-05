// RUN: rm -rf %t.cache %t
// RUN: mkdir -p %t.cache %t
// RUN: cp %s %t/Protocols.swift
// RUN: printf 'public struct ContainerWrapper<C: Container> {\n    public var container: C\n    public var item: C.Item\n    public init(_ c: C) { self.container = c; self.item = c.item }\n}\n' > %t/UsesProtocol.swift
// RUN: printf 'public struct MyArray: Sequence2 {\n    public typealias Element = Int\n    public var values: [Int]\n    public init(_ v: [Int]) { self.values = v }\n}\n' > %t/Conforms.swift
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.proto.o \
// RUN:   -primary-file %t/Protocols.swift %t/UsesProtocol.swift %t/Conforms.swift 2>&1 | %FileCheck %s --check-prefix=COLD1
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.uses.o \
// RUN:   -primary-file %t/UsesProtocol.swift %t/Protocols.swift %t/Conforms.swift 2>&1 | %FileCheck %s --check-prefix=COLD2
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.conforms.o \
// RUN:   -primary-file %t/Conforms.swift %t/Protocols.swift %t/UsesProtocol.swift 2>&1 | %FileCheck %s --check-prefix=COLD3
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.warm.o \
// RUN:   -primary-file %t/UsesProtocol.swift %t/Protocols.swift %t/Conforms.swift 2>&1 | %FileCheck %s --check-prefix=WARM

// COLD1-DAG: AST cache: MISS (no cache file)
// COLD1-DAG: AST cache: SAVED

// COLD2-DAG: AST cache: HIT for {{.*}}Protocols.swift
// COLD2-DAG: AST cache: MISS (no cache file)
// COLD2-DAG: AST cache: SAVED

// COLD3-DAG: AST cache: HIT for {{.*}}Protocols.swift
// COLD3-DAG: AST cache: HIT for {{.*}}UsesProtocol.swift
// COLD3-DAG: AST cache: MISS (no cache file)
// COLD3-DAG: AST cache: SAVED

// WARM-DAG: AST cache: HIT for {{.*}}UsesProtocol.swift
// WARM-DAG: AST cache: HIT for {{.*}}Protocols.swift
// WARM-DAG: AST cache: HIT for {{.*}}Conforms.swift

// Test 1: Protocol with associated type used in cross-file generic constraint
// The C.Item reference in UsesProtocol.swift creates a DEPENDENT_MEMBER_TYPE
// that references Container's associated type Item. When Container is in a
// cached file, this associated type must be resolved via name-based lookup.
public protocol Container {
    associatedtype Item
    var item: Item { get }
}

// Test 2: Protocol conformance across cached files
// MyArray in Conforms.swift conforms to Sequence2 defined here.
// This exercises the setWitness assertion (risk #3 in the plan).
public protocol Sequence2 {
    associatedtype Element
}
