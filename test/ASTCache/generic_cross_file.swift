// RUN: rm -rf %t.cache %t
// RUN: mkdir -p %t.cache %t
// RUN: cp %s %t/Heap.swift
// RUN: printf 'public struct PriorityQueue<Element: Comparable> {\n    internal var _heap: Heap<Element>\n    public init() { self._heap = Heap([]) }\n}\n' > %t/PriorityQueue.swift
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.heap.o -primary-file %t/Heap.swift %t/PriorityQueue.swift 2>&1 | %FileCheck %s --check-prefix=COLD
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.pq.o -primary-file %t/PriorityQueue.swift %t/Heap.swift 2>&1 | %FileCheck %s --check-prefix=COLD2
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.warm.o %t/PriorityQueue.swift %t/Heap.swift 2>&1 | %FileCheck %s --check-prefix=WARM

// WARM-DAG: AST cache: HIT for {{.*}}PriorityQueue.swift
// WARM-DAG: AST cache: HIT for {{.*}}Heap.swift

// COLD-DAG: AST cache: MISS (no cache file)
// COLD-DAG: AST cache: SAVED

// COLD2-DAG: AST cache: HIT for {{.*}}Heap.swift
// COLD2-DAG: AST cache: MISS (no cache file)
// COLD2-DAG: AST cache: SAVED

@usableFromInline
internal struct Heap<Element: Comparable> {
    @usableFromInline
    internal private(set) var storage: [Element]
    internal init(_ storage: [Element]) { self.storage = storage }
}
