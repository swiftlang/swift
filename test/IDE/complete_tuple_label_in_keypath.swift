// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | %FileCheck %s

func foo() -> [(Int, [Int])] {
    var searchResults: [Int: [Int]] = [:]
    searchResults
          .map { $0 }
          .sorted(by: \.#^COMPLETE^#key)
}

public extension Sequence {
    func sorted<T: Comparable>(by keyPath: KeyPath<Element, T>) -> [Element] {
        return sorted { a, b in
            return a[keyPath: keyPath] > b[keyPath: keyPath]
        }
    }
}

// CHECK-DAG: Pattern/CurrNominal:                key[#Int#];
// CHECK-DAG: Pattern/CurrNominal:                value[#[Int]#];

