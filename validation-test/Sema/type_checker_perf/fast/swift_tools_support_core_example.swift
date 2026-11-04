// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

func editDistance(_ source: String, _ target: String) -> Int {
    return 0
}

func bestMatch(for input: String, from options: [String]) -> String? {
    return options
        .map { ($0, editDistance(input, $0)) }
        .filter { $0.1 <= ($0.0.count + 2) / 3 }
        .sorted { $0.1 < $1.1 }
        .first?.0
}

protocol SortComparator {
    associatedtype Compared
}

extension Sequence {
    func sorted<Comparator: SortComparator>(using comparator: Comparator) -> [Element] where Comparator.Compared == Element {
        return []
    }

    func sorted<S: Sequence, Comparator: SortComparator>(using comparators: S) -> [Element] where S.Element == Comparator, Element == Comparator.Compared {
        return []
    }
}

