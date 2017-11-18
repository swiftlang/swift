// RUN: not %target-swift-frontend %s -typecheck

enum SortOrder {
    case ascending
    case equal
    case descending
}

struct SortedArray<Element> {
    var contents = [Element]()
    let compare: (Element, Element) -> SortOrder

    init(_ comparator: @escaping (Element, Element) -> SortOrder) {
        compare = comparator
    }

    mutating func add(_ element: Element) {

    }
}

extension SortedArray where Element: Comparable {
    init() {
        compare = { a, b in
            if a < b { return .ascending }
            else if a > b { return .descending }
            else { return .equal }
        }
    }

    init<S: Sequence>(_ sequence: S) where S.Iterator.Element == Element {
        self.init()

        for element in sequence {
            add(element)
        }
    }
}

extension SortedArray: Sequence {
    typealias Iterator = IndexingIterator

}
