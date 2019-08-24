// RUN: not %target-swift-frontend -emit-ir -primary-file %s

// FIXME: Should this type check?

struct ArrayWrapper<Element>: Collection
    where Element: Comparable {
    private var store = [Element]()

    typealias SubSequence = ArrayWrapper<Element>

    init(_ values: [Element]) {
      store = values
    }

    var startIndex: Int {
        return store.startIndex
    }

    var endIndex: Int {
        return store.endIndex
    }

    subscript(position: Int) -> Iterator.Element {
        return store[index]
    }

    func makeIterator() -> AnyIterator<Element> {
        var index = 0
        return AnyIterator {
            guard index < self.store.count else { return nil }
            defer { index += 1 }
            return self.store[index]
        }
    }
}

print(ArrayWrapper([22, 3, 1, 44, 6, 22]))
