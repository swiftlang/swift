// RUN: %target-typecheck-verify-swift

struct CustomCollection<T>: RandomAccessCollection {
    struct CustomIndices: RandomAccessCollection {
        var count: Int { fatalError() }

        var startIndex: Int { fatalError() }
        var endIndex: Int { fatalError() }

        subscript(index: Int) -> Int { fatalError() }
    }

    var count: Int { fatalError() }
    var indices: CustomIndices { fatalError() }
    var startIndex: Int { fatalError() }
    var endIndex: Int { fatalError() }
    func index(before i: Int) -> Int { fatalError() }
    func index(after i: Int) -> Int { fatalError() }
    subscript(index: Int) -> T { fatalError() }
}
