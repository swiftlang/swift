// RUN: %target-typecheck-verify-swift

func f<T: Sequence>(_: T) -> T.Element {}

let x: Int = f(CustomCollection<Int>())

struct CustomCollection<Element>: RandomAccessCollection, MutableCollection {
    typealias SubSequence = ArraySlice<Element>
    typealias Index = Int
    typealias Indices = Range<Int>

    var startIndex: Int { fatalError() }
    var endIndex: Int { fatalError() }
    var first: Element? { fatalError() }
    var last: Element? { fatalError() }

    subscript(position: Index) -> Element {
        get { fatalError() }
        set { fatalError() }
    }

    subscript(bounds: Indices) -> SubSequence {
        get { fatalError() }
        set { fatalError() }
    }
}

