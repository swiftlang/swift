// RUN: %target-swift-frontend -emit-sil -verify %s

struct Box: ~Copyable {
    let ptr: UnsafeMutablePointer<Int>
    deinit { print("butt") }
}

enum List<Element>: ~Copyable {
    case end
    case more(Element, Box)
}

extension List {
    init() {
        self = .end
    }

    mutating func pop() -> Element {
        switch consume self {
        case .more(let element, let box): // expected-warning{{}}
            self = .end
            return element
        case .end: fatalError()
        }
    }
}

