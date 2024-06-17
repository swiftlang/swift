// RUN: %target-swift-frontend -emit-sil -verify %s

struct Box<Wrapped: ~Copyable>: ~Copyable {
    init(_ element: consuming Wrapped) { }
}

struct Tree<Element>: ~Copyable {
    struct Node: ~Copyable { }
    
    enum Branch: ~Copyable {
        case empty
        case more(Box<Node>)
    }
    var root: Branch = .empty
}

extension Tree {
    mutating func insert(_ element: consuming Element) {
        root =
            switch root {
            case .empty:
                    .more(Box(Node()))
            case .more:
                fatalError()
            }
    }
}
