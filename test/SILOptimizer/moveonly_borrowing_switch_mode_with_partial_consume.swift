// RUN: %target-swift-frontend -emit-sil -verify -parse-as-library %s

func foo() {
    let node = Node()
    bar(node.next)
}

struct Node: ~Copyable {
    var next: Link

    init() { fatalError() }
}

struct Link: ~Copyable {}

func bar(_: consuming Link) {}


