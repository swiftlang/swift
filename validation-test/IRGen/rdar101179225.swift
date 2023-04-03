// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -disable-availability-checking %s -emit-ir > /dev/null

struct Section: Identifiable {
    let id: Int
    let items: [any Item]
}

protocol Item: AnyObject, Identifiable {
}

protocol P {
}

struct Iter<D, I, C>: P where D : RandomAccessCollection, I : Hashable {
    init(_ xs: D, id: KeyPath<D.Element, I>, f: (D.Element) -> C) {}
}

struct V {
  var s: Section
  var p: some P {
    Iter(s.items, id: \.id) { x in
      ""
    }
  }
}
