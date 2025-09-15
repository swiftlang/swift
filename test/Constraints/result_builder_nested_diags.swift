// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

protocol Element {
  associatedtype Body: Element

  @Builder
  var body: Body { get }
}

@resultBuilder
struct Builder {
  static func buildBlock<C>(_ content: C) -> C {
    fatalError()
  }

  static func buildBlock<each C>(
    _ content: repeat each C
  ) -> TupleContent<repeat each C> {
    fatalError()
  }
}

struct TupleContent<each Content> {
  var content: (repeat each Content)
  var body: Never { fatalError() }
}

struct List<Content> {
  @Builder  var content: Content

  var body: Never { fatalError() }
}

extension List: Element where Content: Element {}

public struct For<Data: RandomAccessCollection, Content> {
  init(
    _ data: Data,
    @Builder content: @escaping (Data.Element) -> Content
  ) {
  }

  var body: Never { fatalError() }
}

extension For: Element where Content: Element {}

extension Never : Element {
  var body: Never { fatalError() }
}

extension TupleContent: Element where repeat each Content: Element {}

struct FakeElement {}

struct Element1: Element { var body: some Element { fatalError() } }
struct Element2: Element { var body: some Element { fatalError() } }
struct Element3: Element { var body: some Element { fatalError() } }

struct Test1: Element {
  var body: some Element { // expected-note {{opaque return type declared here}}
    List {
      FakeElement()
      // expected-error@-1 {{return type of property 'body' requires that 'FakeElement' conform to 'Element'}}
    }
  }
}

struct Test2: Element {
  var body: some Element { // expected-note {{opaque return type declared here}}
    List {
      Element1()
      List {
        Element2()
        FakeElement()
        // expected-error@-1 {{return type of property 'body' requires that 'FakeElement' conform to 'Element'}}
        Element3()
      }
    }
  }
}

struct Test3: Element {
  var body: some Element { // expected-note {{opaque return type declared here}}
    List {
      For([1, 2, 3]) { _ in
        Element1()
        List {
          Element2()
          FakeElement() // expected-error {{return type of property 'body' requires that 'FakeElement' conform to 'Element'}}
        }
      }
    }
  }
}

struct Test4: Element {
  var body: some Element { // expected-note {{opaque return type declared here}}
    FakeElement()
    // expected-error@-1 {{return type of property 'body' requires that 'FakeElement' conform to 'Element'}}
    Element1()
  }
}
