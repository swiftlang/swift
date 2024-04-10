// RUN: %target-swift-frontend -emit-silgen %s -DA1
// RUN: %target-swift-frontend -emit-silgen %s -DA2
// RUN: %target-swift-frontend -emit-silgen %s -DA3
// RUN: %target-swift-frontend -emit-silgen %s -DA4
// RUN: %target-swift-frontend -emit-silgen %s -DA5
// RUN: %target-swift-frontend -emit-silgen %s -DA6
// RUN: %target-swift-frontend -emit-silgen %s -DA7
// RUN: %target-swift-frontend -emit-silgen %s -DA8
// RUN: %target-swift-frontend -emit-silgen %s -DA9
// RUN: %target-swift-frontend -emit-silgen %s -DA10
// RUN: %target-swift-frontend -emit-silgen %s -DA11
// RUN: %target-swift-frontend -emit-silgen %s -DA12
// RUN: %target-swift-frontend -emit-silgen %s -DA13
// RUN: %target-swift-frontend -emit-silgen %s -DA14

#if A1

for x in G<String>() { _ = x }

#elseif A2

func f<T: Sequence>(_: T.Type) -> T.Element.Type { fatalError() }
let x: String.Type = f(G<String>.self)

#elseif A3

func f<T: Sequence>(_: T.Type) -> T.Iterator.Type { fatalError() }
let x: IndexingIterator<G<String>>.Type = f(G<String>.self)

#elseif A4

func f<T: Sequence>(_: T.Type) -> T.Iterator.Element.Type { fatalError() }
let x: String.Type = f(G<String>.self)

#elseif A5

func f<T: Collection>(_: T.Type) -> T.Element.Type { fatalError() }
let x: String.Type = f(G<String>.self)

#elseif A6

func f<T: Collection>(_: T.Type) -> T.Index.Type { fatalError() }
let x: Int.Type = f(G<String>.self)

#elseif A7

func f<T: Collection>(_: T.Type) -> T.SubSequence.Type { fatalError() }
let x: Slice<G<String>>.Type = f(G<String>.self)

#elseif A8

func f<T: Collection>(_: T.Type) -> T.SubSequence.Element.Type { fatalError() }
let x: String.Type = f(G<String>.self)

#elseif A9

func f<T: Collection>(_: T.Type) -> T.SubSequence.Index.Type { fatalError() }
let x: Int.Type = f(G<String>.self)

#elseif A10

func f<T: Collection>(_: T.Type) -> T.SubSequence.Iterator.Type { fatalError() }
let x: IndexingIterator<Slice<G<String>>>.Type = f(G<String>.self)

#elseif A11

func f<T: Collection>(_: T.Type) -> T.Indices.Type { fatalError() }
let x: Range<Int>.Type = f(G<String>.self)

#elseif A12

func f<T: Collection>(_: T.Type) -> T.Indices.Element.Type { fatalError() }
let x: Int.Type = f(G<String>.self)

#elseif A13

func f<T: Collection>(_: T.Type) -> T.Indices.SubSequence.Type { fatalError() }
let x: Range<Int>.Type = f(G<String>.self)

#elseif A14

func f<T: Collection>(_: T.Type) -> T.SubSequence.Indices.Type { fatalError() }
let x: Range<Int>.Type = f(G<String>.self)

#endif

struct G<Element>: RandomAccessCollection {
  public var startIndex: Int { 0 }
  public var endIndex: Int { 0 }
  public subscript(position: Int) -> Element { fatalError() }
}
