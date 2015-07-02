// RUN: %target-swift-frontend %s -emit-silgen
import StdlibUnittest

func testCollectionIndexIsMinimalForwardIndex<
  C : CollectionType
  where
  C.Index == MinimalForwardIndex
>(c: C) {}

let c = MinimalForwardCollection([ OpaqueValue(1) ])
let i: MinimalForwardIndex = c.startIndex
dump(i)
testCollectionIndexIsMinimalForwardIndex(c)
