// RUN: %target-swift-frontend %s -emit-ir

extension Sequence {
	typealias Element = Iterator.Element
}

func f<C: Sequence>(c: C) where C.Iterator == C {
  c.makeIterator()
}
