// RUN: %target-typecheck-verify-swift

struct Q<T: Collection> : Sequence {
  func makeIterator() -> T.Iterator {
    return base.makeIterator()
  }
  
  func _adopt(_ newBuffer: Array<Q<T>.Iterator.Element>) {
  }
  var base: T
}
