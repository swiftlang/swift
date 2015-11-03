// RUN: %target-parse-verify-swift

struct Q<T: CollectionType> : SequenceType {
  func iterator() -> T.Iterator {
    return base.iterator()
  }
  
  func _adopt(newBuffer: Array<Q<T>.Iterator.Element>) {
  }
  var base: T
}
