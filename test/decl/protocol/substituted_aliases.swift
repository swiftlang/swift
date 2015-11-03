// RUN: %target-parse-verify-swift

struct Q<T: CollectionType> : SequenceType {
  func generate() -> T.Iterator {
    return base.generate()
  }
  
  func _adopt(newBuffer: Array<Q<T>.Iterator.Element>) {
  }
  var base: T
}
