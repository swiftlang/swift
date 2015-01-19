// RUN: %target-parse-verify-swift

struct Q<T: CollectionType> : SequenceType {
  func generate() -> T.Generator {
    return base.generate()
  }
  
  func _adopt(newBuffer: Array<Q<T>.Generator.Element>) {
  }
  var base: T
}
