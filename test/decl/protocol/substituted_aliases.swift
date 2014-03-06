// RUN: %swift -parse %s -verify

struct Q<T: Collection> : Sequence {
  func generate() -> T.GeneratorType {
    return base.generate()
  }
  
  func _adopt(newBuffer: Array<Q<T>.GeneratorType.Element>) {
  }
  var base: T
}
