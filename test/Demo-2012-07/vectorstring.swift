// RUN: false
// REQUIRES: disabled

struct VectorString {
  typealias Element = String
  var elements : Element[]
  var size, capacity : Int

  constructor() {
    size = 0
    capacity = 32
    elements = new Element[capacity]
  }

  var length : Int {
    get { return size }
  }

  func resize() {
    capacity *= 2
    var newElements = new Element[capacity]
    for i in 0..size {
      newElements[i] = elements[i]
    }
    elements = newElements
  }

  func append(item : Element) {
    if (size == capacity) {
      resize()
    }
    elements[size] = item
    ++size
  }

  func each(f : (Element) -> Void) {
    for item in elements[0..size] {
      f(item)
    }
  }

  subscript (i : Int) -> Element {
    get {
      return elements[i]
    }
    set {
      elements[i] = value
    }
  }

  // Support enumeration protocol on vectors.
  typealias Elements = Element[]
  func getElements() -> Elements { return elements[0..size] }
}
