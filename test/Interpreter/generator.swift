// RUN: %swift %s -i | FileCheck %s
//
// Currently this crashes in IRgen.
// XFAIL: *

/// This structure implements a utility for defining enumerable sequences in
/// terms of a generating function.
struct Generator<T> : Enumerable, Range {
  typealias Element = T
  typealias Elements = Generator<T>

  var isActive : Bool
  var hasNext : Bool
  var currentElement : Element
  var yield : ([byref] Element) -> Bool

  constructor (yield : ([byref] Element) -> Bool) {
    this.isActive = true
    this.hasNext = false
    this.yield = yield
  }

  func _fill() {
    if (!hasNext) {
      hasNext = true
      isActive = yield(&currentElement)
    }
  }

  func getElements() -> Elements { return this }

  func isEmpty() -> Bool {
    _fill()
    return !isActive
  }
  func getFirstAndAdvance() -> Element {
    // Fill the current element, if necessary.
    _fill()

    // Reset the state unless we are inactive.
    if isActive {
      hasNext = false
    }

    return currentElement
  }
}

/// Define a test range function using the generator struct.
func range(min : Int, max : Int) -> Generator<Int> {
  var current = min
  func yield(item : [byref] Int) -> Bool {
    if current >= max { return false }

    item = current
    ++current

    return true
  }

  typealias T = Generator<Int>
  return T(yield)
}

/// Define enumerate() using the generator struct.
func enumerate<T : Enumerable>(items : T)
    -> Generator<(Int, T.Elements.Element)> {
  var index = 0
  var it = items.getElements()
  func yield(item : [byref] (Int, T.Elements.Element)) -> Bool {
    // Check if we have exhausted the input
    if (it.isEmpty()) { return false }

    // Otherwise, return the next item.
    item = (index, it.getFirstAndAdvance())
    ++index
    return true
  }

  typealias T = Generator<(Int, T.Elements.Element)>
  return T(yield)
}

// Run some test code.
//
// CHECK: 0: 5
// CHECK: 1: 6
// CHECK: 2: 7
// CHECK: 3: 8
// CHECK: 4: 9
for (index, value) in enumerate(range(5, 10)) {
    print("\(index): \(value)\n")
}
