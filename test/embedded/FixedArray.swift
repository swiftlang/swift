
// RUN: %target-run-simple-swift(%S/Inputs/ExperimentalFixedArray.swift -enable-experimental-feature Embedded -enable-experimental-feature FixedArrays -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-O %S/Inputs/ExperimentalFixedArray.swift -enable-experimental-feature Embedded -enable-experimental-feature FixedArrays -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// Also test in non-embedded mode

// RUN: %target-run-simple-swift(-target %target-cpu-apple-macosx10.13 %S/Inputs/ExperimentalFixedArray.swift -enable-experimental-feature FixedArrays -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-cpu-apple-macosx10.13 -O %S/Inputs/ExperimentalFixedArray.swift -enable-experimental-feature FixedArrays -parse-as-library -wmo) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

func simpleStackAllocated() {
  var a = FixedArray<Int>(capacity: 10)
  a.append(27)
  a.append(28)
  a.append(29)
  a.append(30)
  printit(a)
}

func testDestroyOfElements() {
  var a = FixedArray<CheckLifetime>(capacity: 10)
  a.append(CheckLifetime(27))
  a.append(CheckLifetime(28))
  a.append(CheckLifetime(29))
  a.append(CheckLifetime(30))

  // The `*` is a prefix operator and serves as syntatic sugar for FixedArray.view.
  printit(*a)
}

struct IntByte: Printable {
  let i: Int32
  let b: Int8

  func print() {
    Swift.print(i, terminator: "")
    Swift.print(",", terminator: "")
    Swift.print(b, terminator: "")
  }
}

extension Optional: Printable where Wrapped == Int64 {
  func print() {
    if let x = self {
      Swift.print(x, terminator: "")
    } else {
      Swift.print("nil", terminator: "")
    }
  }
}

struct S {
  // Space for elements is reserved in the data section, but not initialized, yet
  static var a = FixedArray<Int>(capacity: 10)

  // Elements are initialized in the data section.
  // The `^` is a prefix operator and serves as syntatic sugar to create a FixedArray literal.
  static let b = ^[1, 2, 3]

  static let c: FixedArray<Int64?> = ^[10, 20, 30]
  static let d: FixedArray<IntByte> = ^[IntByte(i: 1, b: 2), IntByte(i: 3, b: 4), IntByte(i: 5, b: 6)]
  static var e = FixedArray<Int64?>(capacity: 3)
  static var f = FixedArray<IntByte>(capacity: 3)
}

func globalWithAppend() {
  S.a.append(27)
  S.a.append(28)
  S.a.append(29)
  S.a.append(30)
  printit(S.a)
}

// First method to pass a FixedArray to a function: as `borrowing` parameter
func printit<T: Printable>(_ a: borrowing FixedArray<T>) {
  // Currently FixedArray cannot conform to Collection, yet.
  // For iteration, we need to take the detour through BufferView.
  for x in *a {
    x.print()
    print("")
  }
}

// Second method to pass a FixedArray to a function: pass a view to its elements
func printit<T: Printable>(_ a: BufferView<T>) {
  for x in a {
    x.print()
    print("")
  }
}

extension FixedArray {
  // Functions _can_ return a FixedArray, but must be declared as `@_transparent`.
  @_transparent
  func map<V>(_ c: (T) -> V) -> FixedArray<V> {
    var newArray = FixedArray<V>(capacity: self.count)
    for element in view() {
      newArray.append(c(element))
    }
    return newArray
  }
}

func testMap() {
  let a = ^[1, 2, 3]

  printit(a.map { $0 + 100 })
}


// A somehow more complex exmample, where FixedArray is used in another struct

struct Matrix<T: Printable>: ~Copyable {
  var elements: FixedArray<T>
  let rows: Int
  let columns: Int

  @_transparent
  init(rows: Int, columns: Int, _ elements: [T]) {
    self.rows = rows
    self.columns = columns
    precondition(rows * columns == elements.count)
    self.elements = FixedArray(elements: elements)
  }

  @_transparent
  init(rows: Int, columns: Int, initialValue: T) {
    self.rows = rows
    self.columns = columns
    self.elements = FixedArray(repeating: initialValue, count: rows * columns)
  }

  subscript(row: Int, col: Int) -> T {
    get {
      return elements[row * columns + col]
    }
    set {
      elements[row * columns + col] = newValue
    }
  }

  func rowView(for row: Int) -> BufferView<T> {
    let start = elements.view().startIndex.advanced(by: row * columns)
    let end = start.advanced(by: columns)
    return elements.view()[start..<end]
  }

  func print() {
    for r in 0..<rows {
      Swift.print("(", terminator: "")
      for c in 0..<columns {
        self[r, c].print()
        if c < columns - 1 {
          Swift.print(",", terminator: "")
        } else {
          Swift.print(")")
        }
      }
    }
  }
}

// Functions _can_ return a struct containing a FixedArray, but must be declared as `@_transparent`.
@_transparent
func +(lhs: borrowing Matrix<Int>, rhs: borrowing Matrix<Int>) -> Matrix<Int> {
  var result = Matrix(rows: lhs.rows, columns: lhs.columns, initialValue: 0)
  for i in 0..<lhs.rows {
    for j in 0..<lhs.columns {
      result[i, j] = lhs[i, j] + rhs[i, j]
    }
  }
  return result
}

struct MatricConsts {
  // A Matrix, containing a FixedArray, initialized with an Array literal -> elements are allocated in the data section.
  static let c = Matrix(rows: 2, columns: 3,
      [ 10, 20, 30,
        40, 50, 60 ])
}

func testMatrixAdd() {
  let x = Matrix(rows: 2, columns: 3,
      [ 1, 2, 3,
        4, 5, 6 ])

  let y = x + MatricConsts.c

  y.print()
}


// Example of another type, using the basic non-allocating buffer utilities (allocateVector).
// It's possible to implement such a type outside of the stdlib.

struct RingBuffer<Element>: ~Copyable {
  private let buffer: UnsafeMutablePointer<Element>
  private let capacity: Int
  private var writeIndex: Int
  private var readIndex = 0
  
  @_transparent
  public init(capacity: Int) {
    self.capacity = capacity
    self.buffer = _allocateVector(elementType: Element.self, capacity: capacity)
    self.writeIndex = 0
  }

  @_transparent
  init(repeating repeatedValue: Element, count: Int) {
    self.capacity = count + 1
    self.buffer = _allocateVector(elementType: Element.self, capacity: capacity)
    self.writeIndex = count
    for i in 0..<count {
      (buffer + i).initialize(to: repeatedValue)
    }
  }

  var isEmpty: Bool { readIndex == writeIndex }

  mutating func push(_ element: Element) -> Bool {
    guard (writeIndex + 1) % capacity != readIndex else {
      return false
    }
    (buffer + writeIndex).initialize(to: element)
    writeIndex = (writeIndex + 1) % capacity
    return true
  }
  
  mutating func next() -> Element? {
    if isEmpty {
      return nil
    }
    let value = (buffer + readIndex).move()
    readIndex = (readIndex + 1) % capacity
    return value
  }

  deinit {
    var idx = readIndex
    while idx != writeIndex {
      _ = (buffer + idx).move()
      idx = (idx + 1) % capacity
    }
  }
}

func testRingBuffer() {
  var rb = RingBuffer<Int>(capacity: 3)

  print(rb.push(1))
  print(rb.push(2))
  print(rb.next())
  print(rb.push(3))
  print(rb.push(4))
  print(rb.next())
  print(rb.next())
  print(rb.next())
}


// Test utilities

protocol Printable {
  func print()
}

extension Int: Printable {
  func print() {
    Swift.print(self, terminator: "")
  }
}

func print(_ i: Int?) {
  if let i = i {
    print(i)
  } else {
    print("nil")
  }
}

final class CheckLifetime: Printable {
  let x: Int
  init(_ x: Int) {
    self.x = x
  }

  deinit {
    Swift.print("deinit ", terminator: "")
    Swift.print(x)
  }

  func print() {
    Swift.print("CheckLifetime(", terminator: "")
    Swift.print(x, terminator: ")")
  }
}

// Check for correctness

@main
struct Main {
  static func main() {
    // CHECK-LABEL: ### simpleStackAllocated
    // CHECK-NEXT:  27
    // CHECK-NEXT:  28
    // CHECK-NEXT:  29
    // CHECK-NEXT:  30
    print("### simpleStackAllocated")
    simpleStackAllocated()

    // CHECK-LABEL: ### testDestroyOfElements
    // CHECK-NEXT:  CheckLifetime(27)
    // CHECK-NEXT:  CheckLifetime(28)
    // CHECK-NEXT:  CheckLifetime(29)
    // CHECK-NEXT:  CheckLifetime(30)
    // CHECK-NEXT:  deinit 27
    // CHECK-NEXT:  deinit 28
    // CHECK-NEXT:  deinit 29
    // CHECK-NEXT:  deinit 30
    print("### testDestroyOfElements")
    testDestroyOfElements()

    // CHECK-LABEL: ### globalWithAppend
    // CHECK-NEXT:  27
    // CHECK-NEXT:  28
    // CHECK-NEXT:  29
    // CHECK-NEXT:  30
    print("### globalWithAppend")
    globalWithAppend()

    // CHECK-LABEL: ### globalLiteral
    // CHECK-NEXT:  1
    // CHECK-NEXT:  2
    // CHECK-NEXT:  3
    print("### globalLiteral")
    printit(S.b)

    // CHECK-LABEL: ### globalOptional
    // CHECK-NEXT:  10
    // CHECK-NEXT:  20
    // CHECK-NEXT:  30
    print("### globalOptional")
    printit(S.c)

    // CHECK-LABEL: ### globalStruct
    // CHECK-NEXT:  1,2
    // CHECK-NEXT:  3,4
    // CHECK-NEXT:  5,6
    print("### globalStruct")
    printit(S.d)

    // CHECK-LABEL: ### globalUninitOptional
    // CHECK-NEXT:  100
    // CHECK-NEXT:  200
    // CHECK-NEXT:  300
    print("### globalUninitOptional")
    S.e.append(100)
    S.e.append(200)
    S.e.append(300)
    printit(S.e)

    // CHECK-LABEL: ### globalUninitStruct
    // CHECK-NEXT:  11,21
    // CHECK-NEXT:  31,41
    // CHECK-NEXT:  51,61
    print("### globalUninitStruct")
    S.f.append(IntByte(i: 11, b: 21))
    S.f.append(IntByte(i: 31, b: 41))
    S.f.append(IntByte(i: 51, b: 61))
    printit(S.f)

    // CHECK-LABEL: ### testMap
    // CHECK-NEXT:  101
    // CHECK-NEXT:  102
    // CHECK-NEXT:  103
    print("### testMap")
    testMap()

    // CHECK-LABEL: ### testMatrixAdd
    // CHECK-NEXT: (11,22,33)
    // CHECK-NEXT: (44,55,66)
    print("### testMatrixAdd")
    testMatrixAdd()

    // CHECK-LABEL: ### testRingBuffer
    // CHECK-NEXT:  true
    // CHECK-NEXT:  true
    // CHECK-NEXT:  1
    // CHECK-NEXT:  true
    // CHECK-NEXT:  false
    // CHECK-NEXT:  2
    // CHECK-NEXT:  3
    // CHECK-NEXT:  nil
    print("### testRingBuffer")
    testRingBuffer()
  }
}

