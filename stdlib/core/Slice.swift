//===----------------------------------------------------------------------===//
// Slice<T> Type
//===----------------------------------------------------------------------===//

struct Slice<T> : Enumerator, Indexable, Enumerable, ArrayLiteralConvertible {
  var base : UnsafePointer<T>
  var length : Int
  var owner : Builtin.ObjectPointer

  // FIXME <rdar://problem/13235568> -- C interop functions should go in
  // extensions under CTypes.swift, but the compiler crashes on generic
  // extension methods that reference type parameters.
  func cAddress() -> UnsafePointer<T> {
    return UnsafePointer(base.value)
  }

  func byteLength() -> Int {
    return length * Int(Builtin.strideof(T))
  }

  static func convertFromHeapArray(base : Builtin.RawPointer,
                                   owner : Builtin.ObjectPointer,
                                   length : Builtin.Int64) -> Slice<T> {
    return Slice(UnsafePointer(base), Int(length) & Int64.max(), owner)
  }

  static func convertFromArrayLiteral(array : T...) -> Slice<T> {
    return array
  }

  // Indexable Protocol
  typealias IndexType = Int
  typealias Element = T
  func begin() -> IndexType {
    return 0
  }
  func end() -> IndexType {
    return length
  }
  func __getitem__(i: IndexType) -> Element {
    return this[i]
  }

  subscript (i : Int) -> T {
  get:
    debugTrap(UInt(i) < UInt(length))
    return base[i]
  set:
    debugTrap(UInt(i) < UInt(length))
    base[i] = value
  }

  // Slicing via subscripting with a range.
  subscript (rng : IntEnumeratorType) -> Slice<T> {
  get:
    debugTrap(rng.min <= length && rng.max <= length)
    return Slice<T>(base + rng.min, rng.max - rng.min, owner)
  set:
    debugTrap(value.length == rng.max - rng.min)

    // Common case: the elements were updated in place, so we do not have to
    // perform any updates.
    var destStart = base + rng.min
    if value.base == destStart {
      return
    }

    // If the start of the destination slice falls inside the source slice,
    // copy backwards.
    if destStart >= value.base {
      if destStart < value.base + value.length {
        var destEnd = destStart + value.length
        for i in value {
          --destEnd
          destEnd.set(i)
        }
        return
      }
    }

    // Copy the data.
    for i in value {
      destStart.set(i)
      ++destStart
    }
  }

  typealias EnumeratorType = Slice<T>
  func getEnumeratorType() -> Slice<T> { return this }

  // FIXME: replPrint doesn't work because T doesn't conform to an
  // appropriate protocol, and we have no way to check it dynamically.
/*
  func replPrint() {
    print('[')
    var first = true
    var total = 0
    for i in this {
      if first {
        first = false
      } else {
        print(", ")
      }
      i.replPrint()
      total = total + 1
      if total > 50 {
        print(" ...]")
        return
      }
    }
    print(']')
  }
*/

  // FIXME: Move to Algorithm.swift as a free function.
  // Blocked on rdar://13251236
  func each(f : (T) -> Void) {
    for i in this { f(i) }
  }

  // FIXME: Move to Algorithm.swift as a free function.
  // Blocked on rdar://13251236
  func reduce(val : T, f : (T, T) -> T) -> T {
    for i in this { val = f(val, i) }
    return val
  }

  func copy() -> T[] {
    var result = new T[length]
    for i in 0..length { result[i] = this[i] }
    return result
  }

  func isEmpty() ->Bool { return length == 0 }
  func next() -> T {
    var prev = base
    base = base + 1
    length = length - 1
    return prev.get()
  }

  func sort(pred : (T, T) -> Bool) {
    for i in 0..length {
      for j in i+1..length {
        if pred(this[j], this[i]) {
          // FIXME: Use swap()
          // BLOCKED: <rdar://problem/12782554> write-back properties do not work
          var temp = this[i]
          this[i] = this[j]
          this[j] = temp
        }
      }
    }
  }
}

