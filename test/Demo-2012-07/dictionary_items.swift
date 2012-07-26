// RUN: false
// REQUIRES: disabled

// Define an item type because we can't create arrays of tuples.
struct DictionaryStringIntItem {
  var key : String
  var value : Int
}

extension DictionaryStringInt {
  func getItems() -> DictionaryStringIntItem[] {
    var result = new DictionaryStringIntItem[size]
    var index = 0
    for i in 0..getBucketCount() {
      if (taken[i] == 0) { continue }
      result[index].key = strings[i]
      result[index].value = ints[i]
      ++index
    }
    return result
  }
}

////////

import Builtin
import swift

struct SliceDictionaryStringIntItem : Enumerable {
  var base : UnsafePointerInt,
  length : Int,
  owner : Builtin.ObjectPointer

  static func getElementSize() -> Int {
    return Int(Builtin.sizeof(DictionaryStringIntItem)) / 8
  }

  static func convertFromHeapArray(base : Builtin.RawPointer,
                                   owner : Builtin.ObjectPointer,
                                   length : Builtin.Int64) -> SliceDictionaryStringIntItem {
    return SliceDictionaryStringIntItem(UnsafePointerInt(base), Int(length) & Int64.max(), owner)
  }

  subscript (i : Int) -> DictionaryStringIntItem {
    get {
      if i >= length {
        Builtin.trap()
      }

      var ptr = base + getElementSize() * i
      return Builtin.load(ptr.value)
    }
    set {
      if i >= length {
        Builtin.trap()
      }

      var ptr = base + getElementSize() * i
      Builtin.assign(value, ptr.value)
    }
  }

  typealias Elements = SliceDictionaryStringIntItem
  func getElements() -> SliceDictionaryStringIntItem { return this }
}

extension SliceDictionaryStringIntItem : Range {
  typealias Element = DictionaryStringIntItem

  func isEmpty() -> Bool { return length == 0 }
  func getFirstAndAdvance() -> Element {
    var prev = base
    base = base + getElementSize()
    length = length - 1
    return Builtin.load(prev.value)
  }
}
