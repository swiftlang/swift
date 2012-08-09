struct DictionaryStringInt : Enumerable {
  var strings : String[]
  var ints    : Int[]
  var taken   : UInt8[]
  var size    : Int

  func setBucketCount(bc : Int) {
    strings = new String[bc]
    ints = new Int[bc]
    taken = new UInt8[bc]
  }

  func getBucketCount() -> Int {
    return strings.length
  }

  func add(s : String, i : Int) -> Bool {
    var h : UInt = s.hash()
    var index : Int = Int(h % UInt(strings.length))
    var count = 0
    while (taken[index] == 1 && strings[index] != s)
    {
      ++index
      if index == taken.length {
        index = 0
      }
      ++count
      if count == taken.length {
        return false
      }
    }
    strings[index] = s
    ints[index] = i
    if taken[index] == 0 {
      ++size
    }
    taken[index] = 1
    return true
  }

  func find(s : String) -> (Bool, Int) {
    var h : UInt = s.hash()
    var index : Int = Int(h % UInt(strings.length))
    var count = 0
    while (taken[index] == 0 || strings[index] != s)
    {
      ++index
      if index == taken.length {
        index = 0
      }
      ++count
      if count == taken.length {
        return (false, 0)
      }
    }
    return (true, ints[index])
  }

  subscript (s : String) -> Int {
    get {
      var t : (found : Bool, i : Int) = find(s)
      if t.found {
        return t.i
      }
      add(s, 0)
      return 0
    }

    set {
      add(s, value)
    }
  }

  // Enumerable
 
  typealias Elements = DictionaryStringIntRange
 
  func getElements() -> Elements {
    return DictionaryStringIntRange(this)
  }
}

struct DictionaryStringIntRange : Range {
  var dict : DictionaryStringInt
  var count : Int

  constructor(v : DictionaryStringInt) {
    dict = v
  }

  typealias Element = (key : String, value : Int)

  func isEmpty() -> Bool {
     return dict.size == 0 || count == dict.getBucketCount()
  }

  func getFirstAndAdvance() -> Element {
     while count != dict.getBucketCount() && dict.taken[count] == 0 {
       ++count
     }
     var e : Element
     e = (dict.strings[count], dict.ints[count])
     ++count
     while count != dict.getBucketCount() && dict.taken[count] == 0 {
       ++count
     }
     return e
  }
}

// FIXME: Define an item type because we can't create arrays of tuples.
struct DictionaryStringIntItem {
  var key : String
  var value : Int
}

extension DictionaryStringInt {
  func itemsAsArray() -> DictionaryStringIntItem[] {
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
