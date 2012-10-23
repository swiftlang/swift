class DictionaryStringInt : Enumerable {
  var strings : String[]
  var ints    : Int[]
  var taken   : UInt8[]
  var size    : Int

  constructor(bc : Int = 127) {
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
      result[index] = DictionaryStringIntItem(strings[i], ints[i])
      ++index
    }
    return result
  }
}
