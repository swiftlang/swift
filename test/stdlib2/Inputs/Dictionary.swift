struct DictionaryItem {
  var key : String
  var value : Int
}

class Dictionary : Enumerable {
  var keys   : String[]
  var values : Int[]
  var taken  : UInt8[]
  var size   : Int

  struct EnumeratorType : Enumerator {
    var keys   : String[]
    var values : Int[]
    var taken  : UInt8[]
    var size   : Int
    var count  : Int

    typealias Element = (key : String, value : Int)

    func isEmpty() -> Bool {
       return size == 0 || count == keys.length
    }

    func next() -> Element {
      while count != keys.length && taken[count] == 0 {
        ++count
      }
      var e : Element = (keys[count], values[count])
      ++count
      while count != keys.length && taken[count] == 0 {
        ++count
      }
      return e
    }
  }

  constructor(bc : Int = 127) {
    keys = new String[bc]
    values = new Int[bc]
    taken = new UInt8[bc]
  }

  func getBucketCount() -> Int {
    return keys.length
  }

  func add(k : String, v : Int) -> Bool {
    var h : UInt = k.hash()
    var index : Int = Int(h % UInt(keys.length))
    var count = 0
    while (taken[index] == 1 && keys[index] != k)
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
    keys[index] = k
    values[index] = v
    if taken[index] == 0 {
      ++size
    }
    taken[index] = 1
    return true
  }

  func find(k : String) -> (Bool, Int) {
    var h : UInt = k.hash()
    var index : Int = Int(h % UInt(keys.length))
    var defaultValue : Int
    var count = 0
    while (taken[index] == 0 || keys[index] != k)
    {
      ++index
      if index == taken.length {
        index = 0
      }
      ++count
      if count == taken.length {
        return (false, defaultValue)
      }
    }
    return (true, values[index])
  }

  subscript (k : String) -> Int {
    get {
      var defaultValue : Int
      var t : (found : Bool, v : Int) = find(k)
      if t.found {
        return t.v
      }
      add(k, defaultValue)
      return defaultValue
    }

    set {
      add(k, value)
    }
  }

  func itemsAsArray() -> DictionaryItem[] {
    var result = new DictionaryItem[size]
    var index = 0
    for i in 0..getBucketCount() {
      if taken[i] == 0 {
        continue
      }
      result[index] = DictionaryItem(keys[i], values[i])
      ++index
    }
    return result
  }

  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(keys, values, taken, size, 0)
  }
}
