// FIXME: Replace with a tuple having named elements pending
// <rdar://problem/14149760> (Naming tuple elements breaks Slice
// conformance to all protocols)

struct DictionaryEnumerator<Key,Value> : Enumerator {
  typealias Element = (key: Key, value: Value)

  var elements: Optional<Element>[]
  var size: Int
  var count: Int


  func isEmpty() -> Bool {
     return size == 0 || count == elements.length
  }

  func next() -> Element {
    while count != elements.length && elements[count] == None {
      ++count
    }
    var e = Element(elements[count].get())
    ++count
    while count != elements.length && elements[count] == None {
      ++count
    }
    return e
  }
}

class Dictionary<KeyType: Hashable, ValueType> 
  : Object, Enumerable, DictionaryLiteralConvertible
{
  typealias Key = KeyType
  typealias Value = ValueType
  typealias Element = (key: KeyType, value: ValueType)

  var elements: Optional<Element>[]
  var size: Int

  typealias EnumeratorType = DictionaryEnumerator<KeyType,ValueType>

  constructor(bc : Int = 127) {
    elements = new Optional<Element>[bc]
  }

  func getBucketCount() -> Int {
    return elements.length
  }

  func add(k : Key, v : Value) -> Bool {
    var h = k.hashValue()
    var index = Int(UInt(h) % UInt(elements.length))
    var count = 0
    while elements[index] != None && elements[index].get().0 != k
    {
      ++index
      if index == elements.length {
        index = 0
      }
      ++count
      if count == elements.length {
        return false
      }
    }
    if !elements[index] {
      ++size
    }
    elements[index] = Some((k,v))
    return true
  }

  func find(k : Key) -> Optional<Value> {
    var h = k.hashValue()
    var index = Int(UInt(h) % UInt(elements.length))
    var count = 0
    while (!elements[index] || elements[index].get().0 != k)
    {
      ++index
      if index == elements.length {
        index = 0
      }
      ++count
      if count == elements.length {
        return None
      }
    }
    return Some(elements[index].get().1)
  }

  subscript (k : Key) -> Value {
  get:
    return find(k).get()
  set(value):
    add(k, value)
  }
  
  func itemsAsArray() -> Element[] {
    var result = new Element[size]
    var index = 0
    for i in 0..getBucketCount() {
      // FIXME: Can't use destructuring bind pending
      // <rdar://problem/14149760> (Naming tuple elements breaks Slice
      // conformance to all protocols)
      if elements[i] != None {
        var e = elements[i].get()
        result[index++] = Element(e.key, e.value)
      }
    }
    return result
  }

  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(elements, size, 0)
  }

  //
  // DictionaryLiteralConvertible conformance
  //
  static func convertFromDictionaryLiteral(elements:(Key, Value)...) 
                -> Dictionary<Key,Value> {
    var dict = Dictionary<Key,Value>()
    for (k, v) in elements {
      dict.add(k, v)
    }
    return dict
  }

  // FIXME: Disabled pending more capable generics that can make this
  // replPrintable when KeyType and ValueType are
/* 
  func replPrint() {
    print("[")
    var first = true
    for (key, value) in this {
      if first { first = false }
      else { print(", ") }

      print((key, value))
    }
    print("]")
  }
*/
}

func replPrint(x: Dictionary<String, Int>) {
  print("[")
  var prefix = ""
  for o in x.elements {
    // FIXME: Can't use destructuring bind pending
    // <rdar://problem/14149760> (Naming tuple elements breaks Slice
    // conformance to all protocols)
    if o != None {
      var (k,v) = o.get()
      print("\(prefix)\"\(k)\": \(v)")
      prefix = ", "
    }
  }
  print("]")
}

