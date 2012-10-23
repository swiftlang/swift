//===----------------------------------------------------------------------===//
// Swift Algorithm Library.
//===----------------------------------------------------------------------===//
func minElement<R : Range requires R.Element : Ordered >(range : R) 
       -> R.Element {
  var result = range.getFirstAndAdvance()
  while !range.isEmpty() {
    var next = range.getFirstAndAdvance()
    if next < result { result = next }
  }
  return result
}

func maxElement<R : Range requires R.Element : Ordered >(range : R) 
       -> R.Element {
  var result = range.getFirstAndAdvance()
  while !range.isEmpty() {
    var next = range.getFirstAndAdvance()
    if next > result { result = next }
  }
  return result
}

func find<T : Equality>(array : T[], value : T) -> Int {
  var idx = 0
  for elt in array {
    if elt == value { return idx }
    ++idx
  }
  return -1
}

func sort<T : Ordered>(array : T[]) -> T[] {
  var result = array.copy()
  for i in 0..result.length {
    for j in i+1..result.length {
      if result[j] < result[i] {
        // FIXME: Use swap()
        var temp = result[i]
        result[i] = result[j]
        result[j] = temp
      }
    }
  }
  return result
}

 func sort(array : String[], pred : (String, String) -> Bool) -> String[] {
  var result = array.copy()
  for i in 0..result.length {
    for j in i+1..result.length {
      if pred(result[j], result[i]) {
        // FIXME: Use swap()
        var temp = result[i]
        result[i] = result[j]
        result[j] = temp
      }
    }
  }
  return result
}

func map(array : String[], fn : (String) -> String) -> String[] {
  var result = new String[array.length]
  for i in 0..array.length {
    result[i] = fn(array[i])
  }
  return result
}

func map(array : Int[], fn : (Int) -> Int) -> Int[] {
  var result = new Int[array.length]
  for i in 0..array.length {
    result[i] = fn(array[i])
  }
  return result
}

func sort(array : DictionaryStringIntItem[],
          pred : (DictionaryStringIntItem, DictionaryStringIntItem) -> Bool) 
       -> DictionaryStringIntItem[] {
  var result = array.copy()
  for i in 0..result.length {
    for j in i+1..result.length {
      if pred(result[j], result[i]) {
        var temp = result[i]
        result[i] = result[j]
        result[j] = temp
      }
    }
  }
  return result
}

func makeArray(array : String...) -> String[] { return array }
func makeArray(array : Int...) -> Int[] { return array }

