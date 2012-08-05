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
