// RUN: %swift -i %s
func testFirstLast(c: Char) {

  func highBit(codePointValue: UInt32) -> Int {
    var v  = codePointValue
    var hiBit = 21
    do {
      if (v & 0x00100000) != 0 { break }
      v <<= 1
    }
    while --hiBit > 0
    // Make sure I've done the computation right
    assert((UInt32(1 << (hiBit-1)) & codePointValue) != 0)
    assert((~(UInt32(1 << hiBit)-1) & codePointValue) == 0)
    return hiBit
  }

  // Separate statements here are a workaround for 
  // <rdar://problem/13756016> hella slow parsing
  print("testing '\(c)'"); 
  print(" (u+\(UInt32(c), radix:16)),"); 
  println(" \(highBit(UInt32(c))) bits")

  var cp0 = CodePoints(c + "elmer")
  var cp1 = CodePoints("bugs" + c)
  assert(cp0.first() == c)
  assert(cp1.last() == c)
  assert(cp0.dropFirst() == "elmer")
  assert(cp1.dropLast() == "bugs")
}

testFirstLast('\u006f')
testFirstLast('\u00A9')
testFirstLast('\u0521')
testFirstLast('\u0ED2')
testFirstLast('\uB977')
testFirstLast('\U0001D452')
testFirstLast('\U0010B9C4')
println("testing isEmpty()...")
assert(!CodePoints("daffy").isEmpty())
assert(CodePoints("box").dropLast().dropFirst().dropLast().isEmpty())

func testStartsEndsWith() {
  println("testing startsWith()...")
  var source = CodePoints("b\u4587u\u0122n\uBF01n\U0001E825y")
  assert(source.startsWith(CodePoints("")))

  var prefix_feed = source
  var prefix = ""
  while !prefix_feed.isEmpty() {
    prefix += prefix_feed.first()
    prefix_feed.inplace_dropFirst()
    assert(source.startsWith(CodePoints(prefix)))
  }
  assert(!source.startsWith(CodePoints("duck season!")))
  assert(!source.startsWith(CodePoints("duck")))

  println("testing endsWith()...")
  var suffix_feed = source
  var suffix = ""
  while !suffix_feed.isEmpty() {
    suffix = suffix_feed.last() + suffix
    suffix_feed.inplace_dropLast()
    assert(source.endsWith(CodePoints(suffix)))
  }
  assert(!source.endsWith(CodePoints("wabbit season!")))
  assert(!source.endsWith(CodePoints("wabbit")))
}

testStartsEndsWith()

func println(x: Vector<CodePoints>) {
  print("[ ")
  var prefix=""
  for s in x {
    print(prefix)
    print("\"")
    print(s)
    print("\"")
    prefix = ", "
  }
  println(" ]")
}

func testSplit() {
  println("testing split()...")
  var qbf = CodePoints("The quick\nbrown fox").split()
  println(qbf)
  assert(qbf.length == 4)
  assert(qbf[0] == "The")
  assert(qbf[1] == "quick")
  assert(qbf[2] == "brown")
  assert(qbf[3] == "fox")

  println("testing split(separator)...")
  qbf = CodePoints("The quick\nbrown fox").split(' ')
  println(qbf)
  assert(qbf.length == 3)
  assert(qbf[0] == "The")
  assert(qbf[1] == "quick\nbrown")
  assert(qbf[2] == "fox")
  

  println("testing split(maxSplit)...")
  qbf = CodePoints("The quick\nbrown fox").split(maxSplit:2)
  println(qbf)
  assert(qbf.length == 3)
  assert(qbf[0] == "The")
  assert(qbf[1] == "quick")
  assert(qbf[2] == "brown fox")
  
  println("testing split(separator, maxSplit)...")
  qbf = CodePoints("The quick\nbrown fox").split(' ', maxSplit:1)
  println(qbf)
  assert(qbf.length == 2)
  assert(qbf[0] == "The")
  assert(qbf[1] == "quick\nbrown fox")

  var sparse = CodePoints("  x  y  ").split()
  println(sparse)
  assert(sparse.length == 2)
  assert(sparse[0] == "x")
  assert(sparse[1] == "y")

  //                   01 23 45
  sparse = CodePoints("  x  y  ").split(' ')
  println(sparse)
  assert(sparse.length == 7)
  assert(sparse[0] == "")
  assert(sparse[1] == "")
  assert(sparse[2] == "x")
  assert(sparse[3] == "")
  assert(sparse[4] == "y")
  assert(sparse[5] == "")
  assert(sparse[6] == "")
}
testSplit()

println("done.")
