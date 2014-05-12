// RUN: %target-run-simple-swift | FileCheck %s

//===---
// Utilities.
//===---

@asmname("random") func random() -> UInt32
@asmname("srandomdev") func srandomdev()

// String fragments that occupy a variety of bits in UTF-8
var fragments = [
  "o", "©", "ԡ", "໒", "륷", "𝑒", "􋧄", "e", "䖇", "Ģ", "뼁", "𞠥"
]

func randomString(minSize: Int, maxSize: Int) -> String {
    var n = Int(random()) % (maxSize - minSize) + minSize
    var result = ""
    for i in 0...n {
      result += fragments[Int(random()) % fragments.count]
    }
    return result
}

//===---
// Tests.
//===---

func testCharacterSize() {
  // FIXME: should be 8.
  // <rdar://problem/16754935> sizeof(Character.self) is 9, should be 8
  println(sizeof(Character.self)) // CHECK: {{^[89]$}}

  var a: Character = "a"
  println(sizeofValue(a)) // CHECK: {{^[89]$}}
}
testCharacterSize()

func testCharacterEquality() {
  // Single Unicode Scalar Value tests
  for i in indices(fragments) {
    for j in indices(fragments) {
      if (i == j) != (fragments[i] == fragments[j]) {
        println("Equality error: comparing \(i) and \(j)")
        return
      }
      if (i != j) != (fragments[i] != fragments[j]) {
        println("Inequality error: comparing \(i) and \(j)")
        return
      }
    }
  }

  // CHECK: testCharacterEquality done.
  println("testCharacterEquality done.")
}
testCharacterEquality()

/// \brief test that s can be transformed into a Character and back
/// without loss of information.  If it can't, print a message showing
/// the problematic data and return true.  Return false otherwise.
func roundTripThroughCharacterFails(s: String) -> Bool {
  var c = Character(s)
  var s2 = String(c)
  if (s != s2) {
     println("Round-tripping error: \"\(s)\" != \"\(s2)\"")
     return true
  }
  return false
}

func expectedRepresentationFails(s: String, expectSmall: Bool) -> Bool {
  var isSmall: Bool

  switch(Character(s)) {
    case .SmallRepresentation:
      isSmall = true
    default:
      isSmall = false
  }
  
  if isSmall != expectSmall {
    var expectedSize = expectSmall ? "small" : "large"
    println("Error: Expected \"\(s)\" to use the \(expectedSize) representation!")
    return true
  }
  return false
}

func testStringRoundTripping() {
   // CHECK: testStringRoundTripping
   println("testStringRoundTripping")

   // Single Unicode Scalar Value tests
   for s in fragments {
     if roundTripThroughCharacterFails(s) { 
       return
     }
   }

   // Edge case tests 
   // 
   // FIXME: Can't use Bool for 2nd elements here pending
   // <rdar://problem/15136048> (different types for TupleElementExpr
   // and the corresponding tuple element)
   for sExpectSmall in [ 
     ("0123456", 1),
     ("012345\u00A9", 1),
     ("01234567", 0),
     ("0123456\u00A9", 0)
   ] {
     if (expectedRepresentationFails(sExpectSmall.0, sExpectSmall.1 != 0)
     || roundTripThroughCharacterFails(sExpectSmall.0)
     ) {
       return
     }
   }

   // Random tests
   // Seed the random number generator
   srandomdev()
   for x in 0...500 {
     // Character's small representation variant has 63 bits. Making
     // the maximum length 9 fragments tests both sides of the limit.
     var s = randomString(1,9)
     if roundTripThroughCharacterFails(s) {
       return
     }
   }

   // Final CHECK causes any error printed by
   // roundTripThroughCharacterFails to be reported by FileCheck if we
   // return early.

   // CHECK: Done.
   println("Done.")
}

testStringRoundTripping()
