// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

func [asmname="random"] random() -> UInt32
func [asmname="srandomdev"] srandomdev()

// String fragments that occupy a variety of bits in UTF-8
var fragments = [
  "o", "©", "ԡ", "໒", "륷", "𝑒", "􋧄", "e", "䖇", "Ģ", "뼁", "𞠥", 
  "\u007f" // This last one is important because we can't store that at 
]

func randomString(minSize: Int, maxSize: Int) -> String
{
    var n = Int(random()) % (maxSize - minSize) + minSize
    var result = ""
    for i in 0..n {
      result += fragments[Int(random()) % fragments.length]
    }
    return result
}

func testCharacter() {
   // Initial CHECK synchronizes FileCheck scanner
   // CHECK: Testing Character...
   println("Testing Character...")

   // FIXME: declare some constant strings because of
   // <rdar://problem/14050788> String Interpolations can't contain
   // quotes
   var ok = "OK"
   var error = "ERROR"

   // Seed the random number generator
   srandomdev()

   for x in 0..1000 {
     var s = randomString(1,9)
     var c = Character(s)
     var s2 = String(c)
     if (s != s2) {
        println("Round-tripping error: \"\(s)\" != \"\(s2)\"")
        return
     }
   }
   // Final CHECK causes the error above to be reported
   // CHECK: Done.
   println("Done.")
}

testCharacter()
