// RUN: %target-run-simple-swift | FileCheck %s

// CHECK-NOT: Reallocations exceeded 30
func testReallocation() {
  var x = "The quick brown fox jumped over the lazy dog\n".split(" ")

  var story = "Let me tell you a story:"
  var laps = 1000

  var reallocations = 0
  for i in 0..laps {
    for s in x {
      var lastBase = story.core._baseAddress
      story += " "
      story += s
      if lastBase != story.core._baseAddress {
        ++reallocations
        
        // To avoid dumping a vast string here, just write the first
        // part of the story out each time there's a reallocation.
        var intro = story.split(":")[0]
        println("reallocation \(reallocations), with intro \(intro)")
        
        if reallocations >= 30 {
          println("Reallocations exceeded 30")
          return
        }
      }
    }
    story += "."
  }
  println("total reallocations = \(reallocations)")
}

testReallocation()
println("done!")
