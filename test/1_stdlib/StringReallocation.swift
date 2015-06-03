// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// CHECK-NOT: Reallocations exceeded 30
func testReallocation() {
  var x = "The quick brown fox jumped over the lazy dog\n"._split(" ")

  var story = "Let me tell you a story:"
  var laps = 1000

  var reallocations = 0
  for i in 0..<laps {
    for s in x {
      var lastBase = story._core._baseAddress
      story += " "
      story += s
      if lastBase != story._core._baseAddress {
        ++reallocations
        
        // To avoid dumping a vast string here, just write the first
        // part of the story out each time there's a reallocation.
        var intro = story._split(":")[0]
        print("reallocation \(reallocations), with intro \(intro)")
        
        if reallocations >= 30 {
          print("Reallocations exceeded 30")
          return
        }
      }
    }
    story += "."
  }
  print("total reallocations = \(reallocations)")
}

testReallocation()
print("done!")
