// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// CHECK-NOT: Reallocations exceeded 30
func testReallocation() {
  let x = "The quick brown fox jumped over the lazy dog\n"._split(separator: " ")

  var story = "Let me tell you a story:"
  let laps = 1000

  var reallocations = 0
  for i in 0..<laps {
    print("lap \(i)")
    for s in x {
      let lastBase: ObjectIdentifier?
        = story.content._latin1.map(ObjectIdentifier.init)
      story += " "
      story += s
      let newBase = story.content._latin1.map(ObjectIdentifier.init)
      if lastBase != newBase  {
        reallocations += 1
        
        print(
          "reallocation \(reallocations), with utf16 "
          + "count \(story.content.utf16.count)")
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
