// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

import StdlibUnittest

extension String {
  var bufferID: UInt {
    guard let id = _classify()._objectIdentifier else { return 0 }
    return UInt(bitPattern: id)
  }
}

// CHECK-NOT: Reallocations exceeded 15
func testReallocation() {
  let x = "The quick brown fox jumped over the lazy dog\n"._split(separator: " ")

  var story = "Let me tell you a story:"
  let laps = 1000

  var reallocations = 0
  for _ in 0..<laps {
    for s in x {
      let lastBase = story.bufferID
      story += " "
      story += s
      if lastBase != story.bufferID {
        reallocations += 1
        
        // To avoid dumping a vast string here, just write the first
        // part of the story out each time there's a reallocation.
        var intro = story._split(separator: ":")[0]

        print("""
          reallocation \(reallocations), lastBase 0x\(
            String(lastBase, radix: 16)), bufferID 0x\(
            String(story.bufferID, radix: 16)), intro \(intro)
          """)

        if reallocations >= 15 {
          print("Reallocations exceeded 15")
          //return
        }
      }
    }
    story += "."
  }
  print("total reallocations = \(reallocations)")
}

testReallocation()
print("done!")
