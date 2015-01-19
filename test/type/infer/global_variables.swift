// RUN: %target-parse-verify-swift

var b = true, i = 17

var d : Dictionary = [0 : "Zero", 1 : "One", 2 : "Two" ]

func testGlobals() {
  b = false
  i = 5
  d[3] = "Three"
}
