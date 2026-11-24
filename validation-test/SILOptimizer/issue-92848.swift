// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

// https://github.com/swiftlang/swift/issues/92848
// rdar://189007197
//
// Code motion sank the identical `builder.append("\n")` calls from both
// returns into their common successor, past the loads of `builder` that
// produce the return value. The function then returned the value as it was
// before the final append: a use-after-free once that append grew the array.

struct Builder {
  enum Piece { case text(String) }
  private(set) var pieces: [Piece] = []

  mutating func append(_ text: String) {
    guard !text.isEmpty else { return }
    pieces.append(.text(text))
  }
}

var skipBody = false
var lines: [String] = []

func makeText() -> Builder {
  var builder = Builder()
  builder.append("header")
  builder.append("\n")
  let lines = lines
  guard !skipBody else {
    builder.append("\n")
    return builder
  }
  for line in lines {
    builder.append("\n")
    builder.append(line)
  }
  builder.append("\n")
  return builder
}

for n in 0...6 {
  lines = (0..<n).map { i in "effect line number \(i)" }
  let r = makeText()
  print(n, r.pieces.count)
}
print("done")

// CHECK:      0 3
// CHECK-NEXT: 1 5
// CHECK-NEXT: 2 7
// CHECK-NEXT: 3 9
// CHECK-NEXT: 4 11
// CHECK-NEXT: 5 13
// CHECK-NEXT: 6 15
// CHECK-NEXT: done
