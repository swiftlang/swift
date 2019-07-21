// StringRemoveDupes benchmark
//
// Source: https://gist.github.com/airspeedswift/83eee31e1d9e52fd5570

import TestsUtils

public var StringRemoveDupes = BenchmarkInfo(
  name: "StringRemoveDupes",
  runFunction: run_StringRemoveDupes,
  tags: [.validation, .String]
)

@inline(never)
public func run_StringRemoveDupes(_ N: Int) {
  let textLengthRef = 25
  let text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. " +
             "Morbi nisi metus, accumsan eu sagittis et, condimentum ut " +
             "arcu. Morbi aliquet porta velit, accumsan aliquam ante " +
             "facilisis non. Maecenas lobortis erat vel elit fringilla " +
             "blandit. Praesent euismod mauris sodales velit facilisis " +
             "blandit. Morbi leo neque, finibus ut mi in, auctor sagittis."
  var s = ""

  for _ in 1...10*N {
    s = text.removeDuplicates()

    if s.count != textLengthRef {
      break
    }
  }

  CheckResults(s.count == textLengthRef)
}

// removes all but first occurrence from a sequence, returning an array.
// requires elements to be hashable, not just equatable, but the alternative
// of using contains is very inefficient
// alternatively, could require comparable, sort, and remove adjacent dupes
func uniq<S: Sequence,
          E: Hashable>(_ seq: S) -> [E] where E == S.Element {
  var seen: [S.Element:Int] = [:]
  return seq.filter { seen.updateValue(1, forKey: $0) == nil }
}

// removeDuplicates returns a version of the String with
// all duplicates removed
extension String {
  func removeDuplicates() -> String {
    return String(uniq(self))
  }
}
