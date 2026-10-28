// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

// https://github.com/swiftlang/swift/issues/92980
// rdar://189288334
//
// In non-OSSA, TempRValueElimination replaced the `p` copy with its source
// `w`, although `w` is destroyed between the `load` of `p.token` (for the
// guard) and the `strong_retain` of the loaded reference (for the return).
// `extract` then returned a reference that had already been released.
//
// `Payload` must be address-only, so the copy is still a `copy_addr` when the
// late, non-OSSA run of the pass sees it. An `Any` field does that here; a
// stored property of a resilient type from another module does the same.

var deinitCount = 0

final class Token {
  let id: Int
  init(id: Int) { self.id = id }
  deinit { deinitCount += 1 }
}

struct Payload {
  var token: Token
  var extra: Any
}

@inline(never)
func extract(_ make: () -> Payload?) -> Token? {
  let w = make()
  guard let p = w, p.token.id != 0 else { return nil }
  return p.token
}

let token = extract { Payload(token: Token(id: 42), extra: 0) }

// CHECK: id=42 deinits=0
print("id=\(token!.id) deinits=\(deinitCount)")
