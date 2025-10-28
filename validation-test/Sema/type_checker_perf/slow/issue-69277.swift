// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000
// This also fails with the default limit.

// REQUIRES: objc_interop

// https://github.com/swiftlang/swift/issues/69277

import Foundation

struct Monster {
    var weaponP1: Int? { nil }
    var weaponP2: Int? { nil }
    var weaponP3: Int? { nil }
}

func slow() {
  // expected-error@+1 {{reasonable time}}
  let predicate = #Predicate<Monster> { monster in
      ((monster.weaponP1 == 1) &&
       (monster.weaponP2 == 2) &&
       (monster.weaponP3 == 3))
  }
}
