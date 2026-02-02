// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-parse-as-library -enable-experimental-feature Embedded -wmo -enable-builtin-module) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

import Builtin

var NoisyLifeCount = 0
var NoisyDeathCount = 0

protocol P {}

class Noisy : P {
  init() { NoisyLifeCount += 1 }
  deinit { NoisyDeathCount += 1 }
}

struct Large : P {
  var a, b, c, d: Noisy

  init() {
    self.a = Noisy()
    self.b = Noisy()
    self.c = Noisy()
    self.d = Noisy()
  }
}

enum Enum {
  case nontrivial(Noisy)
  case trivial(Int)
}

func exerciseArrayValueWitnesses<T>(_ value: T) {
  let buf = UnsafeMutablePointer<T>.allocate(capacity: 5)

  (buf + 0).initialize(to: value)
  (buf + 1).initialize(to: value)

  Builtin.copyArray(T.self, (buf + 2)._rawValue, buf._rawValue, 2._builtinWordValue)
  Builtin.takeArrayBackToFront(T.self, (buf + 1)._rawValue, buf._rawValue, 4._builtinWordValue)
  Builtin.takeArrayFrontToBack(T.self, buf._rawValue, (buf + 1)._rawValue, 4._builtinWordValue)
  Builtin.destroyArray(T.self, buf._rawValue, 4._builtinWordValue)

  buf.deallocate()
}

func test() {
  NoisyLifeCount = 0
  NoisyDeathCount = 0
  do {
    exerciseArrayValueWitnesses(44)
    exerciseArrayValueWitnesses(Noisy())
    exerciseArrayValueWitnesses(Large())
    exerciseArrayValueWitnesses(Enum.trivial(42))
    exerciseArrayValueWitnesses(Enum.nontrivial(Noisy()))
  }
  precondition(NoisyLifeCount == NoisyDeathCount)
  print("Checks out")
  // CHECK: Checks out
}

@main struct Main { static func main() { test() } }
