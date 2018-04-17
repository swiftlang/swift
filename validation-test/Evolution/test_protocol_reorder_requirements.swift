// RUN: %target-resilience-test --no-backward-deployment
// REQUIRES: executable_test

import StdlibUnittest
import protocol_reorder_requirements


var ProtocolReorderRequirementsTest = TestSuite("ProtocolReorderRequirements")

var log = [String]()

struct MyBassinet : Bed {
  func squiggle() {
    log.append("nap time")
  }
}

struct MyOnesie : Outfit {
  let size = 3
}

struct SillyBaby : Baby {
  func eat() {
    log.append("hangry!")
  }

  func sleep(in bassinet: MyBassinet) {
    bassinet.squiggle()
  }

  func wear(outfit: MyOnesie) {
    log.append("wearing outfit size \(outfit.size)")
  }

  func poop() {
    log.append("change the diaper")
  }

  func cry() {
    log.append("waaaaah!")
  }

  func wiggle() {
    log.append("time to wiggle!")
  }

  let outfitSize = 3
}

func typicalDay<B : Baby>(for baby: B,
                          sleepingIn bed: B.Bassinet,
                          wearing outfit: B.Onesie) {
  baby.wear(outfit: outfit)
  baby.sleep(in: bed)
  baby.cry()
  baby.poop()
  baby.cry()
  baby.sleep(in: bed)
  baby.eat()
  baby.cry()
}

ProtocolReorderRequirementsTest.test("ReorderProtocolRequirements") {
  let baby = SillyBaby()
  let bed = MyBassinet()
  let outfit = MyOnesie()

  typicalDay(for: baby, sleepingIn: bed, wearing: outfit)
  expectEqual(log, [
      "wearing outfit size 3",
      "nap time",
      "waaaaah!",
      "change the diaper",
      "waaaaah!",
      "nap time",
      "hangry!",
      "waaaaah!"
    ])
  log = []

  goodDay(for: baby, sleepingIn: bed, wearing: outfit)
  expectEqual(log, [
      "wearing outfit size 3",
      "nap time",
      "change the diaper",
      "nap time",
      "hangry!",
      "nap time",
      "time to wiggle!"
    ])
}

runAllTests()

