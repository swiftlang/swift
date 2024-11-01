// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S/Inputs)
// REQUIRES: executable_test

import StdlibUnittest
import TypedUntypedEnums

// A helper for hashing Hasable values.
func getHash<H>(_ x: H) -> Int where H: Hashable {
    var h = Hasher()
    h.combine(x)
    return h.finalize()
}

let Colors = [kRed, kBlue, kGreen, kYellow]
let Numbers = [kOne, kTwo, kThree, kFour]
let Pets = [Pet.goat, Pet.cat, Pet.dogcow, Pet.rabbit]

var HashableEnumsTestSuite = TestSuite("Enums are hashable")

HashableEnumsTestSuite.test("Hashes preserve equality") {
    for m in 0..<Colors.count {
        for n in 0..<Colors.count {
          if m == n {
              expectEqual(getHash(Colors[m]), getHash(Colors[n]))
          } else {
              expectNotEqual(getHash(Colors[m]), getHash(Colors[n]))
          }
        }
    }

    for m in 0..<Numbers.count {
        for n in 0..<Numbers.count {
          if m == n {
              expectEqual(getHash(Numbers[m]), getHash(Numbers[n]))
          } else {
              expectNotEqual(getHash(Numbers[m]), getHash(Numbers[n]))
          }
        }
    }

    for m in 0..<Pets.count {
        for n in 0..<Pets.count {
          if m == n {
              expectEqual(getHash(Pets[m]), getHash(Pets[n]))
          } else {
              expectNotEqual(getHash(Pets[m]), getHash(Pets[n]))
          }
        }
    }
}

HashableEnumsTestSuite.test("Untyped enums hash using underlying value") {
    for m in 1...4 {
        for n in 1...Numbers.count {
            let number = Numbers[n - 1]
            if m == n {
                expectEqual(getHash(m), getHash(number))
            } else {
                expectNotEqual(getHash(m), getHash(number))
            }
        }
    }
}

HashableEnumsTestSuite.test("Raw values are well-defined") {
    expectEqual(kRed.rawValue, 0)
    expectEqual(kYellow.rawValue, 10)
    expectEqual(Pet.goat.rawValue, 5)
    expectEqual(Pet.cat.rawValue, 15)
}

HashableEnumsTestSuite.test("Sets work as expected") {
    let s: Set = [kRed, kBlue, kRed, kRed]
    assert(s.contains(kRed))
    assert(s.contains(kBlue))
    assert(!s.contains(kGreen))
    assert(s.count == 2) // kRed should have been deduplicated
}

runAllTests()
