// REQUIRES: executable_test
// RUN: %target-run-simple-swiftgyb(-cxx-interoperability-mode=default -Xfrontend -disable-availability-checking -I %S/Inputs)

import StdlibUnittest
import TypedUntypedEnums

// A helper for hashing Hasable values.
func getHash<H>(_ x: H) -> Int where H: Hashable {
    var h = Hasher()
    h.combine(x)
    return h.finalize()
}

%{
Colors = ["kRed", "kBlue", "kGreen", "kYellow"]
Numbers = ["kOne", "kTwo", "kThree", "kFour"]
Pets = ["Pet.goat", "Pet.cat", "Pet.dogcow", "Pet.rabbit"]
}%

var HashableEnumsTestSuite = TestSuite("Enums are hashable")

HashableEnumsTestSuite.test("Hashes preserve equality") {
    % for m in Colors:
    %   for n in Colors:
    %     if m == n:
              expectEqual(getHash(${m}), getHash(${n}))
    %     else:
              expectNotEqual(getHash(${m}), getHash(${n}))
    %     end
    %   end
    % end

    % for m in Pets:
    %   for n in Pets:
    %     if m == n:
              expectEqual(getHash(${m}), getHash(${n}))
    %     else:
              expectNotEqual(getHash(${m}), getHash(${n}))
    %     end
    %   end
    % end

    % for m in Numbers:
    %   for n in Numbers:
    %     if m == n:
              expectEqual(getHash(${m}), getHash(${n}))
    %     else:
              expectNotEqual(getHash(${m}), getHash(${n}))
    %     end
    %   end
    % end
}

HashableEnumsTestSuite.test("Untyped enums hash using underlying value") {
    % for m in range(4):
    %   for n in range(4):
    %     if m == n:
              expectEqual(getHash(${Numbers[m]}), getHash(${n + 1}))
    %     else:
              expectNotEqual(getHash(${Numbers[m]}), getHash(${n + 1}))
    %     end
    %   end
    % end
}

HashableEnumsTestSuite.test("Typed enums and class enums hash using other info") {
    // The raw values of these enum members are known:
    expectEqual(kRed.rawValue, 0)
    expectEqual(kYellow.rawValue, 10)
    expectEqual(Pet.goat.rawValue, 5)
    expectEqual(Pet.cat.rawValue, 15)

    // But the Hashable implementation uses more than the raw value to compute the hash:
    expectNotEqual(getHash(kRed), getHash(0))
    expectNotEqual(getHash(kYellow), getHash(10))
    expectNotEqual(getHash(Pet.goat), getHash(5))
    expectNotEqual(getHash(Pet.cat), getHash(15))
}

HashableEnumsTestSuite.test("Sets work as expected") {
    let s: Set = [kRed, kBlue, kRed, kRed]
    assert(s.contains(kRed))
    assert(s.contains(kBlue))
    assert(!s.contains(kGreen))
    assert(s.count == 2) // kRed should have been deduplicated
}

runAllTests()
