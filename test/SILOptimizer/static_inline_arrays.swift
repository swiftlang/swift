// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -O -disable-availability-checking -module-name=test -emit-sil | %FileCheck %s

// Also do an end-to-end test to check all components, including IRGen.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -parse-as-library -O -Xfrontend -disable-availability-checking -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test,optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc
// UNSUPPORTED: back_deployment_runtime

extension InlineArray: Collection {
  var asArray: Array<Element> { map { $0 } }
}

struct IntByte {
  let i: Int
  let b: Int8
}

public struct ByteAndLargeArray {
  let a: Int
  let b: InlineArray<128, Int>
}

public struct ByteAndSmallArray {
  let a: Int
  let b: InlineArray<3, Int>
}

struct IntByteAndByte {
  let a: InlineArray<3, IntByte>
  let x: Int8
}

struct S {
  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV6simples11InlineArrayVy$2_SiGvpZ : $InlineArray<3, Int> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         %initval = struct $InlineArray<3, Int> ([[V]])
  // CHECK:       }
  static let simple: InlineArray = [1, 2, 3]

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV12optionalIntss11InlineArrayVy$2_SiSgGvpZ : $InlineArray<3, Optional<Int>> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         %initval = struct $InlineArray<3, Optional<Int>> ([[V]])
  // CHECK:       }
  static let optionalInts: InlineArray<_, Int?> = [10, 20, 30]

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV13optionalArrays06InlineC0Vy$2_SiGSgvpZ : $Optional<InlineArray<3, Int>> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         [[A:%.*]] = struct $InlineArray<3, Int> ([[V]])
  // CHECK:         %initval = enum $Optional<InlineArray<3, Int>>, #Optional.some!enumelt, [[A]]
  // CHECK:       }
  static let optionalArray: InlineArray? = [1, 2, 3]

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV12intBytePairss11InlineArrayVy$2_AA03IntC0VGvpZ : $InlineArray<3, IntByte> = {
  // CHECK:         [[S0:%.*]] = struct $IntByte
  // CHECK:         [[S1:%.*]] = struct $IntByte
  // CHECK:         [[S2:%.*]] = struct $IntByte
  // CHECK:         [[V:%.*]] = vector ([[S0]], [[S1]], [[S2]])
  // CHECK:         %initval = struct $InlineArray<3, IntByte> ([[V]])
  // CHECK:       }
  static let intBytePairs: InlineArray<_, IntByte> = [IntByte(i: 1, b: 2), IntByte(i: 3, b: 4), IntByte(i: 5, b: 6)]

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV26optionalInlineArrayOfPairss0cD0Vy$2_AA7IntByteVGSgvpZ : $Optional<InlineArray<3, IntByte>> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         [[A:%.*]] = struct $InlineArray<3, IntByte> ([[V]])
  // CHECK:         %initval = enum $Optional<InlineArray<3, IntByte>>, #Optional.some!enumelt, [[A]]
  // CHECK:       }
  static let optionalInlineArrayOfPairs: InlineArray<_, IntByte>? = [IntByte(i: 11, b: 12), IntByte(i: 13, b: 14), IntByte(i: 15, b: 16)]

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV6tupless11InlineArrayVy$2_Si_SitGvpZ : $InlineArray<3, (Int, Int)> = {
  // CHECK:         [[T0:%.*]] = tuple
  // CHECK:         [[T1:%.*]] = tuple
  // CHECK:         [[T2:%.*]] = tuple
  // CHECK:         [[V:%.*]] = vector ([[T0]], [[T1]], [[T2]])
  // CHECK:         %initval = struct $InlineArray<3, (Int, Int)> ([[V]])
  // CHECK:       }
  static let tuples: InlineArray = [(10, 20), (30, 40), (50, 60)]

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV6nesteds11InlineArrayVy$2_AFy$1_SiGGvpZ : $InlineArray<3, InlineArray<2, Int>> = {
  // CHECK:         [[V0:%.*]] = vector
  // CHECK:         [[A0:%.*]] = struct $InlineArray<2, Int> ([[V0]])
  // CHECK:         [[V1:%.*]] = vector
  // CHECK:         [[A1:%.*]] = struct $InlineArray<2, Int> ([[V1]])
  // CHECK:         [[V2:%.*]] = vector
  // CHECK:         [[A2:%.*]] = struct $InlineArray<2, Int> ([[V2]])
  // CHECK:         [[V:%.*]] = vector ([[A0]], [[A1]], [[A2]])
  // CHECK:         %initval = struct $InlineArray<3, InlineArray<2, Int>> ([[V]])
  // CHECK:       }
  static let nested: InlineArray<3, InlineArray<2, Int>> = [[100, 200], [300, 400], [500, 600]]

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV010intByteAndC0AA03IntcdC0VvpZ : $IntByteAndByte = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         [[A:%.*]] = struct $InlineArray<3, IntByte> ([[V]])
  // CHECK:         %initval = struct $IntByteAndByte ([[A]],
  // CHECK:       }
  static let intByteAndByte = IntByteAndByte(a: [IntByte(i: 1, b: 2), IntByte(i: 3, b: 4), IntByte(i: 5, b: 6)], x: 27)

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV14smallRepeatings11InlineArrayVy$4_SiGvpZ : $InlineArray<5, Int> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         %initval = struct $InlineArray<5, Int> ([[V]])
  // CHECK:       }
  static let smallRepeating: InlineArray<5, Int> = .init(repeating: 26)

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV14largeRepeatings11InlineArrayVy$127_SiGvpZ : $InlineArray<128, Int> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         %initval = struct $InlineArray<128, Int> ([[V]])
  // CHECK:       }
  static let largeRepeating: InlineArray<128, Int> = .init(repeating: 28)

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV20mediumRepeatingTuples11InlineArrayVy$31_Si_SbtGvpZ : $InlineArray<32, (Int, Bool)> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         %initval = struct $InlineArray<32, (Int, Bool)> ([[V]])
  // CHECK:       }
  static let mediumRepeatingTuple: InlineArray<32, (Int, Bool)> = .init(repeating: (29, true))

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV22inStructRepeatingSmallAA07ByteAndE5ArrayVvpZ : $ByteAndSmallArray = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         [[A:%.*]] = struct $InlineArray<3, Int> ([[V]])
  // CHECK:         %initval = struct $ByteAndSmallArray ({{%[0-9]+}}, [[A]])
  // CHECK:       }
  static let inStructRepeatingSmall = ByteAndSmallArray(a: 31, b: [1, 2, 3])

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV22inStructRepeatingLargeAA07ByteAndE5ArrayVvpZ : $ByteAndLargeArray = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         [[A:%.*]] = struct $InlineArray<128, Int> ([[V]])
  // CHECK:         %initval = struct $ByteAndLargeArray ({{%[0-9]+}}, [[A]])
  // CHECK:       }
  static let inStructRepeatingLarge = ByteAndLargeArray(a: 32, b: InlineArray(repeating: 35))

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV15nestedRepeatings11InlineArrayVy$1_AFy$2_SiGGvpZ : $InlineArray<2, InlineArray<3, Int>> = {
  // CHECK:         vector
  // CHECK:         vector
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         %initval = struct $InlineArray<2, InlineArray<3, Int>> ([[V]])
  // CHECK:       }
  static let nestedRepeating: InlineArray<2, InlineArray<3, Int>> = .init(repeating: .init(repeating: 33))

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV20nestedLargeRepeatings11InlineArrayVy$39_AFy$49_SiGGvpZ : $InlineArray<40, InlineArray<50, Int>> = {
  // CHECK:         %initval = struct $InlineArray<40, InlineArray<50, Int>>
  // CHECK:       }
  static let nestedLargeRepeating: InlineArray<40, InlineArray<50, Int>> = .init(repeating: .init(repeating: 34))

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV22optionalRepeatingSmalls11InlineArrayVy$4_SiGSgvpZ : $Optional<InlineArray<5, Int>> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         [[A:%.*]] = struct $InlineArray<5, Int> ([[V]])
  // CHECK:         %initval = enum $Optional<InlineArray<5, Int>>, #Optional.some!enumelt, [[A]]
  // CHECK:       }
  static let optionalRepeatingSmall: InlineArray<5, Int>? = .init(repeating: 36)

  // CHECK-LABEL: sil_global hidden [let] @$s4test1SV22optionalRepeatingLarges11InlineArrayVy$63_SiGSgvpZ : $Optional<InlineArray<64, Int>> = {
  // CHECK:         [[V:%.*]] = vector
  // CHECK:         [[A:%.*]] = struct $InlineArray<64, Int> ([[V]])
  // CHECK:         %initval = enum $Optional<InlineArray<64, Int>>, #Optional.some!enumelt, [[A]]
  // CHECK:       }
  static let optionalRepeatingLarge: InlineArray<64, Int>? = .init(repeating: 37)
}

@main
struct Main {
  static func main() {
    // CHECK-OUTPUT: simple: [1, 2, 3]
    print("simple: \(S.simple.asArray)")

    // CHECK-OUTPUT: optionalInts: [Optional(10), Optional(20), Optional(30)]
    print("optionalInts: \(S.optionalInts.asArray)")

    // CHECK-OUTPUT: optionalArray: [1, 2, 3]
    print("optionalArray: \(S.optionalArray!.asArray)")

    // CHECK-OUTPUT: intBytePairs: [test.IntByte(i: 1, b: 2), test.IntByte(i: 3, b: 4), test.IntByte(i: 5, b: 6)]
    print("intBytePairs: \(S.intBytePairs.asArray)")

    // CHECK-OUTPUT: optionalInlineArrayOfPairs: [test.IntByte(i: 11, b: 12), test.IntByte(i: 13, b: 14), test.IntByte(i: 15, b: 16)]
    print("optionalInlineArrayOfPairs: \(S.optionalInlineArrayOfPairs!.asArray)")

    // CHECK-OUTPUT: tuples: [(10, 20), (30, 40), (50, 60)]
    print("tuples: \(S.tuples.asArray)")

    // CHECK-OUTPUT: nested: {{\[\[}}100, 200], [300, 400], [500, 600{{\]\]}}
    print("nested: \(S.nested.map { $0.asArray })")

    // CHECK-OUTPUT: intByteAndByte: a: [test.IntByte(i: 1, b: 2), test.IntByte(i: 3, b: 4), test.IntByte(i: 5, b: 6)], x: 27
    print("intByteAndByte: a: \(S.intByteAndByte.a.asArray), x: \(S.intByteAndByte.x)")

    // CHECK-OUTPUT: smallRepeating: [26, 26, 26, 26, 26]
    print("smallRepeating: \(S.smallRepeating.asArray)")

    // CHECK-OUTPUT: largeRepeating: [{{(28, ){127}28}}]
    print("largeRepeating: \(S.largeRepeating.asArray)")

    // CHECK-OUTPUT: mediumRepeatingTuple: [{{(\(29, true\), ){31}\(29, true\)}}]
    print("mediumRepeatingTuple: \(S.mediumRepeatingTuple.asArray)")

    // CHECK-OUTPUT: inStructRepeatingSmall:  a: 31, b: [1, 2, 3]
    print("inStructRepeatingSmall:  a: \(S.inStructRepeatingSmall.a), b: \(S.inStructRepeatingSmall.b.asArray)")

    // CHECK-OUTPUT: inStructRepeatingLarge: a: 32, b: [{{(35, ){127}35}}]
    print("inStructRepeatingLarge: a: \(S.inStructRepeatingLarge.a), b: \(S.inStructRepeatingLarge.b.asArray)")

    // CHECK-OUTPUT: nestedRepeating: {{\[\[}}33, 33, 33], [33, 33, 33{{\]\]}}
    print("nestedRepeating: \(S.nestedRepeating.map { $0.asArray })")

    // CHECK-OUTPUT: nestedLargeRepeating: [{{(\[(34, ){49}34\], ){39}\[(34, ){49}34\]}}]
    print("nestedLargeRepeating: \(S.nestedLargeRepeating.map { $0.asArray })")

    // CHECK-OUTPUT: optionalRepeatingSmall: [36, 36, 36, 36, 36]
    print("optionalRepeatingSmall: \(S.optionalRepeatingSmall!.asArray)")

    // CHECK-OUTPUT: optionalRepeatingLarge: [{{(37, ){63}37}}]
    print("optionalRepeatingLarge: \(S.optionalRepeatingLarge!.asArray)")
  }
}

