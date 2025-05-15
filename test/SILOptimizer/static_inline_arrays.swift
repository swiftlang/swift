// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -O -disable-availability-checking -module-name=test -emit-sil | %FileCheck %s

// Also do an end-to-end test to check all components, including IRGen.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -parse-as-library -O -Xfrontend -disable-availability-checking -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test,optimized_stdlib
// UNSUPPORTED: back_deployment_runtime

extension InlineArray: Collection {
  var asArray: Array<Element> { map { $0 } }
}

struct IntByte {
  let i: Int
  let b: Int8
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
  }
}

