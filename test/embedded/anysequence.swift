// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo %target-embedded-posix-shim) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O %target-embedded-posix-shim) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// The vtable of an `AnySequence` box must not create infinitely many box
// specializations through `prefix`, `dropFirst` and `drop(while:)`.

func makeSequence() -> AnySequence<Int> {
  var i = 0
  return AnySequence<Int>({ () -> AnyIterator<Int> in
    i = 0
    return AnyIterator({
      i += 1
      return i <= 10 ? i : nil
    })
  })
}

func printElements(_ s: AnySequence<Int>) {
  for x in s {
    print(x, terminator: " ")
  }
  print("")
}

@main
struct Main {
  static func main() {
    let s = makeSequence()

    // CHECK: 1 2 3 4 5 6 7 8 9 10
    printElements(s)
    // CHECK-NEXT: 1 2 3
    printElements(s.prefix(3))
    // CHECK-NEXT: 8 9 10
    printElements(s.dropFirst(7))
    // CHECK-NEXT: 5 6 7 8 9 10
    printElements(s.drop(while: { $0 < 5 }))
    // CHECK-NEXT: 3 4 5
    printElements(s.dropFirst(1).prefix(6).drop(while: { $0 < 3 }).dropFirst(0).prefix(3))
    // CHECK-NEXT: 3
    printElements(s.prefix(5).dropFirst(2).prefix(1))
    // CHECK-NEXT: done
    print("done")
  }
}
