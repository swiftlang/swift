// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/existential_foreign.h -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s --check-prefix=OUTPUT
// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/existential_foreign.h -enable-experimental-feature EmbeddedExistentials -enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s --check-prefix=OUTPUT

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedExistentials

protocol P {
  func printme()
}

protocol Q {
  associatedtype Printable : P

  func getPrintable() -> Printable
}

extension SomeCStruct : P {
  func printme() {
    print("SomeCStruct: \(self.f1), \(self.f2)")
  }
}

extension SomeCEnum : P {
  func printme() {
    switch self {
      case caseA:
        print("SomeCEnum: .caseA")
      case caseB:
        print("SomeCEnum: .caseB")
      case caseC:
        print("SomeCEnum: .caseC")
      default:
        print("SomeCEnum: default")
    }
  }
}

struct SomeCStructContainer : Q {
  let s: SomeCStruct

  init() {
    self.s = createSomeCStruct()
  }

  func getPrintable() -> SomeCStruct {
    return s
  }
}

struct SomeCEnumContainer : Q {
  let s: SomeCEnum

  init() {
    self.s = createSomeCEnum()
  }

  func getPrintable() -> SomeCEnum {
    return s
  }
}

extension SomeNSEnum : P {
  func printme() {
    switch self {
      case .someCaseA:
        print("SomeNSEnum: .someCaseA")
      case .someCaseB:
        print("SomeNSEnum: .someCaseB")
    }
  }
}

struct SomeNSEnumContainer : Q {
  let s: SomeNSEnum

  init() {
    self.s = createSomeNSEnum()
  }

  func getPrintable() -> SomeNSEnum {
    return s
  }
}

@main
struct Main {
  static func main() {

    let a: [any Q] = [ SomeCStructContainer(), SomeCEnumContainer(), SomeNSEnumContainer() ]

    for x0 in a {
      let x = x0.getPrintable()
      x.printme()
      // OUTPUT: SomeCStruct: 1, 2
      // OUTPUT: SomeCEnum: .caseA
      // OUTPUT: SomeNSEnum: .someCaseB
    }
  }
}
