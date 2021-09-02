// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)

// RUN: touch %t/empty.swift
// RUN: %{python} %utils/split_file.py -o %t %s

// Errors often only occur during merging, hence creating an empty module here
// RUN: %target-swift-frontend -verify -module-name errors -emit-module -o %t/mods/errorsmain.partial.swiftmodule -experimental-allow-module-with-compiler-errors %t/errors.swift
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errorsempty.partial.swiftmodule %t/empty.swift
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errors.swiftmodule -experimental-allow-module-with-compiler-errors %t/mods/errorsmain.partial.swiftmodule %t/mods/errorsempty.partial.swiftmodule

// RUN: %target-swift-frontend -emit-module -o %t/mods/uses.swiftmodule -experimental-allow-module-with-compiler-errors -I %t/mods %t/uses.swift 2>&1 | %FileCheck -check-prefix=CHECK-USES %s

// BEGIN errors.swift
public struct ValidStructInvalidMember {
  public var member: String
  public let memberMissingType: undefined // expected-error {{cannot find type 'undefined'}}

  public var memberMissingTypeValidSets: undefined { // expected-error {{cannot find type 'undefined'}}
    willSet {
      print("Setting value \(newValue)")
    }
    didSet {
      print("Set value \(oldValue)")
    }
  }
  public var memberInvalidSets: Int {
    willSet {
      undefined // expected-error {{cannot find 'undefined'}}
    }
    didSet {
      undefined // expected-error {{cannot find 'undefined'}}
    }
  }

  public lazy var lazyMemberMissingTypeValidBody: undefined = { // expected-error {{cannot find type 'undefined'}}
    return ""
  }()
  public lazy var lazyMemberInvalidBody: String = {
    return undefined // expected-error {{cannot find 'undefined'}}
  }()

  public var memberMissingTypeValidGetSets: String {
    get { member }
    set { member = "" }
  }
  public var memberInvalidGetSet: String {
    get { undefined } // expected-error {{cannot find 'undefined'}}
    set { undefined = "" } // expected-error {{cannot find 'undefined'}}
  }

  public func someFunc() {}
  public func funcBadArg(_ arg: undefined? = nil) {} // expected-error {{cannot find type 'undefined'}}
}


// BEGIN uses.swift
import errors

func test(s: ValidStructInvalidMember) {
  s.someFunc()
    s.BOOOP()
  print(s.member)

  s.funcBadArg()
  print(s.memberMissingType)
}

// CHECK-USES-NOT: has no member 'someFunc'
// CHECK-USES-NOT: has no member 'member'
// CHECK-USES-NOT: has no member 'funcBadArg'
// CHECK-USES-NOT: has no member 'memberMissingType'
