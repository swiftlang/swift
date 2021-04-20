// RUN: %empty-directory(%t)

// The module should be generated regardless of errors and diagnostic should still be output
// RUN: %target-swift-frontend -verify -emit-module -o %t/errors.swiftmodule -emit-reference-dependencies-path %t/errors.swiftdeps -emit-dependencies-path %t/errors.d -experimental-allow-module-with-compiler-errors -D ERROR_MODULE -primary-file %s
// RUN: llvm-bcanalyzer %t/errors.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s
// CHECK-BC-NOT: UnknownCode
// RUN: ls %t/errors.swiftdeps
// RUN: ls %t/errors.d

#if ERROR_MODULE
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

  public func funcBadArg(_ arg: undefined? = nil) {} // expected-error {{cannot find type 'undefined'}}
}

public func validFunc() -> String { "" }

public func invalidFuncBody() -> ValidStructInvalidMember {
  ret // expected-error {{cannot find 'ret'}}
}

public func invalidFunc() -> undefined {} // expected-error {{cannot find type 'undefined'}}

extension undefined: undefined {} // expected-error {{cannot find type 'undefined'}}

class GenericClass<T> {}
class InvalidSuperclass: GenericClass<undefined> {} // expected-error {{cannot find type 'undefined'}}
#endif

// RUN: %target-swift-frontend -emit-module -o %t/validUses.swiftmodule -experimental-allow-module-with-compiler-errors -I%t -D VALID_USES %s 2>&1 | %FileCheck -check-prefix=CHECK-VALID %s
// RUN: llvm-bcanalyzer %t/validUses.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s
#if VALID_USES
import errors
func test(s: ValidStructInvalidMember) {
  print(s.member)
  print(validFunc())
  print(invalidFuncBody())
}

// Check SIL diagnostics are still output (no reason not to output SIL since
// there were no errors)
func other() -> Int {}
// CHECK-VALID: allow-errors.swift:[[@LINE-1]]:22: error: missing return in a function expected to return 'Int'
func other2() -> Bool {}
// CHECK-VALID: allow-errors.swift:[[@LINE-1]]:24: error: missing return in a function expected to return 'Bool'
#endif

// All invalid uses should have no errors in the file itself, all referenced
// invalid declarations should have an error elsewhere (but we don't care what
// that location is)

// RUN: %target-swift-frontend -emit-module -o %t/invalidTopUse.swiftmodule -experimental-allow-module-with-compiler-errors -I%t -D INVALID_TOP_LEVEL_USE %s 2>&1 | %FileCheck -check-prefix=CHECK-INVALID-TOP %s
// RUN: llvm-bcanalyzer %t/invalidTopUse.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s
#if INVALID_TOP_LEVEL_USE
import errors
func test() {
  invalidFunc()
}
// CHECK-INVALID-TOP-NOT: allow-errors.swift:{{.*}} error:
// CHECK-INVALID-TOP: error: allowing deserialization of error type '<null>' in module 'errors'
// CHECK-INVALID-TOP: error: allowing deserialization of invalid declaration 'invalidFunc()' (global function) in module 'errors'
// CHECK-INVALID-TOP-NOT: allow-errors.swift:{{.*}} error:
#endif

// RUN: %target-swift-frontend -emit-module -o %t/invalidMemberUse.swiftmodule -experimental-allow-module-with-compiler-errors -I%t -D INVALID_MEMBER_USE %s 2>&1 | %FileCheck -check-prefix=CHECK-INVALID-MEMBER %s
// RUN: llvm-bcanalyzer %t/invalidMemberUse.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s
#if INVALID_MEMBER_USE
import errors
func test(s: ValidStructInvalidMember) {
  print(s.memberMissingType)
}
// CHECK-INVALID-MEMBER-NOT: allow-errors.swift:{{.*}} error:
// CHECK-INVALID-MEMBER: error: allowing deserialization of error type '<null>' in module 'errors'
// CHECK-INVALID-MEMBER: error: allowing deserialization of invalid declaration '_' (getter) in module 'errors'
// CHECK-INVALID-MEMBER: error: allowing deserialization of invalid declaration 'memberMissingType' (property) in module 'errors'
// CHECK-INVALID-MEMBER-NOT: allow-errors.swift:{{.*}} error:
#endif

// RUN: %target-swift-frontend -emit-module -o %t/invalidMethodUse.swiftmodule -experimental-allow-module-with-compiler-errors -I%t -D INVALID_METHOD_USE %s 2>&1 | %FileCheck -check-prefix=CHECK-INVALID-METHOD %s
// RUN: llvm-bcanalyzer %t/invalidMethodUse.swiftmodule | %FileCheck -check-prefix=CHECK-BC %s
#if INVALID_METHOD_USE
import errors
func test(s: ValidStructInvalidMember) {
  s.funcBadArg()
}
// CHECK-INVALID-METHOD-NOT: allow-errors.swift:{{.*}} error:
// CHECK-INVALID-METHOD: error: allowing deserialization of error type '<null>' in module 'errors'
// CHECK-INVALID-METHOD: error: allowing deserialization of invalid declaration 'arg' (parameter) in module 'errors'
// CHECK-INVALID-METHOD: error: allowing deserialization of invalid declaration 'funcBadArg' (instance method) in module 'errors'
// CHECK-INVALID-METHOD-NOT: allow-errors.swift:{{.*}} error:
#endif
