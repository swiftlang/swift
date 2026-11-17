// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)

// RUN: touch %t/empty.swift
// RUN: %{python} %utils/split_file.py -o %t %s

// Errors often only occur during merging, hence creating an empty module here
// RUN: %target-swift-frontend -verify -verify-ignore-unrelated -module-name errors -emit-module -o %t/mods/errorsmain.partial.swiftmodule -experimental-allow-module-with-compiler-errors %t/errors.swift
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errorsempty.partial.swiftmodule %t/empty.swift
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errors.swiftmodule -experimental-allow-module-with-compiler-errors %t/mods/errorsmain.partial.swiftmodule %t/mods/errorsempty.partial.swiftmodule

// RUN: %target-swift-frontend -emit-module -o %t/mods/validUses.swiftmodule -experimental-allow-module-with-compiler-errors -I%t/mods %t/valid-uses.swift 2>&1 | %FileCheck -check-prefix=CHECK-VALID %s

// RUN: %target-swift-frontend -emit-module -o %t/mods/invalidUses.swiftmodule -experimental-allow-module-with-compiler-errors -I%t/mods %t/invalid-uses.swift 2>&1 | %FileCheck -check-prefix=CHECK-INVALID %s

// A client that does not allow errors gets a fatal error rather than a crash
// RUN: not %target-swift-frontend -typecheck -I%t/mods %t/no-allow-uses.swift 2>&1 | %FileCheck -check-prefix=CHECK-NOALLOW %s
// RUN: %target-swift-ide-test -print-module -source-filename dummy -module-to-print errors -I %t/mods 2>%t/print.err | %FileCheck -check-prefix=CHECK-NOALLOW-PRINT %s
// RUN: %FileCheck -check-prefix=CHECK-NOALLOW %s < %t/print.err

// BEGIN errors.swift
public struct ValidType {}

public func validFunc() -> ValidType { return ValidType() }

public func invalidFuncBody() -> ValidType {
  ret // expected-error {{cannot find 'ret'}}
}

public func invalidFuncRet() -> undefined {} // expected-error {{cannot find type 'undefined'}}

public func invalidFuncParam(_ p: undefined) {} // expected-error {{cannot find type 'undefined'}}


// BEGIN valid-uses.swift
import errors

func test(s: ValidType) {
  print(validFunc())
  print(invalidFuncBody())
}

// Check SIL diagnostics are still output (no reason not to output SIL since
// there were no errors)
func other() -> Int {}
// CHECK-VALID: valid-uses.swift:10:22: error: missing return
func other2() -> Bool {}
// CHECK-VALID: valid-uses.swift:12:24: error: missing return


// BEGIN invalid-uses.swift
import errors

func test() {
  invalidFuncRet()
}

// All invalid uses should have no errors in the file itself - all referenced
// invalid declarations should have an error elsewhere (but we don't care what
// that location is)
// CHECK-INVALID-NOT: invalid-uses.swift:{{.*}} error:
// CHECK-INVALID: error: allowing deserialization of error type '<null>' in module 'errors'
// CHECK-INVALID: error: allowing deserialization of invalid declaration 'invalidFuncRet()' (global function) in module 'errors'
// CHECK-INVALID-NOT: invalid-uses.swift:{{.*}} error:


// BEGIN no-allow-uses.swift
import errors

func test() {
  invalidFuncParam(1)
}

// CHECK-NOALLOW: error: deserialized error type '<null>' in module 'errors'
// CHECK-NOALLOW-NOT: allowing deserialization

// CHECK-NOALLOW-PRINT: func invalidFuncParam(_ p: <<error type>>)
