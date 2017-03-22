// REQUIRES: OS=macosx

// RUN: %target-swift-frontend -DVALUE -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | %FileCheck %s -check-prefix=VALUE
// RUN: %target-swift-frontend -DEMPTYCATCH -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | %FileCheck %s -check-prefix=EMPTYCATCH
// RUN: %target-swift-frontend -DASQEXPR -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | %FileCheck %s -check-prefix=ASQEXPR
// RUN: %target-swift-frontend -DASBANGEXPR -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | %FileCheck %s -check-prefix=ASBANGEXPR
// RUN: %target-swift-frontend -DCATCHIS -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | %FileCheck %s -check-prefix=CATCHIS
// RUN: %target-swift-frontend -DCATCHAS -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | %FileCheck %s -check-prefix=CATCHAS
// RUN: %target-swift-frontend -DGENERICONLY -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | %FileCheck %s -check-prefix=GENERICONLY

// RUN: %target-swift-frontend -emit-sil %s -import-objc-header %S/Inputs/enum-error.h -verify

// RUN: echo '#include "enum-error.h"' > %t.m
// RUN: %target-swift-ide-test -source-filename %s -print-header -header-to-print %S/Inputs/enum-error.h -import-objc-header %S/Inputs/enum-error.h -print-regular-comments --cc-args %target-cc-options -fsyntax-only %t.m -I %S/Inputs > %t.txt
// RUN: %FileCheck -check-prefix=HEADER %s < %t.txt

import Foundation

func testDropCode(other: OtherError) -> OtherError.Code {
  return other.code
}

func testError() {
  let testErrorNSError = NSError(domain: TestErrorDomain,
                                 code: Int(TestError.TENone.rawValue),
                                 userInfo: nil)

// Below are a number of test cases to make sure that various pattern and cast
// expression forms are sufficient in pulling in the _NSBridgedError
// conformance.
#if VALUE
// VALUE: TestError: _BridgedStoredNSError
  let terr = getErr(); terr

#elseif EMPTYCATCH
// EMPTYCATCH: TestError: _BridgedStoredNSError
  do {
    throw TestError(.TENone)
  } catch {}

#elseif ASQEXPR
// ASQEXPR: sil_witness_table shared [fragile] TestError: _BridgedStoredNSError module __ObjC
  let wasTestError = testErrorNSError as? TestError; wasTestError

#elseif ASBANGEXPR
// ASBANGEXPR: sil_witness_table shared [fragile] TestError: _BridgedStoredNSError module __ObjC
  let terr2 = testErrorNSError as! TestError; terr2

#elseif ISEXPR
// ISEXPR: sil_witness_table shared [fragile] TestError: _BridgedStoredNSError module __ObjC
  if (testErrorNSError is TestError) {
    print("true")
  } else {
    print("false")
  }

#elseif CATCHIS
// CATCHIS: sil_witness_table shared [fragile] TestError: _BridgedStoredNSError module __ObjC
  do {
    throw TestError(.TETwo)
  } catch is TestError {
  } catch {}

#elseif CATCHAS
// CATCHAS: sil_witness_table shared [fragile] TestError: _BridgedStoredNSError module __ObjC
  do {
    throw TestError(.TETwo)
  } catch let err as TestError {
    err
  } catch {}

#elseif GENERICONLY
// GENERICONLY: TestError: _BridgedStoredNSError
  func dyncast<T, U>(_ x: T) -> U {
    return x as! U
  }
  let _ : TestError = dyncast(testErrorNSError)

#else
// CHECK: sil_witness_table shared [fragile] TestError: _BridgedStoredNSError module __ObjC
  let terr = getErr()
  switch (terr) { case .TENone, .TEOne, .TETwo: break } // ok

  switch (terr) { case .TENone, .TEOne: break }
    // expected-error@-1 {{switch must be exhaustive, consider adding missing cases}}

  let _ = TestError.Code(rawValue: 2)!

  do {
    throw TestError(.TEOne)
  } catch is TestError {
  } catch {}

#endif

}

// HEADER: struct TestError : _BridgedStoredNSError {
// HEADER:   let _nsError: NSError
// HEADER:   init(_nsError: NSError)
// HEADER:   static var _nsErrorDomain: String { get }
// HEADER:   enum Code : Int32, _ErrorCodeProtocol {
// HEADER:     init?(rawValue: Int32)
// HEADER:     var rawValue: Int32 { get }
// HEADER:     typealias _ErrorType = TestError
// HEADER:     case TENone
// HEADER:     case TEOne
// HEADER:     case TETwo
// HEADER:   }
// HEADER:   static var TENone: TestError.Code { get }
// HEADER:   static var TEOne: TestError.Code { get }
// HEADER:   static var TETwo: TestError.Code { get }
// HEADER: }
// HEADER: func getErr() -> TestError.Code
