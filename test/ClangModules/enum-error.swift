// REQUIRES: OS=macosx

// RUN: %target-swift-frontend -DVALUE -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | FileCheck %s -check-prefix=VALUE
// RUN: %target-swift-frontend -DEMPTYCATCH -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | FileCheck %s -check-prefix=EMPTYCATCH
// RUN: %target-swift-frontend -DASQEXPR -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | FileCheck %s -check-prefix=ASQEXPR
// RUN: %target-swift-frontend -DASBANGEXPR -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | FileCheck %s -check-prefix=ASBANGEXPR
// RUN: %target-swift-frontend -DCATCHIS -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | FileCheck %s -check-prefix=CATCHIS
// RUN: %target-swift-frontend -DCATCHAS -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | FileCheck %s -check-prefix=CATCHAS
// RUN: %target-swift-frontend -DGENERICONLY -emit-sil %s -import-objc-header %S/Inputs/enum-error.h | FileCheck %s -check-prefix=GENERICONLY

// RUN: %target-swift-frontend -emit-sil %s -import-objc-header %S/Inputs/enum-error.h -verify

import Foundation


func testError() {
  let testErrorNSError = NSError(domain: TestErrorDomain,
                                 code: Int(TestError.TENone.rawValue),
                                 userInfo: nil)

// Below are a number of test cases to make sure that various pattern and cast
// expression forms are sufficient in pulling in the _NSBridgedError
// conformance.
#if VALUE
// VALUE: TestError: _BridgedNSError
  let terr = getErr(); terr

#elseif EMPTYCATCH
// EMPTYCATCH: TestError: _BridgedNSError
  do {
    throw TestError.TENone
  } catch {}

#elseif ASQEXPR
// ASQEXPR: sil_witness_table shared [fragile] TestError: _BridgedNSError module __ObjC
  let wasTestError = testErrorNSError as? TestError; wasTestError

#elseif ASBANGEXPR
// ASBANGEXPR: sil_witness_table shared [fragile] TestError: _BridgedNSError module __ObjC
  let terr2 = testErrorNSError as! TestError; terr2

#elseif ISEXPR
// ISEXPR: sil_witness_table shared [fragile] TestError: _BridgedNSError module __ObjC
  if (testErrorNSError is TestError) {
    print("true")
  } else {
    print("false")
  }

#elseif CATCHIS
// CATCHIS: sil_witness_table shared [fragile] TestError: _BridgedNSError module __ObjC
  do {
    throw TestError.TETwo
  } catch is TestError {
  } catch {}

#elseif CATCHAS
// CATCHAS: sil_witness_table shared [fragile] TestError: _BridgedNSError module __ObjC
  do {
    throw TestError.TETwo
  } catch let err as TestError {
    err
  } catch {}

#elseif GENERICONLY
// GENERICONLY: TestError: _BridgedNSError
  func dyncast<T, U>(_ x: T) -> U {
    return x as! U
  }
  let _ : TestError = dyncast(testErrorNSError)

#else
// CHECK: sil_witness_table shared [fragile] TestError: _BridgedNSError module __ObjC
  let terr = getErr()
  switch (terr) { case .TENone, .TEOne, .TETwo: break } // ok

  switch (terr) { case .TENone, .TEOne: break }
    // expected-error@-1 {{switch must be exhaustive, consider adding a default clause}}

  let _ = TestError(rawValue: 2)!

  do {
    throw TestError.TEOne
  } catch is TestError {
  } catch {}

#endif

}
