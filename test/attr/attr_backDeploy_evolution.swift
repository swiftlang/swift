//
// At a high level, this test is designed to verify that use of declarations
// annotated with @_backDeploy behave as expected when running a client binary
// on an older OS that does not have the back deployed APIs. The
// BackDeployHelper framework has a number of APIs that are available in the
// OSes identified by the "BackDeploy 1.0" availability macro and are back
// deployed before OSes identified "BackDeploy 2.0". Verification is performed
// the following way:
//
//  1. Build the helper framework with both BackDeploy 1.0 defined to an
//     OS version before Swift ABI stability and 2.0 defined to an OS version
//     after Swift ABI stability. Note that stradling ABI stability is not
//     a necessary requirement of this test; it's just convenient to use OS
//     versions that correspond to existing lit substitutions.
//  2. Build the client executable with a deployment target set to the same
//     OS version as BackDeploy 2.0.
//  3. Run the client executable, verifying that the copies of the functions in
//     the framework are used (verified by runtime logic using #dsohandle).
//  4. Build a new copy of the helper framework, this time with BackDeploy 2.0
//     set to a distant future OS version.
//  5. Build a new copy of the client executable using the new framework and
//     the deployment target set to the same OS version as BackDeploy 1.0.
//  6. Run the new executable, verifying with #dsohandle that client copies of
//     the APIs are used.
//  7. Re-build the framework in place, this time stripping the definitions of
//     the back deployed APIs entirely.
//  8. Re-run the unmodified executable, with the same expectations as in (6).
//     However, this time we're also verifying that the executable can run even
//     without the original API symbols present in the linked dylib.
//

// REQUIRES: executable_test
// REQUIRES: VENDOR=apple

// ---- (0) Prepare SDK
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/SDK_ABI)
// RUN: %empty-directory(%t/SDK_BD)

// ---- (1) Build famework with BackDeploy 2.0 in the past
// RUN: mkdir -p %t/SDK_ABI/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK_ABI/Frameworks/BackDeployHelper.framework/BackDeployHelper) \
// RUN:   -emit-module-path %t/SDK_ABI/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule/%module-target-triple.swiftmodule \
// RUN:   -module-name BackDeployHelper -emit-module %S/Inputs/BackDeployHelper.swift \
// RUN:   -Xfrontend -target -Xfrontend %target-next-stable-abi-triple \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3, iOS 12.1, tvOS 12.1, watchOS 5.1' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 10.15, iOS 13, tvOS 13, watchOS 6' \
// RUN:   -Xlinker -install_name -Xlinker @rpath/BackDeployHelper.framework/BackDeployHelper \
// RUN:   -enable-library-evolution

// ---- (2) Build executable
// RUN: %target-build-swift -emit-executable %s -g -o %t/test_ABI \
// RUN:   -Xfrontend -target -Xfrontend %target-pre-stable-abi-triple \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3, iOS 12.1, tvOS 12.1, watchOS 5.1' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 10.15, iOS 13, tvOS 13, watchOS 6' \
// RUN:   -F %t/SDK_ABI/Frameworks/ -framework BackDeployHelper \
// RUN:   %target-rpath(@executable_path/SDK_ABI/Frameworks)

// ---- (3) Run executable
// RUN: %target-codesign %t/test_ABI
// RUN: %target-run %t/test_ABI %t/SDK_ABI/Frameworks/BackDeployHelper.framework/BackDeployHelper | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-ABI %s

// ---- (4) Build famework with BackDeploy 2.0 in the future
// RUN: mkdir -p %t/SDK_BD/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK_BD/Frameworks/BackDeployHelper.framework/BackDeployHelper) \
// RUN:   -emit-module-path %t/SDK_BD/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule/%module-target-triple.swiftmodule \
// RUN:   -module-name BackDeployHelper -emit-module %S/Inputs/BackDeployHelper.swift \
// RUN:   -Xfrontend -target -Xfrontend %target-next-stable-abi-triple \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3, iOS 12.1, tvOS 12.1, watchOS 5.1' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 999.0, iOS 999.0, watchOS 999.0, tvOS 999.0' \
// RUN:   -Xlinker -install_name -Xlinker @rpath/BackDeployHelper.framework/BackDeployHelper \
// RUN:   -enable-library-evolution

// ---- (5) Build executable
// RUN: %target-build-swift -emit-executable %s -g -o %t/test_BD \
// RUN:   -Xfrontend -target -Xfrontend %target-next-stable-abi-triple \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3, iOS 12.1, tvOS 12.1, watchOS 5.1' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 999.0, iOS 999.0, watchOS 999.0, tvOS 999.0' \
// RUN:   -F %t/SDK_BD/Frameworks/ -framework BackDeployHelper \
// RUN:   %target-rpath(@executable_path/SDK_BD/Frameworks)

// ---- (6) Run executable
// RUN: %target-codesign %t/test_BD
// RUN: %target-run %t/test_BD %t/SDK_BD/Frameworks/BackDeployHelper.framework/BackDeployHelper | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-BD %s

// ---- (7) Re-build famework with the back deployed APIs stripped
// RUN: mkdir -p %t/SDK_BD/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK_BD/Frameworks/BackDeployHelper.framework/BackDeployHelper) \
// RUN:   -emit-module-path %t/SDK_BD/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule/%module-target-triple.swiftmodule \
// RUN:   -module-name BackDeployHelper -emit-module %S/Inputs/BackDeployHelper.swift \
// RUN:   -Xfrontend -target -Xfrontend %target-next-stable-abi-triple \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3, iOS 12.1, tvOS 12.1, watchOS 5.1' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 999.0, iOS 999.0, watchOS 999.0, tvOS 999.0' \
// RUN:   -Xlinker -install_name -Xlinker @rpath/BackDeployHelper.framework/BackDeployHelper \
// RUN:   -enable-library-evolution -DSTRIP_V2_APIS

// ---- (8) Re-run executable
// RUN: %target-codesign %t/test_BD
// RUN: %target-run %t/test_BD %t/SDK_BD/Frameworks/BackDeployHelper.framework/BackDeployHelper | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-BD %s

import BackDeployHelper

// CHECK: client: check
testPrint(handle: #dsohandle, "check")
// CHECK: library: check
testPrint(handle: libraryHandle(), "check")

if isV2OrLater() {
  assert(!v2APIsAreStripped())
}

// CHECK-ABI: library: trivial
// CHECK-BD: client: trivial
trivial()

assert(try! pleaseThrow(false))
do {
  _ = try pleaseThrow(true)
  fatalError("Should have thrown")
} catch {
  assert(error as? BadError == BadError.bad)
}

do {
  let zero = MutableInt.zero
  assert(zero.value == 0)

  var int = MutableInt(5)

  // CHECK-ABI: library: 5
  // CHECK-BD: client: 5
  int.print()

  assert(int.increment(by: 2) == 7)
  assert(genericIncrement(&int, by: 3) == 10)
  assert(int.decrement(by: 1) == 9)

  var incrementable: any Incrementable = int.toIncrementable()

  // CHECK-ABI: library: 10
  // CHECK-BD: client: 10
  existentialIncrementByOne(&incrementable)

  let int2 = MutableInt(0x7BB7914B)
  for (i, expectedByte) in [0x4B, 0x91, 0xB7, 0x7B].enumerated() {
    assert(int2[byteAt: i] == expectedByte)
  }
}

do {
  let zero = ReferenceInt.zero
  assert(zero.value == 0)

  var int = ReferenceInt(42)

  // CHECK-ABI: library: 42
  // CHECK-BD: client: 42
  int.print()

  do {
    let copy = int.copy()
    assert(int !== copy)
    assert(copy.value == 42)
  }

  assert(int.increment(by: 2) == 44)
  assert(genericIncrement(&int, by: 3) == 47)
  assert(int.decrement(by: 46) == 1)

  var incrementable: any Incrementable = int.toIncrementable()

  // CHECK-ABI: library: 2
  // CHECK-BD: client: 2
  existentialIncrementByOne(&incrementable)

  let int2 = MutableInt(0x08AFAB76)
  for (i, expectedByte) in [0x76, 0xAB, 0xAF, 0x08].enumerated() {
    assert(int2[byteAt: i] == expectedByte)
  }
}
