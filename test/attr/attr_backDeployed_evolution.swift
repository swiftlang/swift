//
// At a high level, this test is designed to verify that use of declarations
// annotated with @backDeployed behave as expected when running a client binary
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
// REQUIRES: OS=macosx

// The deployment targets and availability versions hardcoded into this test
// aren't compatible with the environment of the back deployment CI bots.
// UNSUPPORTED: back_deployment_runtime

// Remote test execution does not support dynamically loaded libraries.
// UNSUPPORTED: remote_run

// ---- (0) Prepare SDK
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/SDK_ABI)
// RUN: %empty-directory(%t/SDK_BD)

// ---- (1) Build framework with BackDeploy 2.0 in the past
// RUN: mkdir -p %t/SDK_ABI/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK_ABI/Frameworks/BackDeployHelper.framework/BackDeployHelper) \
// RUN:   -emit-module-path %t/SDK_ABI/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule/%module-target-triple.swiftmodule \
// RUN:   -module-name BackDeployHelper -emit-module %S/Inputs/BackDeployHelper.swift \
// RUN:   -target %target-cpu-apple-macosx10.15 \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 10.15' \
// RUN:   -Xlinker -install_name -Xlinker @rpath/BackDeployHelper.framework/BackDeployHelper \
// RUN:   -enable-library-evolution

// ---- (2) Build executable
// RUN: %target-build-swift -emit-executable %s -g -o %t/test_ABI \
// RUN:   -target %target-cpu-apple-macosx10.14.3 \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 10.15' \
// RUN:   -F %t/SDK_ABI/Frameworks/ -framework BackDeployHelper \
// RUN:   %target-rpath(@executable_path/SDK_ABI/Frameworks)

// ---- (3) Run executable
// RUN: %target-codesign %t/test_ABI
// RUN: %target-run %t/test_ABI | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-ABI %s

// ---- (4) Build framework with BackDeploy 2.0 in the future
// RUN: mkdir -p %t/SDK_BD/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK_BD/Frameworks/BackDeployHelper.framework/BackDeployHelper) \
// RUN:   -emit-module-path %t/SDK_BD/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule/%module-target-triple.swiftmodule \
// RUN:   -module-name BackDeployHelper -emit-module %S/Inputs/BackDeployHelper.swift \
// RUN:   -target %target-cpu-apple-macosx10.15 \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 999.0' \
// RUN:   -Xlinker -install_name -Xlinker @rpath/BackDeployHelper.framework/BackDeployHelper \
// RUN:   -enable-library-evolution

// ---- (5) Build executable
// RUN: %target-build-swift -emit-executable %s -g -o %t/test_BD \
// RUN:   -target %target-cpu-apple-macosx10.14.3 \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 999.0' \
// RUN:   -F %t/SDK_BD/Frameworks/ -framework BackDeployHelper \
// RUN:   %target-rpath(@executable_path/SDK_BD/Frameworks)

// ---- (6) Run executable
// RUN: %target-codesign %t/test_BD
// RUN: %target-run %t/test_BD | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-BD %s

// ---- (7) Re-build framework with the back deployed APIs stripped
// RUN: %empty-directory(%t/SDK_BD)
// RUN: mkdir -p %t/SDK_BD/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule
// RUN: %target-build-swift-dylib(%t/SDK_BD/Frameworks/BackDeployHelper.framework/BackDeployHelper) \
// RUN:   -emit-module-path %t/SDK_BD/Frameworks/BackDeployHelper.framework/Modules/BackDeployHelper.swiftmodule/%module-target-triple.swiftmodule \
// RUN:   -module-name BackDeployHelper -emit-module %S/Inputs/BackDeployHelper.swift \
// RUN:   -target %target-cpu-apple-macosx10.15 \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 1.0:macOS 10.14.3' \
// RUN:   -Xfrontend -define-availability \
// RUN:     -Xfrontend 'BackDeploy 2.0:macOS 999.0' \
// RUN:   -Xlinker -install_name -Xlinker @rpath/BackDeployHelper.framework/BackDeployHelper \
// RUN:   -enable-library-evolution -DSTRIP_V2_APIS

// ---- (8) Re-run executable
// RUN: %target-codesign %t/test_BD
// RUN: %target-run %t/test_BD | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-BD %s

import BackDeployHelper

// CHECK: client: check
testPrint(handle: #dsohandle, "check")
// CHECK: library: check
testPrint(handle: libraryHandle(), "check")

if isV2OrLater() {
  precondition(!v2APIsAreStripped())
}

// CHECK-ABI: library: trivial
// CHECK-BD: client: trivial
trivial()

precondition(try! pleaseThrow(false))
do {
  _ = try pleaseThrow(true)
  fatalError("Should have thrown")
} catch {
  precondition(error as? BadError == BadError.bad)
}

precondition(BadError(fromEmoji: "💥") == nil)
precondition(BadError(fromEmoji: "❗️") == .bad)
precondition(BadError(fromEmoji: "‼️") == .reallyBad)

do {
  let defaulted = IntArray()
  precondition(defaulted.values == [])

  let empty = IntArray.empty
  precondition(empty.values == [])
  precondition(empty[keyPath: \.values] == [])

  var array = IntArray([5])

  // CHECK-ABI: library: [5]
  // CHECK-BD: client: [5]
  array.print()

  array.append(42)
  genericAppend(&array, 3)
  let countable = array.toCountable()
  precondition(existentialCount(countable) == 3)
  array[1] += 1
  precondition(array[1] == 43)

  // CHECK-ABI: library: [5, 43, 3]
  // CHECK-BD: client: [5, 43, 3]
  array.print()
  
  // CHECK-ABI: library: [5, 43, 3]
  // CHECK-BD: client: [5, 43, 3]
  array.rawValues.print()

  precondition(array.removeLast() == 3)
}

do {
  let defaulted = ReferenceIntArray()
  precondition(defaulted.values == [])
  precondition(defaulted[keyPath: \.values] == [])

  let empty = ReferenceIntArray.empty
  precondition(empty.values == [])

  var array = ReferenceIntArray([7])

  // CHECK-ABI: library: [7]
  // CHECK-BD: client: [7]
  array.print()

  do {
    let copy = array.copy()
    precondition(array !== copy)
    precondition(copy.values == [7])
  }

  array.append(39)
  genericAppend(&array, 1)
  let countable = array.toCountable()
  precondition(existentialCount(countable) == 3)
  array[1] += 1
  precondition(array[1] == 40)

  // CHECK-ABI: library: [7, 40, 1]
  // CHECK-BD: client: [7, 40, 1]
  array.print()
  
  // CHECK-ABI: library: [7, 40, 1]
  // CHECK-BD: client: [7, 40, 1]
  array.rawValues.print()

  precondition(array.removeLast() == 1)
}
