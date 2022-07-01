// RUN: %empty-directory(%t)

// NOTE: the two tests below look like a lot of copy-pasting, but the only thing that's varying between
// the two checks is whether "-D DELEGATES" is passed when compiling MysteryInit and whether the check-prefix when running the executable matches that or not.

/// -----------------------------------------------------------------------
/// Check if the actor init in the library can change from delegating to non-delegating
/// -----------------------------------------------------------------------

// ------> first library DOES delegate
// RUN: %target-build-swift-dylib(%t/%target-library-name(MysteryInit)) -D DELEGATES -Xfrontend -disable-availability-checking -enable-library-evolution %S/Inputs/MysteryInit.swift -emit-module -emit-module-path %t/MysteryInit.swiftmodule -module-name MysteryInit
// RUN: %target-codesign %t/%target-library-name(MysteryInit)


// ------> make sure that works
// RUN: %target-build-swift -parse-as-library  -Xfrontend -disable-availability-checking %s -lMysteryInit -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(MysteryInit) | %FileCheck --check-prefix=CHECK-DELEGATES %s

// ------> do a little internal sanity check on this test itself
// RUN: %target-run %t/main %t/%target-library-name(MysteryInit) | not %FileCheck --check-prefix=CHECK-NO-DELEGATES %s

// ------> now recompile that library's init so it DOES NOT delegate, without recompiling executable
// RUN: %target-build-swift-dylib(%t/%target-library-name(MysteryInit)) -Xfrontend -disable-availability-checking -enable-library-evolution %S/Inputs/MysteryInit.swift -emit-module -emit-module-path %t/MysteryInit.swiftmodule -module-name MysteryInit
// RUN: %target-codesign %t/%target-library-name(MysteryInit)

// ------> re-run executable
// RUN: %target-run %t/main %t/%target-library-name(MysteryInit) | %FileCheck --check-prefix=CHECK-NO-DELEGATES %s


/// -----------------------------------------------------------------------
/// now, other direction: Check if the actor init can change from non-delegating to delegating
/// -----------------------------------------------------------------------

// ------> library currently DOES NOT delegate. recompile executable to match.
// RUN: %target-build-swift -parse-as-library  -Xfrontend -disable-availability-checking %s -lMysteryInit -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(MysteryInit) | %FileCheck --check-prefix=CHECK-NO-DELEGATES %s

// ------> now recompile that library's init so it DOES delegate, without recompiling executable
// RUN: %target-build-swift-dylib(%t/%target-library-name(MysteryInit)) -D DELEGATES -Xfrontend -disable-availability-checking -enable-library-evolution %S/Inputs/MysteryInit.swift -emit-module -emit-module-path %t/MysteryInit.swiftmodule -module-name MysteryInit
// RUN: %target-codesign %t/%target-library-name(MysteryInit)

// ------> re-run executable
// RUN: %target-run %t/main %t/%target-library-name(MysteryInit) | %FileCheck --check-prefix=CHECK-DELEGATES %s


// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import MysteryInit

@main
struct Main {
  static func main() {
    switch BigFoot() {
    case .none:
      print("bigfoot is myth")
      // CHECK-NO-DELEGATES: bigfoot is myth
    default:
      print("bigfoot is real")
      // CHECK-DELEGATES: bigfoot is real
    }
  }
}
