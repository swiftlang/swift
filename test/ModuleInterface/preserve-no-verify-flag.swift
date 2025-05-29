// RUN: %empty-directory(%t)

/// Check that the flag -no-verify-emitted-module-interface is written down in the swiftinterface.
// RUN: %target-build-swift -emit-library -enable-library-evolution -emit-module-interface -emit-module -swift-version 5 -o %t/NoVerified.o -no-verify-emitted-module-interface -module-name NoVerified %s
// RUN: cat %t/NoVerified.swiftinterface | %FileCheck --check-prefix FLAG %s
// FLAG: swift-module-flags-ignorable:
// FLAG-SAME: -no-verify-emitted-module-interface

/// Check that there's no skip flag in the swiftinterface.
// RUN: %target-build-swift -emit-library -enable-library-evolution -emit-module-interface -emit-module -swift-version 5 -o %t/Verified.o -verify-emitted-module-interface -module-name Verified %s
// RUN: cat %t/Verified.swiftinterface | %FileCheck --check-prefix NO-FLAG %s
// NO-FLAG-NOT: verify-emitted-module-interface

/// Check last flag priority.
// RUN: %target-build-swift -emit-library -enable-library-evolution -emit-module-interface -emit-module -swift-version 5 -o %t/VerifiedManyFlags.o -no-verify-emitted-module-interface -module-name VerifiedManyFlags %s -verify-emitted-module-interface
// RUN: cat %t/VerifiedManyFlags.swiftinterface | %FileCheck --check-prefix NO-FLAG %s

// RUN: %target-build-swift -emit-library -enable-library-evolution -emit-module-interface -emit-module -swift-version 5 -o %t/NoVerifiedManyFlags.o -verify-emitted-module-interface -module-name NoVerifiedManyFlags %s -no-verify-emitted-module-interface
// RUN: cat %t/NoVerifiedManyFlags.swiftinterface | %FileCheck --check-prefix FLAG %s

public struct MyStruct {}
