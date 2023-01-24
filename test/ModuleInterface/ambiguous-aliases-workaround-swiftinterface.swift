/// Test the generated swiftinterface with -alias-module-names-in-module-interface.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/PublicLib.swiftmodule %t/EmptyLib.swift
// RUN: %target-swift-frontend -emit-module \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/SPILib.swiftmodule %t/EmptyLib.swift
// RUN: %target-swift-frontend -emit-module \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/IOILib.swiftmodule %t/EmptyLib.swift

// RUN: %target-swift-frontend -emit-module \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/Client.swiftmodule \
// RUN:     -emit-module-interface-path %t/Client.swiftinterface \
// RUN:     -emit-private-module-interface-path %t/Client.private.swiftinterface \
// RUN:     %t/Client.swift -I %t -experimental-spi-only-imports \
// RUN:     -alias-module-names-in-module-interface
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I%t
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -module-name Client -I%t

// RUN: cat %t/Client.swiftinterface | %FileCheck %s -check-prefix=PUBLIC
// RUN: cat %t/Client.private.swiftinterface | %FileCheck %s -check-prefix=PRIVATE

//--- EmptyLib.swift

public struct SomeType {}

//--- Client.swift

@_implementationOnly import IOILib
@_spiOnly import SPILib
import PublicLib

/// Check alias declarations.
// PUBLIC-NOT: IOILib
// PUBLIC-NOT: SPILib
// PUBLIC: -module-alias Module___PublicLib
// PRIVATE-NOT: IOILib
// PRIVATE: -module-alias Module___SPILib
// PRIVATE: -module-alias Module___PublicLib

/// Check imports.
// PUBLIC: import Module___PublicLib
// PRIVATE: import Module___PublicLib
// PRIVATE: import Module___SPILib
