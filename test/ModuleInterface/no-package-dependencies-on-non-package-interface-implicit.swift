// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/Swift)
// RUN: %empty-directory(%t/OtherSwift)
// RUN: split-file %s %t

// Build in-package ModuleC
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherSwift/ModuleC.swiftmodule -module-name ModuleC -package-name TestPak %t/C.swift

// Build in-package ModuleB
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Swift/ModuleB.swiftmodule -enable-library-evolution -emit-module-interface-path %t/Swift/ModuleB.swiftinterface  -module-name ModuleB -package-name TestPak %t/B.swift -I%t/OtherSwift

// Build in-package ModuleA
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Swift/ModuleA.swiftmodule -enable-library-evolution -emit-module-interface-path %t/Swift/ModuleA.swiftinterface -module-name ModuleA -package-name TestPak %t/A.swift -I%t/Swift -I%t/OtherSwift

// Remove binary module for A to make sure an implicit interface compile gets triggered
// RUN: rm %t/Swift/ModuleA.swiftmodule
// Remove in-package-only dependency of B to ensure that if the compiler looks for it, compilation will fail
// RUN: rm %t/OtherSwift/ModuleC.swiftmodule

// Build out-of-package client source
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Swift/Client.swiftmodule -module-name Client %t/Client.swift -I%t/Swift

//--- C.swift
public func c() {}

//--- B.swift
package import ModuleC

//--- A.swift
@_exported public import ModuleB

//--- Client.swift
import ModuleA

