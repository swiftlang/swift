/// Check that only public imports are printed in modules interfaces,
/// package imports and below are not.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PackageLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/InternalLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/FileprivateLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution

/// Build client and generate swiftinterfaces.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name TestPackage \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client.private.swiftinterface \
// RUN:   -enable-experimental-feature AccessLevelOnImport

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -I %t \
// RUN:   -module-name Client

// RUN: %FileCheck -check-prefix=CHECK-PUBLIC %s < %t/Client.swiftinterface
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/Client.private.swiftinterface

/// Build a client composed of many files.
// RUN: %target-swift-frontend -typecheck %t/MultiFiles?.swift -I %t \
// RUN:   -package-name TestPackage \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-interface-path %t/MultiFiles.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/MultiFiles.private.swiftinterface \
// RUN:   -enable-experimental-feature AccessLevelOnImport

// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles.private.swiftinterface) -I %t \
// RUN:   -module-name MultiFiles

// RUN: %FileCheck -check-prefix=CHECK-PUBLIC %s < %t/MultiFiles.swiftinterface
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/MultiFiles.private.swiftinterface

//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift

//--- Client.swift
package import PackageLib
// CHECK-PUBLIC-NOT: PackageLib
// CHECK-PRIVATE-NOT: PackageLib

internal import InternalLib
// CHECK-PUBLIC-NOT: InternalLib
// CHECK-PRIVATE-NOT: InternalLib

fileprivate import FileprivateLib
// CHECK-PUBLIC-NOT: FileprivateLib
// CHECK-PRIVATE-NOT: FileprivateLib

private import PrivateLib
// CHECK-PUBLIC-NOT: PrivateLib
// CHECK-PRIVATE-NOT: PrivateLib

//--- MultiFilesA.swift
public import PublicLib
private import InternalLib

//--- MultiFilesB.swift
internal import PublicLib
internal import InternalLib
