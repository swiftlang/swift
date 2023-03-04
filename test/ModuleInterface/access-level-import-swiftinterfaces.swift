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
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client.private.swiftinterface \
// RUN:   -enable-experimental-feature AccessLevelOnImport

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -I %t \
// RUN:   -module-name Client

// RUN: %FileCheck %s < %t/Client.swiftinterface
// RUN: %FileCheck %s < %t/Client.private.swiftinterface

/// Build a client composed of many files.
// RUN: %target-swift-frontend -typecheck %t/MultiFiles?.swift -I %t \
// RUN:   -package-name TestPackage \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/MultiFiles.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/MultiFiles.private.swiftinterface \
// RUN:   -enable-experimental-feature AccessLevelOnImport

// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles.private.swiftinterface) -I %t \
// RUN:   -module-name MultiFiles

// RUN: %FileCheck %s < %t/MultiFiles.swiftinterface
// RUN: %FileCheck %s < %t/MultiFiles.private.swiftinterface

// REQUIRES: rdar106219959

//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift

//--- Client.swift
public import PublicLib
// CHECK: PublicLib

package import PackageLib
// CHECK-NOT: PackageLib

internal import InternalLib
// CHECK-NOT: InternalLib

fileprivate import FileprivateLib
// CHECK-NOT: FileprivateLib

private import PrivateLib
// CHECK-NOT: PrivateLib

//--- MultiFilesA.swift
public import PublicLib
private import InternalLib

//--- MultiFilesB.swift
internal import PublicLib
internal import InternalLib
