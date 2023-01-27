// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -export-as PublicLib \
// RUN:   -o %t/PrivateLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/PrivateLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/PrivateLib.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/PrivateLib.swiftinterface)
// RUN: %target-swift-typecheck-module-from-interface(%t/PrivateLib.private.swiftinterface) \
// RUN:   -module-name PrivateLib
// RUN: cat %t/PrivateLib.swiftinterface | %FileCheck --check-prefixes=PRIVATELIB-PUBLIC %s
// RUN: cat %t/PrivateLib.private.swiftinterface | %FileCheck --check-prefixes=PRIVATELIB-PUBLIC %s

// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -o %t/PublicLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/PublicLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/PublicLib.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/PublicLib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/PublicLib.private.swiftinterface) -I %t \
// RUN:   -module-name PublicLib
// RUN: cat %t/PublicLib.swiftinterface | %FileCheck --check-prefixes=PUBLICLIB-PUBLIC %s
// RUN: cat %t/PublicLib.private.swiftinterface | %FileCheck --check-prefixes=PUBLICLIB-PUBLIC %s

/// Default logic applying export-as in both swiftinterface.
// RUN: %target-swift-frontend -emit-module %t/ClientLib.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -o %t/ClientLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/ClientLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/ClientLib.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.private.swiftinterface) -I %t
// RUN: cat %t/ClientLib.swiftinterface | %FileCheck --check-prefixes=CLIENT-PUBLIC %s
// RUN: cat %t/ClientLib.private.swiftinterface | %FileCheck --check-prefixes=CLIENT-PUBLIC %s

/// New logic applying export-as only in the public swiftinterface with
/// `-enable-experimental-feature ModuleInterfaceExportAs`.
// RUN: %target-swift-frontend -emit-module %t/ClientLib.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -o %t/ClientLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/ClientLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/ClientLib.private.swiftinterface \
// RUN:   -enable-experimental-feature ModuleInterfaceExportAs
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.private.swiftinterface) -I %t
// RUN: cat %t/ClientLib.swiftinterface | %FileCheck --check-prefixes=CLIENT-PUBLIC %s
// RUN: cat %t/ClientLib.private.swiftinterface | %FileCheck --check-prefixes=CLIENT-PRIVATE %s

/// Check that we get the same behavior using swiftinterfaces only.
// RUN: rm -f %t/PrivateLib.swiftmodule %t/PublicLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/ClientLib.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -o %t/ClientLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/ClientLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/ClientLib.private.swiftinterface \
// RUN:   -enable-experimental-feature ModuleInterfaceExportAs
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.private.swiftinterface) -I %t
// RUN: cat %t/ClientLib.swiftinterface | %FileCheck --check-prefixes=CLIENT-PUBLIC %s
// RUN: cat %t/ClientLib.private.swiftinterface | %FileCheck --check-prefixes=CLIENT-PRIVATE %s

//--- PrivateLib.swift

public struct PrivateNameStruct {}

public func privateLibUser(_ arg: PrivateNameStruct) {}
// PRIVATELIB-PUBLIC: arg: PrivateLib.PrivateNameStruct

//--- PublicLib.swift

@_exported import PrivateLib

public struct PublicNameStruct {}

public func publicLibUser(_ arg: PrivateNameStruct) {}
// PUBLICLIB-PUBLIC: arg: PublicLib.PrivateNameStruct

//--- ClientLib.swift

import PublicLib

public func userOfPrivate(_ argUserOfPrivate: PrivateNameStruct) {}
// CLIENT-PUBLIC: argUserOfPrivate: PublicLib.PrivateNameStruct
// CLIENT-PRIVATE: argUserOfPrivate: PrivateLib.PrivateNameStruct
public func userOfPublic(_ argUserOfPublic: PublicNameStruct) {}
// CLIENT-PUBLIC: argUserOfPublic: PublicLib.PublicNameStruct
// CLIENT-PRIVATE: argUserOfPublic: PublicLib.PublicNameStruct
