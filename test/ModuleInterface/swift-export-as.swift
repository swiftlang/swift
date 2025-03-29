// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Make sure the flag `ModuleInterfaceExportAs` doesn't raise an error.
// RUN: %target-swift-frontend -emit-module %t/PrivateLib.swift \
// RUN:   -enable-experimental-feature ModuleInterfaceExportAs

/// Build exportee.
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

/// Build exporter.
// RUN: %target-swift-frontend -emit-module %t/PublicLib.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -o %t/PublicLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/PublicLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/PublicLib.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/PublicLib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/PublicLib.private.swiftinterface) -I %t \
// RUN:   -module-name PublicLib
// RUN: cat %t/PublicLib.swiftinterface | %FileCheck --check-prefixes=PUBLICLIB-PUBLIC %s
// RUN: cat %t/PublicLib.private.swiftinterface | %FileCheck --check-prefixes=PUBLICLIB-PRIVATE %s

/// Build client.
// RUN: %target-swift-frontend -emit-module %t/ClientLib.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -o %t/ClientLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/ClientLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/ClientLib.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.private.swiftinterface) -I %t -module-name ClientLib
// RUN: cat %t/ClientLib.swiftinterface | %FileCheck --check-prefixes=CLIENT-PUBLIC %s
// RUN: cat %t/ClientLib.private.swiftinterface | %FileCheck --check-prefixes=CLIENT-PRIVATE %s

/// Build client against private swiftinterfaces.
// RUN: rm -f %t/PrivateLib.swiftmodule %t/PublicLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/ClientLib.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -o %t/ClientLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/ClientLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/ClientLib.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.private.swiftinterface) -I %t -module-name ClientLib
// RUN: cat %t/ClientLib.swiftinterface | %FileCheck --check-prefixes=CLIENT-PUBLIC %s
// RUN: cat %t/ClientLib.private.swiftinterface | %FileCheck --check-prefixes=CLIENT-PRIVATE %s

/// Build client against public swiftinterfaces, for the same result.
// RUN: rm -f %t/PrivateLib.private.swiftinterface %t/PublicLib.private.swiftinterface
// RUN: %target-swift-frontend -emit-module %t/ClientLib.swift -I %t \
// RUN:   -swift-version 5 -enable-library-evolution \
// RUN:   -o %t/ClientLib.swiftmodule \
// RUN:   -emit-module-interface-path %t/ClientLib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/ClientLib.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/ClientLib.private.swiftinterface) -I %t -module-name ClientLib
// RUN: cat %t/ClientLib.swiftinterface | %FileCheck --check-prefixes=CLIENT-PUBLIC %s
// RUN: cat %t/ClientLib.private.swiftinterface | %FileCheck --check-prefixes=CLIENT-PRIVATE %s

// REQUIRES: swift_feature_ModuleInterfaceExportAs

//--- PrivateLib.swift

public struct PrivateNameStruct {}

public func privateLibUser(_ arg: PrivateNameStruct) {}
// PRIVATELIB-PUBLIC: arg: PrivateLib.PrivateNameStruct

//--- PublicLib.swift

@_exported import PrivateLib

public struct PublicNameStruct {}

public func publicLibUser(_ arg: PrivateNameStruct) {}
// PUBLICLIB-PUBLIC: arg: PublicLib.PrivateNameStruct
// PUBLICLIB-PRIVATE: arg: PrivateLib.PrivateNameStruct

//--- ClientLib.swift

import PublicLib

public func userOfPrivate(_ argUserOfPrivate: PrivateNameStruct) {}
// CLIENT-PUBLIC: argUserOfPrivate: PublicLib.PrivateNameStruct
// CLIENT-PRIVATE: argUserOfPrivate: PrivateLib.PrivateNameStruct
public func userOfPublic(_ argUserOfPublic: PublicNameStruct) {}
// CLIENT-PUBLIC: argUserOfPublic: PublicLib.PublicNameStruct
// CLIENT-PRIVATE: argUserOfPublic: PublicLib.PublicNameStruct
