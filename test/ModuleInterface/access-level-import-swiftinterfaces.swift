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
// RUN: %target-swift-frontend -emit-module %t/LibX.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/LibY.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/LibZ.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/LibS.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/LibT.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/LibU.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/LibV.swift -o %t \
// RUN:   -swift-version 5 -enable-library-evolution

/// Build client and generate swiftinterfaces.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name TestPackage \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Client.package.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -I %t \
// RUN:   -module-name Client
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.package.swiftinterface) -I %t \
// RUN:   -module-name Client

// RUN: %FileCheck --check-prefixes=CHECK,CHECK-5 %s < %t/Client.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK,CHECK-5 %s < %t/Client.private.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK-5-PKG %s < %t/Client.package.swiftinterface

/// Build a client composed of many files.
// RUN: %target-swift-frontend -typecheck %t/MultiFiles?.swift -I %t \
// RUN:   -package-name TestPackage \
// RUN:   -experimental-spi-only-imports \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/MultiFiles.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/MultiFiles.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/MultiFiles.package.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles.private.swiftinterface) -I %t \
// RUN:   -module-name MultiFiles
// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles.package.swiftinterface) -I %t \
// RUN:   -module-name MultiFiles

// RUN: %FileCheck --check-prefixes=CHECK-5-MUL %s < %t/MultiFiles.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK-5-MUL-PRV %s < %t/MultiFiles.private.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK-5-MUL-PKG %s < %t/MultiFiles.package.swiftinterface

/// Feature flag.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name TestPackage -module-name Client_FeatureFlag \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Client_FeatureFlag.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client_FeatureFlag.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Client_FeatureFlag.package.swiftinterface \
// RUN:   -enable-upcoming-feature InternalImportsByDefault

// RUN: %target-swift-typecheck-module-from-interface(%t/Client_FeatureFlag.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Client_FeatureFlag.private.swiftinterface) -I %t \
// RUN:   -module-name Client_FeatureFlag
// RUN: %target-swift-typecheck-module-from-interface(%t/Client_FeatureFlag.package.swiftinterface) -I %t \
// RUN:   -module-name Client_FeatureFlag

// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-7,CHECK-FLAG < %t/Client_FeatureFlag.swiftinterface
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-7,CHECK-FLAG < %t/Client_FeatureFlag.private.swiftinterface
// RUN: %FileCheck %s --check-prefixes=CHECK-7-PKG,CHECK-FLAG < %t/Client_FeatureFlag.package.swiftinterface

/// Build a client with multiple files.
// RUN: %target-swift-frontend -typecheck %t/MultiFiles?.swift -I %t \
// RUN:   -package-name TestPackage -module-name MultiFiles_Swift6 \
// RUN:   -experimental-spi-only-imports \
// RUN:   -enable-library-evolution -enable-upcoming-feature InternalImportsByDefault \
// RUN:   -emit-module-interface-path %t/MultiFiles_Swift6.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/MultiFiles_Swift6.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/MultiFiles_Swift6.package.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles_Swift6.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles_Swift6.private.swiftinterface) -I %t \
// RUN:   -module-name MultiFiles_Swift6
// RUN: %target-swift-typecheck-module-from-interface(%t/MultiFiles_Swift6.package.swiftinterface) -I %t \
// RUN:   -module-name MultiFiles_Swift6

// RUN: %FileCheck --check-prefixes=CHECK-7-MUL %s < %t/MultiFiles_Swift6.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK-7-MUL-PRV %s < %t/MultiFiles_Swift6.private.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK-7-MUL-PKG %s < %t/MultiFiles_Swift6.package.swiftinterface

// REQUIRES: swift_feature_InternalImportsByDefault

//--- PublicLib.swift
//--- PackageLib.swift
//--- InternalLib.swift
//--- FileprivateLib.swift
//--- PrivateLib.swift
//--- LibX.swift
//--- LibY.swift
//--- LibZ.swift
//--- LibS.swift
//--- LibT.swift
//--- LibU.swift
//--- LibV.swift

//--- Client.swift

// CHECK-5-NOT: public
// CHECK-5-PKG: package import PackageLib
// CHECK-5-PKG: import PublicLib

// CHECK-FLAG: -enable-upcoming-feature InternalImportsByDefault
// CHECK-7: public
// CHECK-7-PKG: package import PackageLib
// CHECK-7-PKG: public import PublicLib

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
package import PackageLib
private import InternalLib
public import LibX
@_exported public import LibY
internal import LibZ
@_spiOnly public import LibS
@_spiOnly public import LibT
@_spi(Lib) public import LibU
@_spi(Lib) public import LibV

//--- MultiFilesB.swift

internal import PublicLib
internal import PackageLib
internal import InternalLib
package import LibX
package import LibY
fileprivate import LibZ
package import LibS
internal import LibT
package import LibU
internal import LibV

// CHECK-5-MUL-NOT: public
// CHECK-5-MUL: import LibU
// CHECK-5-MUL: import LibV
// CHECK-5-MUL: import LibX
// CHECK-5-MUL: @_exported import LibY
// CHECK-5-MUL: import PublicLib

// CHECK-7-MUL: public import LibU
// CHECK-7-MUL: public import LibV
// CHECK-7-MUL: public import LibX
// CHECK-7-MUL: @_exported public import LibY
// CHECK-7-MUL: public import PublicLib

// CHECK-5-MUL-PKG: @_spiOnly import LibS
// CHECK-5-MUL-PKG: @_spiOnly import LibT
// CHECK-5-MUL-PKG: @_spi(Lib) import LibU
// CHECK-5-MUL-PKG: @_spi(Lib) import LibV
// CHECK-5-MUL-PKG: import LibX
// CHECK-5-MUL-PKG: @_exported import LibY
// CHECK-5-MUL-PKG: package import PackageLib
// CHECK-5-MUL-PKG: import PublicLib

// CHECK-7-MUL-PKG: @_spiOnly public import LibS
// CHECK-7-MUL-PKG: @_spiOnly public import LibT
// CHECK-7-MUL-PKG: @_spi(Lib) public import LibU
// CHECK-7-MUL-PKG: @_spi(Lib) public import LibV
// CHECK-7-MUL-PKG: public import LibX
// CHECK-7-MUL-PKG: @_exported public import LibY
// CHECK-7-MUL-PKG: package import PackageLib
// CHECK-7-MUL-PKG: public import PublicLib

// CHECK-5-MUL-PRV: @_spiOnly import LibS
// CHECK-5-MUL-PRV: @_spiOnly import LibT
// CHECK-5-MUL-PRV: @_spi(Lib) import LibU
// CHECK-5-MUL-PRV: @_spi(Lib) import LibV
// CHECK-5-MUL-PRV: import LibX
// CHECK-5-MUL-PRV: @_exported import LibY
// CHECK-5-MUL-PRV: import PublicLib

// CHECK-7-MUL-PRV: @_spiOnly public import LibS
// CHECK-7-MUL-PRV: @_spiOnly public import LibT
// CHECK-7-MUL-PRV: @_spi(Lib) public import LibU
// CHECK-7-MUL-PRV: @_spi(Lib) public import LibV
// CHECK-7-MUL-PRV: public import LibX
// CHECK-7-MUL-PRV: @_exported public import LibY
// CHECK-7-MUL-PRV: public import PublicLib
