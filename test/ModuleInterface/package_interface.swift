// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build a package swiftinterface.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -module-name Lib -package-name libPkg \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Lib.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Lib.package.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.package.swiftinterface) -I %t -module-name Lib
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.private.swiftinterface) -I %t -module-name Lib

// RUN: %FileCheck --check-prefixes=CHECK-PUB %s < %t/Lib.swiftinterface
// CHECK-PUB-NOT: @_spi(libBar) public func spi(x: Swift.Int, y: Swift.String)
// CHECK-PUB-NOT: package func pkg(x: Swift.Int, y: Swift.String)
// CHECK-PUB-NOT: internal func int(x: Swift.Int, y: Swift.String)
// CHECK-PUB-NOT: private func priv(x: Swift.Int, y: Swift.String)
// CHECK-PUB: public func pub(x: Swift.Int, y: Swift.String)
// CHECK-PUB: @usableFromInline
// CHECK-PUB: package func ufipkg(x: Swift.Int, y: Swift.String)

// RUN: %FileCheck --check-prefixes=CHECK-PRIV %s < %t/Lib.private.swiftinterface
// CHECK-PRIV-NOT: package func pkg(x: Swift.Int, y: Swift.String)
// CHECK-PRIV-NOT: internal func int(x: Swift.Int, y: Swift.String)
// CHECK-PRIV-NOT: private func priv(x: Swift.Int, y: Swift.String)
// CHECK-PRIV: public func pub(x: Swift.Int, y: Swift.String)
// CHECK-PRIV: @_spi(libBar) public func spi(x: Swift.Int, y: Swift.String)
// CHECK-PRIV: @usableFromInline
// CHECK-PRIV: package func ufipkg(x: Swift.Int, y: Swift.String)

// RUN: %FileCheck --check-prefixes=CHECK-PKG %s < %t/Lib.package.swiftinterface
// CHECK-PKG-NOT: internal func int(x: Swift.Int, y: Swift.String)
// CHECK-PKG-NOT: private func priv(x: Swift.Int, y: Swift.String)
// CHECK-PKG: public func pub(x: Swift.Int, y: Swift.String)
// CHECK-PKG: @_spi(libBar) public func spi(x: Swift.Int, y: Swift.String)
// CHECK-PKG: @usableFromInline
// CHECK-PKG: package func ufipkg(x: Swift.Int, y: Swift.String)
// CHECK-PKG: package func pkg(x: Swift.Int, y: Swift.String)


//--- Lib.swift

public func pub(x: Int, y: String) {
  print("something")
}

@_spi(libBar)
public func spi(x: Int, y: String) {
  print("something")
}

@usableFromInline
package func ufipkg(x: Int, y: String) {
  print("something")
}

package func pkg(x: Int, y: String) {
  print("something")
}

func int(x: Int, y: String) {
  print("something")
}

private func priv(x: Int, y: String) {
  print("something")
}


//--- Client.swift
#if PkgImport
package import Lib
#elseif SPIImport
@_spiOnly import Lib
#elseif In6Mode
public import Lib
#else
import Lib
#endif


