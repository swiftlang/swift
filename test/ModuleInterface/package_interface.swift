// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build a package swiftinterface.
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -I %t \
// RUN:   -module-name Lib -package-name libPkg \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Lib.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Lib.private.swiftinterface) -I %t -module-name Lib

// RUN: %FileCheck --check-prefixes=CHECK %s < %t/Lib.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK %s < %t/Lib.private.swiftinterface



//--- Lib.swift

public func publicFunc() {
  print("something")
}

@_spi(LibGroup)
public func spiFunc() {
  print("something")
}

package func pkgFunc() {
  print("something")
}

func internalFunc() {
  print("something")
}

private func privateFunc() {
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


