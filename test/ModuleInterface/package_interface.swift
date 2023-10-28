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

// RUN: %FileCheck %s < %t/Lib.swiftinterface
// CHECK-NOT: @_spi(libBar) public func spi(x: Swift.Int, y: Swift.String)
// CHECK-NOT: package func pkg(x: Swift.Int, y: Swift.String)
// CHECK-NOT: internal func int(x: Swift.Int, y: Swift.String)
// CHECK-NOT: private func priv(x: Swift.Int, y: Swift.String)

// RUN: %FileCheck --check-prefixes=CHECK,CHECK-PRIV %s < %t/Lib.private.swiftinterface
// CHECK-PRIV-NOT: package func pkg(x: Swift.Int, y: Swift.String)
// CHECK-PRIV-NOT: internal func int(x: Swift.Int, y: Swift.String)
// CHECK-PRIV-NOT: private func priv(x: Swift.Int, y: Swift.String)

// RUN: %FileCheck --check-prefixes=CHECK,CHECK-PKG %s < %t/Lib.package.swiftinterface
// CHECK-PKG-NOT: internal func int(x: Swift.Int, y: Swift.String)
// CHECK-PKG-NOT: private func priv(x: Swift.Int, y: Swift.String)


// RUN: %target-swift-frontend -compile-module-from-interface %t/Lib.package.swiftinterface -o %t/Lib.swiftmodule -module-name Lib
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t \
// RUN:   -package-name libPkg \
// RUN:   -Rmodule-loading 2> %t/loadresult.output
// RUN: %FileCheck -check-prefix=CHECK-DEP %s < %t/loadresult.output
// CHECK-DEP: loaded module 'Lib'; source:{{.*}}/Lib.package.swiftinterface', loaded: {{.*}}/Lib.swiftmodule

//--- Lib.swift

public class PubKlass {
  public var v1: String {
    didSet {}
  }
  var v2: String {
    didSet {}
  }
  package var v3: String
  public private(set) var v4: Int

  public init() {
    v1 = ""
    v2 = ""
    v3 = ""
    v4 = 1
  }
}
// CHECK: public class PubKlass {
// CHECK:   public var v1: Swift.String {
// CHECK:     get
// CHECK:     set
// CHECK:   }
// CHECK:   public var v4: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK:   public init()
// CHECK:   @objc deinit
// CHECK: }

class IntKlass {
  var intvar: String {
    didSet {}
  }
  init() { intvar = "" }
}

package class PkgKlass {
  package var q1: String {
    didSet {}
  }
  package private(set) var r1: Int
  package internal(set) var r2: Double

  package init() {
    q1 = ""
    r1 = 2
    r2 = 2.0
  }
}
// CHECk-PKG: package class PkgKlass {
// CHECk-PKG:   @_hasStorage package var q1: Swift.String {
// CHECk-PKG:     get
// CHECk-PKG:     set
// CHECk-PKG:   }
// CHECk-PKG:   @_hasStorage package var r1: Swift.Int {
// CHECk-PKG:     get
// CHECk-PKG:   }
// CHECk-PKG:   @_hasStorage package var r2: Swift.Double {
// CHECk-PKG:     get
// CHECk-PKG:   }
// CHECk-PKG:   package init()
// CHECk-PKG:   @objc deinit
// CHECk-PKG: }

public func pub(x: Int, y: String) { print("pub func") }
// CHECK: public func pub(x: Swift.Int, y: Swift.String)

@_spi(libBar)
public func spi(x: Int, y: String) { print("spi func") }
// CHECK-PRIV: @_spi(libBar) public func spi(x: Swift.Int, y: Swift.String)
// CHECK-PKG: @_spi(libBar) public func spi(x: Swift.Int, y: Swift.String)

@usableFromInline
package func ufipkg(x: Int, y: String) { print("ufi pkg func") }
// CHECK: @usableFromInline
// CHECK: package func ufipkg(x: Swift.Int, y: Swift.String)

package func pkg(x: Int, y: String) { print("pkg func") }
// CHECK-PKG: package func pkg(x: Swift.Int, y: Swift.String)

func int(x: Int, y: String) { print("int func") }

private func priv(x: Int, y: String) { print("priv func") }


//--- Client.swift
import Lib

func client() {
  let p = PkgKlass()
  print(p)
  pkg(x: 1, y: "")
}
