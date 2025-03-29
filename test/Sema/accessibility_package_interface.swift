// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name swift-utils.log \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Utils.swiftmodule \
// RUN:   -emit-module-interface-path %t/Utils.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Utils.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Utils.swiftinterface) -I %t
// RUN: %FileCheck %s --check-prefix=CHECK-PUBLIC-UTILS < %t/Utils.swiftinterface

// CHECK-PUBLIC-UTILS-NOT: package func packageFunc()
// CHECK-PUBLIC-UTILS-NOT: package protocol PackageProto
// CHECK-PUBLIC-UTILS-NOT: var pkgVar
// CHECK-PUBLIC-UTILS-NOT: package class PackageKlass
// CHECK-PUBLIC-UTILS-NOT: package var pkgVar
// CHECK-PUBLIC-UTILS: -module-name Utils
// CHECK-PUBLIC-UTILS: -package-name swift-utils.log
// CHECK-PUBLIC-UTILS: public func publicFunc()
// CHECK-PUBLIC-UTILS: @usableFromInline
// CHECK-PUBLIC-UTILS: package func ufiPackageFunc()
// CHECK-PUBLIC-UTILS: @usableFromInline
// CHECK-PUBLIC-UTILS: package protocol UfiPackageProto
// CHECK-PUBLIC-UTILS: var ufiPkgVar
// CHECK-PUBLIC-UTILS: @usableFromInline
// CHECK-PUBLIC-UTILS: package class UfiPackageKlass
// CHECK-PUBLIC-UTILS: @usableFromInline
// CHECK-PUBLIC-UTILS: package var ufiPkgVar

// RUN: %target-swift-typecheck-module-from-interface(%t/Utils.private.swiftinterface) -module-name Utils -I %t
// RUN: %FileCheck %s --check-prefix=CHECK-PRIVATE-UTILS < %t/Utils.private.swiftinterface

// CHECK-PRIVATE-UTILS-NOT: package func packageFunc()
// CHECK-PRIVATE-UTILS-NOT: package protocol PackageProto
// CHECK-PRIVATE-UTILS-NOT: var pkgVar
// CHECK-PRIVATE-UTILS-NOT: package class PackageKlass
// CHECK-PRIVATE-UTILS-NOT: package var pkgVar
// CHECK-PRIVATE-UTILS: -module-name Utils
// CHECK-PRIVATE-UTILS: -package-name swift-utils.log
// CHECK-PRIVATE-UTILS: public func publicFunc()
// CHECK-PRIVATE-UTILS: @usableFromInline
// CHECK-PRIVATE-UTILS: package func ufiPackageFunc()
// CHECK-PRIVATE-UTILS: @usableFromInline
// CHECK-PRIVATE-UTILS: package protocol UfiPackageProto
// CHECK-PRIVATE-UTILS: var ufiPkgVar
// CHECK-PRIVATE-UTILS: @usableFromInline
// CHECK-PRIVATE-UTILS: package class UfiPackageKlass
// CHECK-PRIVATE-UTILS: @usableFromInline
// CHECK-PRIVATE-UTILS: package var ufiPkgVar

// RUN: %target-swift-frontend -emit-module %t/Client.swift \
// RUN:   -module-name Client -swift-version 5 -I %t \
// RUN:   -package-name swift-utils.log \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module-path %t/Client.swiftmodule \
// RUN:   -emit-module-interface-path %t/Client.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Client.private.swiftinterface

// RUN: rm -rf %t/Utils.swiftmodule
// RUN: rm -rf %t/Client.swiftmodule

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t -verify
// RUN: %FileCheck %s --check-prefix=CHECK-PUBLIC-CLIENT < %t/Client.swiftinterface
// CHECK-PUBLIC-CLIENT: -package-name swift-utils.log
// CHECK-PUBLIC-CLIENT: @inlinable public func clientFunc()
// CHECK-PUBLIC-CLIENT: publicFunc()
// CHECK-PUBLIC-CLIENT: ufiPackageFunc()
// CHECK-PUBLIC-CLIENT: let u = UfiPackageKlass()
// CHECK-PUBLIC-CLIENT: return u.ufiPkgVar
// CHECK-PUBLIC-CLIENT: public class ClientKlass1 : Utils.UfiPackageProto
// CHECK-PUBLIC-CLIENT: @usableFromInline
// CHECK-PUBLIC-CLIENT: package var ufiPkgVar: Swift.String
// CHECK-PUBLIC-CLIENT: public class ClientKlass2 : Utils.UfiPackageProto
// CHECK-PUBLIC-CLIENT: public var ufiPkgVar: Swift.String

// RUN: %target-swift-typecheck-module-from-interface(%t/Client.private.swiftinterface) -module-name Client -I %t -verify


//--- Utils.swift
public func publicFunc() {}

package func packageFunc() {}
@usableFromInline
package func ufiPackageFunc() {}

package protocol PackageProto {
  var pkgVar: String { get set }
}
package class PackageKlass: PackageProto {
  package var pkgVar = ""
}

@usableFromInline
package protocol UfiPackageProto {
  var ufiPkgVar: String { get set }
}

@usableFromInline
package class UfiPackageKlass: UfiPackageProto {
  @usableFromInline
  package init() {}
  @usableFromInline
  package var ufiPkgVar = ""
}


//--- Client.swift
import Utils

@inlinable public func clientFunc() -> String {
  publicFunc()
  ufiPackageFunc()
  let u = UfiPackageKlass()
  return u.ufiPkgVar
}

public class ClientKlass1: UfiPackageProto {
  @usableFromInline
  package var ufiPkgVar = "B"
}

public class ClientKlass2: UfiPackageProto {
  public var ufiPkgVar = "C"
}
