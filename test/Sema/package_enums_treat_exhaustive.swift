// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Test that resilient package or public enums are treated as exhaustive at the use site
/// outside of the defining module, as long as they are in the same package.

/// Test accessing resilient enum in module (built from source) in the same or different package.
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Utils.swift -I %t -swift-version 6 -package-name mypkg -verify
// RUN: %target-swift-frontend -typecheck %t/ClientInPkg.swift -I %t -swift-version 6 -package-name mypkg -verify
// RUN: %target-swift-frontend -emit-sil %t/ClientInPkg.swift -I %t -swift-version 6 -package-name mypkg -sil-verify-all -o /dev/null
// RUN: %target-swift-frontend -emit-sil %t/ClientInPkg.swift -I %t -swift-version 6 -package-name mypkg -O -sil-verify-all -o /dev/null
// RUN: %target-swift-frontend -typecheck %t/ClientDiffPkg.swift -I %t -swift-version 6 -package-name diffpkg -verify

// RUN: rm -rf %t/Utils.swiftmodule

/// Test accessing resilient enum in module (built from source) with package optimization in the same or different package.
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -I %t \
// RUN:   -package-name mypkg \
// RUN:   -allow-non-resilient-access -package-cmo \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Utils.swift -I %t -swift-version 6 -package-name mypkg -verify
// RUN: %target-swift-frontend -typecheck %t/ClientInPkg.swift -I %t -swift-version 6 -package-name mypkg -verify
// RUN: %target-swift-frontend -emit-sil %t/ClientInPkg.swift -I %t -swift-version 6 -package-name mypkg -sil-verify-all -o /dev/null
// RUN: %target-swift-frontend -emit-sil %t/ClientInPkg.swift -I %t -swift-version 6 -package-name mypkg -O -wmo -sil-verify-all -o /dev/null
// RUN: %target-swift-frontend -typecheck %t/ClientDiffPkg.swift -I %t -swift-version 6 -package-name diffpkg -verify

// RUN: rm -rf %t/Utils.swiftmodule

/// Test accessing resilient enum in module (built from interface) in the same or different package.
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -emit-module -emit-module-interface-path %t/Utils.swiftinterface \
// RUN:   -emit-module -emit-package-module-interface-path %t/Utils.package.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface %t/Utils.swiftinterface -o %t/Utils.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/ClientInPkgLoadPublicInterface.swift -I %t -swift-version 6 -package-name mypkg -verify -experimental-package-interface-load
// RUN: %target-swift-frontend -typecheck %t/ClientDiffPkg.swift -I %t -swift-version 6 -package-name diffpkg -verify
// RUN: rm -rf %t/Utils.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/Utils.package.swiftinterface -o %t/Utils.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/ClientInPkgLoadPackageInterface.swift -I %t -swift-version 6 -package-name mypkg -verify -experimental-package-interface-load
// RUN: %target-swift-frontend -emit-sil %t/ClientInPkgLoadPackageInterface.swift -I %t -swift-version 6 -package-name mypkg -sil-verify-all -experimental-package-interface-load -o /dev/null
// RUN: %target-swift-frontend -emit-sil %t/ClientInPkgLoadPackageInterface.swift -I %t -swift-version 6 -package-name mypkg -O -sil-verify-all -experimental-package-interface-load -o /dev/null

//--- Utils.swift

package enum PkgEnum {
  case one
  case two(Int)
}

@frozen
package enum FrozenPkgEnum {
  case one
  case two(Int)
}

@frozen
@usableFromInline
package enum FrozenUfiPkgEnum {
  case one
  case two(Int)
}

@usableFromInline
package enum UfiPkgEnum {
  case one
  case two(Int)
}

public enum PublicEnum {
  case one
  case two(Int)
}

@frozen
public enum FrozenPublicEnum {
  case one
  case two(Int)
}

package func uf(_ arg: PkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

package func um(_ arg: FrozenPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

package func un(_ arg: FrozenUfiPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

package func ug(_ arg: UfiPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

public func uh(_ arg: PublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  @unknown default: // Adding this unnecessary line should not cause an error or warning.
    return 3
  }
}

public func uk(_ arg: FrozenPublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

//--- ClientInPkg.swift

// If in the same package as Utils, accessing resilient public
// or package enums defined in Utils no longer requires `@unknown
// default` in switch stmts.
import Utils

package func f(_ arg: PkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

package func m(_ arg: FrozenPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

package func n(_ arg: FrozenUfiPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

package func g(_ arg: UfiPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

@inlinable
package func gi(_ arg: UfiPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

public func h(_ arg: PublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  @unknown default: // Adding this unnecessary line should not cause an error or warning.
    return 3
  }
}

@inlinable
public func hi(_ arg: PublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

public func k(_ arg: FrozenPublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

//--- ClientInPkgLoadPackageInterface.swift

// Utils is built from package interface. Accessing
// resilient enums from Utils should not require
// @unknown default.
import Utils

package func n(_ arg: FrozenUfiPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

package func g(_ arg: UfiPkgEnum) -> Int {
  switch arg {
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  @unknown default:
    return 3
  }
}

@inlinable
package func gi(_ arg: UfiPkgEnum) -> Int {
  switch arg {
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

public func h(_ arg: PublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  @unknown default:
    return 3
  }
}

@inlinable
public func hi(_ arg: PublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

public func k(_ arg: FrozenPublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

//--- ClientInPkgLoadPublicInterface.swift

// Utils is built from public interface. Accessing
// resilient enums from Utils should not require
// @unknown default.
import Utils

package func n(_ arg: FrozenUfiPkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

package func g(_ arg: UfiPkgEnum) -> Int {
  switch arg {
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  @unknown default:
    return 3
  }
}

@inlinable
package func gi(_ arg: UfiPkgEnum) -> Int {
  switch arg {
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

public func h(_ arg: PublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  @unknown default:
    return 3
  }
}

@inlinable
public func hi(_ arg: PublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

public func k(_ arg: FrozenPublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

//--- ClientDiffPkg.swift

// If _not_ in the same package as Utils, accessing resilient
// public enums defined in Utils does require `@unknown default`
// in switch stmts.
import Utils

public func x(_ arg: PublicEnum) -> Int {
  switch arg { // expected-error {{switch covers known cases, but 'PublicEnum' may have additional unknown values}} expected-note {{handle unknown values using "@unknown default"}}
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

@inlinable
public func y(_ arg: PublicEnum) -> Int {
  switch arg { // expected-error {{switch covers known cases, but 'PublicEnum' may have additional unknown values}} expected-note {{handle unknown values using "@unknown default"}}
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

public func z(_ arg: FrozenPublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}
