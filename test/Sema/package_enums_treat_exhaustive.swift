// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Test that resilient package or public enums are treated as exhaustive at the use site
/// outside of the defining module, as long as they are in the same package; a warning
/// should be shown for a non-package client per below.

/// Test accessing resilient enum in module built from source in the same or different package.
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Utils.swift -I %t -swift-version 6 -package-name mypkg -verify

// RUN: %target-swift-frontend -typecheck %t/ClientInPkg.swift -I %t -swift-version 6 -package-name mypkg -verify

// RUN: %target-swift-frontend -emit-sil %t/ClientInPkg.swift -I %t -swift-version 6 -package-name mypkg -O -sil-verify-all

// RUN: %target-swift-frontend -typecheck %t/ClientDiffPkg.swift -I %t -swift-version 6 -package-name diffpkg -verify

// RUN: %target-swift-frontend -emit-sil %t/ClientDiffPkg.swift -I %t -swift-version 6 -package-name diffpkg -O -sil-verify-all

// RUN: rm -rf %t/Utils.swiftmodule

/// Test accessing resilient enum in module built from interface in the same or different package.
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution -swift-version 6 \
// RUN:   -emit-module -emit-module-interface-path %t/Utils.swiftinterface \
// RUN:   -emit-module -emit-package-module-interface-path %t/Utils.package.swiftinterface

// RUN: %target-swift-frontend -compile-module-from-interface %t/Utils.swiftinterface -o %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/ClientDiffPkg.swift -I %t -swift-version 6 -package-name diffpkg -verify

// RUN: %target-swift-frontend -emit-sil %t/ClientDiffPkg.swift -I %t -swift-version 6 -package-name diffpkg -O -sil-verify-all

// RUN: rm -rf %t/Utils.swiftmodule

// RUN: %target-swift-frontend -compile-module-from-interface %t/Utils.package.swiftinterface -o %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/ClientInPkgLoadInterface.swift -I %t -swift-version 6 -package-name mypkg -verify -experimental-package-interface-load

// RUN: %target-swift-frontend -emit-sil %t/ClientInPkgLoadInterface.swift -I %t -swift-version 6 -package-name mypkg -O -sil-verify-all -experimental-package-interface-load

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

//--- ClientInPkgLoadInterface.swift

// Utils is built from public interface containing
// package decls with @usableFromInline. Accessing
// those decls should match the behavior of accessing
// public enums.
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

public func h(_ arg: PublicEnum) -> Int {
  switch arg { // expected-warning {{switch covers known cases, but 'PublicEnum' may have additional unknown values}} {{none}} expected-note {{handle unknown values using "@unknown default"}}
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

@inlinable
public func hi(_ arg: PublicEnum) -> Int {
  switch arg { // expected-warning {{switch covers known cases, but 'PublicEnum' may have additional unknown values}} {{none}} expected-note {{handle unknown values using "@unknown default"}}
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

//--- ClientDiffPkgLoadInterface.swift

// If _not_ in the same package as Utils, accessing resilient
// public enums defined in Utils does require `@unknown default`
// in switch stmts.
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
  switch arg { // expected-warning {{switch covers known cases, but 'PublicEnum' may have additional unknown values}} {{none}} expected-note {{handle unknown values using "@unknown default"}}
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

@inlinable
public func hi(_ arg: PublicEnum) -> Int {
  switch arg { // expected-warning {{switch covers known cases, but 'PublicEnum' may have additional unknown values}} {{none}} expected-note {{handle unknown values using "@unknown default"}}
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
