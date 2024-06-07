// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -experimental-allow-non-resilient-access \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Utils.swift -I %t -swift-version 5 -package-name mypkg -verify

// RUN: %target-swift-frontend -typecheck %t/ClientDefault.swift -I %t -swift-version 5 -package-name mypkg -verify

// RUN: %target-swift-frontend -typecheck %t/ClientOptimized.swift -I %t -swift-version 5 -package-name mypkg -experimental-package-bypass-resilience -verify

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


//--- ClientDefault.swift
import Utils

package func f(_ arg: PkgEnum) -> Int {
  switch arg { // expected-warning {{switch covers known cases, but 'PkgEnum' may have additional unknown values}} {{none}} expected-note {{handle unknown values using "@unknown default"}}
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
  switch arg { // expected-warning {{switch covers known cases, but 'UfiPkgEnum' may have additional unknown values}} {{none}} expected-note {{handle unknown values using "@unknown default"}}
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

public func k(_ arg: FrozenPublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}


//--- ClientOptimized.swift
import Utils

// With optimization enabled to bypass resilience checks within
// a package boundary, public (non-frozen) or package (non-frozen)
// enums no longer require `@unknown default` in switch stmts.
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

public func h(_ arg: PublicEnum) -> Int {
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
