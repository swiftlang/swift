// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Generate a package swiftinterface. Note -package-name is repeated; the last value should be picked.
// RUN: %target-swift-frontend -emit-module %t/Bar.swift -I %t \
// RUN:   -module-name Bar -package-name foopkg -package-name barpkg \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -emit-module-interface-path %t/Bar.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Bar.private.swiftinterface \
// RUN:   -emit-package-module-interface-path %t/Bar.package.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t/Bar.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Bar.package.swiftinterface) -I %t -module-name Bar
// RUN: %target-swift-typecheck-module-from-interface(%t/Bar.private.swiftinterface) -I %t -module-name Bar

// RUN: %FileCheck --check-prefixes=CHECK %s < %t/Bar.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK,CHECK-PRIV %s < %t/Bar.private.swiftinterface
// RUN: %FileCheck --check-prefixes=CHECK,CHECK-PRIV,CHECK-PKG %s < %t/Bar.package.swiftinterface


//--- Bar.swift
public enum PubEnum {
  case red, green
}

// CHECK: -package-name barpkg
// CHECK: public enum PubEnum {
// CHECK:   case red, green
// CHECK:   public static func == (a: Bar.PubEnum, b: Bar.PubEnum) -> Swift.Bool
// CHECK:   public func hash(into hasher: inout Swift.Hasher)
// CHECK:   public var hashValue: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK: }

@_transparent
public func pubFunc(arg: PubEnum) {
  switch arg {
  case .red:
    print("r")
  case .green:
    print("g")
  @unknown default:
    print("def")
  }
}

// CHECK: @_transparent public func pubFunc(arg: Bar.PubEnum) {
// CHECK:   switch arg {
// CHECK:   case .red:
// CHECK:     print("r")
// CHECK:   case .green:
// CHECK:     print("g")
// CHECK:   @unknown default:
// CHECK:     print("def")
// CHECK:   }
// CHECK: }

package enum PkgEnum {
  case blue, yellow
}

// CHECK-PKG: package enum PkgEnum {
// CHECK-PKG:   case blue, yellow
// CHECK-PKG:   package static func == (a: Bar.PkgEnum, b: Bar.PkgEnum) -> Swift.Bool
// CHECK-PKG:   package func hash(into hasher: inout Swift.Hasher)
// CHECK-PKG:   package var hashValue: Swift.Int {
// CHECK-PKG:     get
// CHECK-PKG:   }
// CHECK-PKG: }


@usableFromInline
package enum UfiPkgEnum {
  case one, two
}

// CHECK: @usableFromInline
// CHECK: package enum UfiPkgEnum {
// CHECK:   case one, two
// CHECK:   @usableFromInline
// CHECK:   package static func == (a: Bar.UfiPkgEnum, b: Bar.UfiPkgEnum) -> Swift.Bool
// CHECK:   @usableFromInline
// CHECK:   package func hash(into hasher: inout Swift.Hasher)
// CHECK:   @usableFromInline
// CHECK:   package var hashValue: Swift.Int {
// CHECK:     @usableFromInline
// CHECK:     get
// CHECK:   }
// CHECK: }

@inlinable
package func pkgFunc(arg: UfiPkgEnum) {
  switch arg {
  case .one:
    print("1")
  case .two:
    print("2")
  @unknown default:
    print("def")
  }
}
// CHECK: @inlinable package func pkgFunc(arg: Bar.UfiPkgEnum) {
// CHECK:   switch arg {
// CHECK:   case .one:
// CHECK:     print("1")
// CHECK:   case .two:
// CHECK:     print("2")
// CHECK:   @unknown default:
// CHECK:     print("def")
// CHECK:   }
// CHECK: }

@frozen public struct FrozenPub {
  public var one: String
  var two: String // expected to be printed in public .swiftinterface since contained in @frozen
  private var three: String // expected to be printed in public .swiftinterface since contained in @frozen
  public private(set) var four: String
  public package(set) var five: String
}
// CHECK: @frozen public struct FrozenPub {
// CHECK:   public var one: Swift.String
// CHECK:   internal var two: Swift.String
// CHECK:   private var three: Swift.String
// CHECK:   @_hasStorage public var four: Swift.String {
// CHECK:     get
// CHECK:   }
// CHECK:   @_hasStorage public var five: Swift.String {
// CHECK:     get
// CHECK:   }
// CHECK: }

package struct PkgStruct {
  package var one: String
  var two: String // expected be printed in package.swiftinterface since contained in non-resilient type
  private var three: String // expected be printed in package.swiftinterface since contained in non-resilient type
  package private(set) var four: String
}

// CHECK-PKG: package struct PkgStruct {
// CHECK-PKG:   package var one: Swift.String
// CHECK-PKG:   package var four: Swift.String {
// CHECK-PKG:     get
// CHECK-PKG:   }
// CHECK-PKG: }

public class PubKlass {
  public var pubVarInPub: String
  var intrnVarInPub: String
  package var pkgVarInPub: String
  public private(set) var pubVarPrivSetInPub: Int
  public package(set) var pubVarPkgSetInPub: Int

  public init() {
    pubVarInPub = ""
    intrnVarInPub = ""
    pkgVarInPub = ""
    pubVarPrivSetInPub = 1
    pubVarPkgSetInPub = 1
  }
}

// CHECK: public class PubKlass {
// CHECK:   public var pubVarInPub: Swift.String
// CHECK-PKG:   package var pkgVarInPub: Swift.String
// CHECK:   public var pubVarPrivSetInPub: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK:   public var pubVarPkgSetInPub: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK:   public init()
// CHECK:   deinit
// CHECK: }

class IntrnKlass {
  var intrnVarInIntrn: String
  init() { intrnVarInIntrn = "" }
}

package class PkgKlass {
  package var pkgVarInPkg: String
  var intrnVarInPkg: String
  package private(set) var pkgVarPrivSetInPkg: Int

  package init() {
    pkgVarInPkg = ""
    intrnVarInPkg = ""
    pkgVarPrivSetInPkg = 1
  }
}

// CHECK-PKG: package class PkgKlass {
// CHECK-PKG:   package var pkgVarInPkg: Swift.String
// CHECK-PKG:   package var pkgVarPrivSetInPkg: Swift.Int {
// CHECK-PKG:     get
// CHECK-PKG:   }
// CHECK-PKG:   package init()
// CHECK-PKG:   deinit
// CHECK-PKG: }

public protocol PubProto {
  var p1: String { get set }
  func f1()
}

// CHECK: public protocol PubProto {
// CHECK:   var p1: Swift.String { get set }
// CHECK:   func f1()
// CHECK: }

public extension PubProto {
  var p2: Int { return 2 }
  func f1() { print("f1 ext") }

  @_spi(PubExt)
  func f2() { print("f2") }
}

// CHECK: extension Bar.PubProto {
// CHECK:   public var p2: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK:   public func f1()
// CHECK-PRIV:   @_spi(PubExt) public func f2()
// CHECK: }

extension PubProto {
  public var p3: Int { return 3 }
  package var p4: Int { return 4 }
  var p5: Int { return 5 }
  public func f3() { print("f3") } // expected to show up in public interface
}

// CHECK: extension Bar.PubProto {
// CHECK:   public var p3: Swift.Int {
// CHECK:     get
// CHECK:   }
// CHECK-PKG:   package var p4: Swift.Int {
// CHECK-PKG:     get
// CHECK-PKG:   }
// CHECK:   public func f3()
// CHECK: }

package protocol PkgProto {
  var k1: String { get set }
  func g1()
}
// CHECK-PKG: package protocol PkgProto {
// CHECK-PKG:   var k1: Swift.String { get set }
// CHECK-PKG:   func g1()
// CHECK-PKG: }

package extension PkgProto {
  var k2: Int { return 2 }
  func g1() { print("g1 ext") }
  func g2() { print("g2") }
}
// CHECK-PKG: extension Bar.PkgProto {
// CHECK-PKG:   package var k2: Swift.Int {
// CHECK-PKG:     get
// CHECK-PKG:   }
// CHECK-PKG:   package func g1()
// CHECK-PKG:   package func g2()
// CHECK-PKG: }

extension PkgProto {
  package var k3: Int { return 3 }
  var k4: Int { return 4 }
  package func g3() { print("g3") }
}

// CHECK-PKG: extension Bar.PkgProto {
// CHECK-PKG:   package var k3: Swift.Int {
// CHECK-PKG:     get
// CHECK-PKG:   }
// CHECK-PKG:   package func g3()
// CHECK-PKG: }

public func pub(x: Int, y: String) { print("pub func") }
// CHECK: public func pub(x: Swift.Int, y: Swift.String)

@_spi(LibBar) public func spi(x: Int, y: String) { print("spi func") }
// CHECK-PRIV: @_spi(LibBar) public func spi(x: Swift.Int, y: Swift.String)

@usableFromInline
package func ufipkg(x: Int, y: String) { print("ufi pkg func") }
// CHECK: @usableFromInline
// CHECK: package func ufipkg(x: Swift.Int, y: Swift.String)

package func pkg(x: Int, y: String) { print("pkg func") }
// CHECK-PKG: package func pkg(x: Swift.Int, y: Swift.String)

func int(x: Int, y: String) { print("int func") }
private func priv(x: Int, y: String) { print("priv func") }

// CHECK: extension Bar.PubEnum : Swift.Equatable {}
// CHECK: extension Bar.PubEnum : Swift.Hashable {}
// CHECK: extension Bar.FrozenPub : Swift.Sendable {}

