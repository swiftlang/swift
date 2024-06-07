// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -module-name Lib -swift-version 6 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/ClientA.swift -I %t -swift-version 6 -package-name mypkg -enable-library-evolution -verify
// RUN: %target-swift-frontend -typecheck %t/ClientB.swift -I %t -swift-version 6 -package-name mypkg -enable-library-evolution -verify
// RUN: %target-swift-frontend -typecheck %t/ClientC.swift -I %t -swift-version 6 -package-name mypkg -enable-library-evolution -verify
// RUN: %target-swift-frontend -typecheck %t/ClientD.swift -I %t -swift-version 6 -package-name mypkg -enable-library-evolution -verify
// RUN: %target-swift-frontend -typecheck %t/ClientE.swift -I %t -swift-version 6 -package-name mypkg -enable-library-evolution -verify
// RUN: %target-swift-frontend -typecheck %t/ClientF.swift -I %t -swift-version 6 -package-name mypkg -enable-library-evolution -verify

//--- ClientA.swift

@_implementationOnly import Lib // expected-warning {{'@_implementationOnly' is deprecated, use 'internal import' instead}}

public func f() -> PubProto? { // expected-error {{cannot use protocol 'PubProto' here; 'Lib' has been imported as implementation-only}}
  return nil
}
package func g() -> PkgProto? { // expected-error {{cannot use protocol 'PkgProto' here; 'Lib' has been imported as implementation-only}}
  return nil
}

//--- ClientB.swift
package import Lib // no-warning

extension PkgStruct {
  package static var v: Self {
    fatalError()
  }
}

//--- ClientC.swift
package import Lib // no-warning

extension PkgStruct {
  package func f() {}
}

//--- ClientD.swift
package import Lib // no-warning

package extension PubStruct {
  func f() {}
}

//--- ClientE.swift
package import Lib

package enum FeatureFlag: PubProto { // no-warning
  case myFeatureFlag

  package var domain: StaticString { "MyDomain" }
  package var feature: StaticString { "MyFeature" }

  package var someVar: String { "" }
}

package struct MyStruct: PubProto { // no-warning
  package var someVar: String { "" }
}

//--- ClientF.swift
package struct PkgStruct {}
public protocol PubProto {
  associatedtype CodeUnit
}

extension PkgStruct {
  @frozen
  package enum ASCII {}
}

extension PkgStruct.ASCII: PubProto {
  package typealias CodeUnit = UInt8
}

//--- Lib.swift

// expected-note@+1 1{{type declared here}}
public protocol PubProto {
  var someVar: String { get }
}

// expected-note@+1 1{{type declared here}}
package protocol PkgProto {
  var someVar: String { get }
}

public struct PubStruct {
  public init() {}
}

package struct PkgStruct {
  package init() {}
}

public class PubKlass {
  public init() {}
}

package class PkgKlass {
  package init() {}
}
