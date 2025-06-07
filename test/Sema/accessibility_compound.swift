// RUN: %target-typecheck-verify-swift -swift-version 4 -package-name mypkg

public struct Pair<A, B> {}

public struct PublicStruct {
  public struct Inner {}
  package struct PkgInner {}
  internal struct Internal {} // expected-note * {{type declared here}}
}

private typealias PrivateAlias = PublicStruct // expected-note * {{type declared here}}
package typealias PackageAlias = PublicStruct // expected-note * {{type declared here}}

public let a0 = nil as PrivateAlias.Inner? // expected-error {{constant cannot be declared public because its type 'PrivateAlias.Inner?' (aka 'Optional<PublicStruct.Inner>') uses a private type}}
public let a: PrivateAlias.Inner? // expected-error {{constant cannot be declared public because its type uses a private type}}
public let b: PrivateAlias.Internal? // expected-error {{constant cannot be declared public because its type uses a private type}}
public let c: Pair<PrivateAlias.Inner, PublicStruct.Internal>? // expected-error {{constant cannot be declared public because its type uses a private type}}
public let c2: Pair<PublicStruct.Internal, PrivateAlias.Inner>? // expected-error {{constant cannot be declared public because its type uses a private type}}
public let d: PrivateAlias? // expected-error {{constant cannot be declared public because its type uses a private type}}

package let e = nil as PrivateAlias.Inner? // expected-error {{constant cannot be declared package because its type 'PrivateAlias.Inner?' (aka 'Optional<PublicStruct.Inner>') uses a private type}}
package let f: PrivateAlias.Inner? // expected-error {{constant cannot be declared package because its type uses a private type}}
package let g: PrivateAlias.PkgInner? // expected-error {{constant cannot be declared package because its type uses a private type}}
package let h: Pair<PrivateAlias.PkgInner, PublicStruct.Inner>? // expected-error {{constant cannot be declared package because its type uses a private type}}
package let i: Pair<PublicStruct.PkgInner, PrivateAlias.Inner>? // expected-error {{constant cannot be declared package because its type uses a private type}}
package let j: PrivateAlias? // expected-error {{constant cannot be declared package because its type uses a private type}}

public let k = nil as PackageAlias.Inner? // expected-error {{constant cannot be declared public because its type 'PackageAlias.Inner?' (aka 'Optional<PublicStruct.Inner>') uses a package type}}
public let l: PackageAlias.Inner? // expected-error {{constant cannot be declared public because its type uses a package type}}
public let m: PackageAlias.Internal? // expected-error {{constant cannot be declared public because its type uses an internal type}}
public let n: Pair<PackageAlias.Inner, PublicStruct.Internal>? // expected-error {{constant cannot be declared public because its type uses an internal type}}
public let o: Pair<PublicStruct.Internal, PackageAlias.Inner>? // expected-error {{constant cannot be declared public because its type uses an internal type}}
public let p: PackageAlias? // expected-error {{constant cannot be declared public because its type uses a package type}}

package let q = nil as PackageAlias.Inner? // no-error
package let r: PackageAlias.Inner? // no-error
package let s: PackageAlias.Internal? // expected-error {{constant cannot be declared package because its type uses an internal type}}
package let t: Pair<PackageAlias.Inner, PublicStruct.Internal>? // expected-error {{constant cannot be declared package because its type uses an internal type}}
package let u: Pair<PublicStruct.Internal, PackageAlias.Inner>? // expected-error {{constant cannot be declared package because its type uses an internal type}}
package let v: PackageAlias? // no-error

// rdar://problem/21408035
private class PrivateBox<T> { // expected-note 2 {{type declared here}}
  typealias ValueType = T
  typealias AlwaysFloat = Float
}

let boxUnboxInt: PrivateBox<Int>.ValueType = 0 // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}
let boxFloat: PrivateBox<Int>.AlwaysFloat = 0 // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}

private protocol PrivateProto {
  associatedtype Inner
}
extension PublicStruct: PrivateProto {}

private class SpecificBox<T: PrivateProto> { // expected-note 2 {{type declared here}}
  typealias InnerType = T.Inner
  typealias AlwaysFloat = Float
}

let specificBoxUnboxInt: SpecificBox<PublicStruct>.InnerType = .init() // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}
let specificBoxFloat: SpecificBox<PublicStruct>.AlwaysFloat = 0 // expected-error {{constant must be declared private or fileprivate because its type uses a private type}}

