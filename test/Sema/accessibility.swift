// RUN: %target-typecheck-verify-swift -enable-objc-interop -disable-objc-attr-requires-foundation-module -swift-version 5 -package-name mylib

/// Test structs with protocols and extensions
public protocol PublicProto {
  func publicReq()
}

// expected-note@+1 * {{type declared here}}
package protocol PackageProto {
  func packageReq()
}

// expected-note@+1 * {{type declared here}}
internal protocol InternalProto {
  func internalReq()
}

fileprivate protocol FilePrivateProto {
  func filePrivateReq()
}

// expected-note@+1 * {{type declared here}}
private protocol PrivateProto {
  func privateReq()
}

public struct PublicStruct: PublicProto, PackageProto, InternalProto, FilePrivateProto, PrivateProto {
  private func publicReq() {} // expected-error {{method 'publicReq()' must be declared public because it matches a requirement in public protocol 'PublicProto'}} {{none}} expected-note {{mark the instance method as 'public' to satisfy the requirement}} {{3-10=public}}
  private func packageReq() {} // expected-error {{method 'packageReq()' must be declared package because it matches a requirement in package protocol 'PackageProto'}} {{none}} expected-note {{mark the instance method as 'package' to satisfy the requirement}} {{3-10=package}}
  private func internalReq() {} // expected-error {{method 'internalReq()' must be declared internal because it matches a requirement in internal protocol 'InternalProto'}} {{none}} expected-note {{mark the instance method as 'internal' to satisfy the requirement}} {{3-10=internal}}
  private func filePrivateReq() {} // expected-error {{method 'filePrivateReq()' must be declared fileprivate because it matches a requirement in fileprivate protocol 'FilePrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func privateReq() {} // expected-error {{method 'privateReq()' must be declared fileprivate because it matches a requirement in private protocol 'PrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}

  public var publicVar = 0
}

// expected-note@+1 * {{type declared here}}
package struct PackageStruct: PublicProto, PackageProto, InternalProto, FilePrivateProto, PrivateProto {
  private func publicReq() {} // expected-error {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{none}} expected-note {{mark the instance method as 'package' to satisfy the requirement}} {{3-10=package}}
  private func packageReq() {} // expected-error {{method 'packageReq()' must be declared package because it matches a requirement in package protocol 'PackageProto'}} {{none}} expected-note {{mark the instance method as 'package' to satisfy the requirement}} {{3-10=package}}
  private func internalReq() {} // expected-error {{method 'internalReq()' must be declared internal because it matches a requirement in internal protocol 'InternalProto'}} {{none}} expected-note {{mark the instance method as 'internal' to satisfy the requirement}} {{3-10=internal}}
  private func filePrivateReq() {} // expected-error {{method 'filePrivateReq()' must be declared fileprivate because it matches a requirement in fileprivate protocol 'FilePrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func privateReq() {} // expected-error {{method 'privateReq()' must be declared fileprivate because it matches a requirement in private protocol 'PrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}

  public var publicVar = 0
}

// expected-note@+1 * {{type declared here}}
internal struct InternalStruct: PublicProto, PackageProto, InternalProto, FilePrivateProto, PrivateProto {
  private func publicReq() {} // expected-error {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{none}} expected-note {{mark the instance method as 'internal' to satisfy the requirement}} {{3-10=internal}}
  private func packageReq() {} // expected-error {{method 'packageReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PackageProto'}} {{none}} expected-note {{mark the instance method as 'internal' to satisfy the requirement}} {{3-10=internal}}
  private func internalReq() {} // expected-error {{method 'internalReq()' must be declared internal because it matches a requirement in internal protocol 'InternalProto'}} {{none}} expected-note {{mark the instance method as 'internal' to satisfy the requirement}} {{3-10=internal}}
  private func filePrivateReq() {} // expected-error {{method 'filePrivateReq()' must be declared fileprivate because it matches a requirement in fileprivate protocol 'FilePrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func privateReq() {} // expected-error {{method 'privateReq()' must be declared fileprivate because it matches a requirement in private protocol 'PrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}

  public var publicVar = 0
}

// expected-note@+1 * {{type declared here}}
fileprivate struct FilePrivateStruct: PublicProto, PackageProto, InternalProto, FilePrivateProto, PrivateProto {
  private func publicReq() {} // expected-error {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func packageReq() {} // expected-error {{method 'packageReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PackageProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func internalReq() {} // expected-error {{method 'internalReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'InternalProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func filePrivateReq() {} // expected-error {{method 'filePrivateReq()' must be declared fileprivate because it matches a requirement in fileprivate protocol 'FilePrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func privateReq() {} // expected-error {{method 'privateReq()' must be declared fileprivate because it matches a requirement in private protocol 'PrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}

  public var publicVar = 0
}

// expected-note@+1 * {{type declared here}}
private struct PrivateStruct: PublicProto, PackageProto, InternalProto, FilePrivateProto, PrivateProto {
  private func publicReq() {} // expected-error {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func packageReq() {} // expected-error {{method 'packageReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PackageProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func internalReq() {} // expected-error {{method 'internalReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'InternalProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func filePrivateReq() {} // expected-error {{method 'filePrivateReq()' must be declared fileprivate because it matches a requirement in fileprivate protocol 'FilePrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func privateReq() {} // expected-error {{method 'privateReq()' must be declared fileprivate because it matches a requirement in private protocol 'PrivateProto'}} {{none}} expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}

  public var publicVar = 0
}

public struct PublicStructWithInternalExtension: PublicProto, PackageProto, InternalProto, FilePrivateProto, PrivateProto {}
// expected-error@-1 {{method 'publicReq()' must be declared public because it matches a requirement in public protocol 'PublicProto'}} {{none}}
// expected-error@-2 {{method 'packageReq()' must be declared package because it matches a requirement in package protocol 'PackageProto'}} {{none}}
// expected-error@-3 {{method 'internalReq()' must be declared internal because it matches a requirement in internal protocol 'InternalProto'}} {{none}}
// expected-error@-4 {{method 'filePrivateReq()' must be declared fileprivate because it matches a requirement in fileprivate protocol 'FilePrivateProto'}} {{none}}
// expected-error@-5 {{method 'privateReq()' must be declared fileprivate because it matches a requirement in private protocol 'PrivateProto'}} {{none}}

public struct PublicStructWithPackageExtension: PublicProto, PackageProto, InternalProto, FilePrivateProto, PrivateProto {}
// expected-error@-1 {{method 'publicReq()' must be declared public because it matches a requirement in public protocol 'PublicProto'}} {{none}}
// expected-error@-2 {{method 'packageReq()' must be declared package because it matches a requirement in package protocol 'PackageProto'}} {{none}}
// expected-error@-3 {{method 'internalReq()' must be declared internal because it matches a requirement in internal protocol 'InternalProto'}} {{none}}
// expected-error@-4 {{method 'filePrivateReq()' must be declared fileprivate because it matches a requirement in fileprivate protocol 'FilePrivateProto'}} {{none}}
// expected-error@-5 {{method 'privateReq()' must be declared fileprivate because it matches a requirement in private protocol 'PrivateProto'}} {{none}}

package struct PackageStructWithInternalExtension: PublicProto, PackageProto, InternalProto, FilePrivateProto, PrivateProto {}
// expected-error@-1 {{method 'publicReq()' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicProto'}} {{none}}
// expected-error@-2 {{method 'packageReq()' must be declared package because it matches a requirement in package protocol 'PackageProto'}} {{none}}
// expected-error@-3 {{method 'internalReq()' must be declared internal because it matches a requirement in internal protocol 'InternalProto'}} {{none}}
// expected-error@-4 {{method 'filePrivateReq()' must be declared fileprivate because it matches a requirement in fileprivate protocol 'FilePrivateProto'}} {{none}}
// expected-error@-5 {{method 'privateReq()' must be declared fileprivate because it matches a requirement in private protocol 'PrivateProto'}} {{none}}

package extension PublicStructWithPackageExtension {
  private func publicReq() {} // expected-note {{move the instance method to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  private func packageReq() {} // expected-note {{mark the instance method as 'package' to satisfy the requirement}} {{3-11=}}
  private func internalReq() {} // expected-note {{mark the instance method as 'internal' to satisfy the requirement}} {{3-10=internal}}
  private func filePrivateReq() {} // expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func privateReq() {} // expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
}

internal extension PublicStructWithInternalExtension {
  private func publicReq() {} // expected-note {{move the instance method to another extension where it can be declared 'public' to satisfy the requirement}} {{none}}
  private func packageReq() {} // expected-note {{move the instance method to another extension where it can be declared 'package' to satisfy the requirement}} {{none}}
  private func internalReq() {} // expected-note {{mark the instance method as 'internal' to satisfy the requirement}} {{3-11=}}
  private func filePrivateReq() {} // expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func privateReq() {} // expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
}

internal extension PackageStructWithInternalExtension {
  private func publicReq() {} // expected-note {{move the instance method to another extension where it can be declared 'package' to satisfy the requirement}} {{none}}
  private func packageReq() {} // expected-note {{move the instance method to another extension where it can be declared 'package' to satisfy the requirement}} {{none}}
  private func internalReq() {} // expected-note {{mark the instance method as 'internal' to satisfy the requirement}} {{3-11=}}
  private func filePrivateReq() {} // expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
  private func privateReq() {} // expected-note {{mark the instance method as 'fileprivate' to satisfy the requirement}} {{3-10=fileprivate}}
}


extension PublicStruct {
  public init(x: Int) { self.init() }
}

extension PackageStruct {
  public init(x: Int) { self.init() }
}

extension InternalStruct {
  public init(x: Int) { self.init() }
}

extension FilePrivateStruct {
  public init(x: Int) { self.init() }
}

extension PrivateStruct {
  public init(x: Int) { self.init() }
}

public extension PublicStruct {
  public func extMemberPublic() {} // expected-warning {{'public' modifier is redundant for instance method declared in a public extension}} {{3-10=}}
  package func extMethodPublic() {}
  internal func extDeclPublic() {}
  fileprivate func extFuncPublic() {}
  private func extImplPublic() {}
}
package extension PublicStruct {
  public func extMemberPackage() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'package'}} {{none}}
  package func extMethodPackage() {}  // expected-warning {{'package' modifier is redundant for instance method declared in a package extension}} {{3-11=}}
  internal func extDeclPackage() {}
  fileprivate func extFuncPackage() {}
  private func extImplPackage() {}
}
internal extension PublicStruct {
  public func extMemberInternal() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'internal'}} {{none}}
  package func extMethodInternal() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'internal'}} {{none}}
  internal func extDeclInternal() {} // expected-warning {{'internal' modifier is redundant for instance method declared in an internal extension}} {{3-12=}}
  fileprivate func extFuncInternal() {}
  private func extImplInternal() {}
}
fileprivate extension PublicStruct {
  public func extMemberFilePrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  package func extMethodFilePrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  internal func extDeclFilePrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  fileprivate func extFuncFilePrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a fileprivate extension}} {{3-15=}}
  private func extImplFilePrivate() {}
}
private extension PublicStruct {
  public func extMemberPrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'private'}} {{none}}
  package func extMethodPrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'private'}} {{none}}
  internal func extDeclPrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'private'}} {{none}}
  fileprivate func extFuncPrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a private (equivalent to fileprivate) extension}} {{3-15=}}
  private func extImplPrivate() {}
}
public extension PackageStruct { // expected-error {{extension of package struct cannot be declared public}} {{1-8=}}
  public func extMemberPublic() {}
  package func extMethodPublic() {}
  internal func extDeclPublic() {}
  fileprivate func extFuncPublic() {}
  private func extImplPublic() {}
}
package extension PackageStruct {
  public func extMemberPackage() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'package'}} {{none}}
  package func extMethodPackage() {} // expected-warning {{'package' modifier is redundant for instance method declared in a package extension}} {{3-11=}}
  internal func extDeclPackage() {}
  fileprivate func extFuncPackage() {}
  private func extImplPackage() {}
}
internal extension PackageStruct {
  public func extMemberInternal() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'internal'}} {{none}}
  package func extMethodInternal() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'internal'}} {{none}}
  internal func extDeclInternal() {} // expected-warning {{'internal' modifier is redundant for instance method declared in an internal extension}} {{3-12=}}
  fileprivate func extFuncInternal() {}
  private func extImplInternal() {}
}
fileprivate extension PackageStruct {
  public func extMemberFilePrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  package func extMethodFilePrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  internal func extDeclFilePrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  fileprivate func extFuncFilePrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a fileprivate extension}} {{3-15=}}
  private func extImplFilePrivate() {}
}
private extension PackageStruct {
  public func extMemberPrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'private'}} {{none}}
  package func extMethodPrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'private'}} {{none}}
  internal func extDeclPrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'private'}} {{none}}
  fileprivate func extFuncPrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a private (equivalent to fileprivate) extension}} {{3-15=}}
  private func extImplPrivate() {}
}
public extension InternalStruct { // expected-error {{extension of internal struct cannot be declared public}} {{1-8=}}
  public func extMemberPublic() {}
  package func extMethodPublic() {}
  internal func extDeclPublic() {}
  fileprivate func extFuncPublic() {}
  private func extImplPublic() {}
}
package extension InternalStruct { // expected-error {{extension of internal struct cannot be declared package}} {{1-9=}}
  public func extMemberPackage() {}
  package func extMethodPackage() {}
  internal func extDeclPackage() {}
  fileprivate func extFuncPackage() {}
  private func extImplPackage() {}
}
internal extension InternalStruct {
  public func extMemberInternal() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'internal'}} {{none}}
  package func extMethodInternal() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'internal'}} {{none}}
  internal func extDeclInternal() {}  // expected-warning {{'internal' modifier is redundant for instance method declared in an internal extension}} {{3-12=}}
  fileprivate func extFuncInternal() {}
  private func extImplInternal() {}
}
fileprivate extension InternalStruct {
  public func extMemberFilePrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  package func extMethodFilePrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  internal func extDeclFilePrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  fileprivate func extFuncFilePrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a fileprivate extension}} {{3-15=}}
  private func extImplFilePrivate() {}
}
private extension InternalStruct {
  public func extMemberPrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'private'}} {{none}}
  package func extMethodPrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'private'}} {{none}}
  internal func extDeclPrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'private'}} {{none}}
  fileprivate func extFuncPrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a private (equivalent to fileprivate) extension}} {{3-15=}}
  private func extImplPrivate() {}
}
public extension FilePrivateStruct { // expected-error {{extension of fileprivate struct cannot be declared public}} {{1-8=}}
  public func extMemberPublic() {}
  package func extMethodPublic() {}
  internal func extDeclPublic() {}
  fileprivate func extFuncPublic() {}
  private func extImplPublic() {}
}
package extension FilePrivateStruct { // expected-error {{extension of fileprivate struct cannot be declared package}} {{1-9=}}
  public func extMemberPackage() {}
  package func extMethodPackage() {}
  internal func extDeclPackage() {}
  fileprivate func extFuncPackage() {}
  private func extImplPackage() {}
}
internal extension FilePrivateStruct { // expected-error {{extension of fileprivate struct cannot be declared internal}} {{1-10=}}
  public func extMemberInternal() {}
  package func extMethodInternal() {}
  internal func extDeclInternal() {}
  fileprivate func extFuncInternal() {}
  private func extImplInternal() {}
}
fileprivate extension FilePrivateStruct {
  public func extMemberFilePrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  package func extMethodFilePrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  internal func extDeclFilePrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'fileprivate'}} {{none}}
  fileprivate func extFuncFilePrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a fileprivate extension}} {{3-15=}}
  private func extImplFilePrivate() {}
}
private extension FilePrivateStruct {
  public func extMemberPrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'private'}} {{none}}
  package func extMethodPrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'private'}} {{none}}
  internal func extDeclPrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'private'}} {{none}}
  fileprivate func extFuncPrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a private (equivalent to fileprivate) extension}} {{3-15=}}
  private func extImplPrivate() {}
}
public extension PrivateStruct { // expected-error {{extension of private struct cannot be declared public}} {{1-8=}}
  public func extMemberPublic() {}
  package func extMethodPublic() {}
  internal func extDeclPublic() {}
  fileprivate func extFuncPublic() {}
  private func extImplPublic() {}
}
package extension PrivateStruct { // expected-error {{extension of private struct cannot be declared package}} {{1-9=}}
  public func extMemberPackage() {}
  package func extMethodPackage() {}
  internal func extDeclPackage() {}
  fileprivate func extFuncPackage() {}
  private func extImplPackage() {}
}
internal extension PrivateStruct { // expected-error {{extension of private struct cannot be declared internal}} {{1-10=}}
  public func extMemberInternal() {}
  package func extMethodInternal() {}
  internal func extDeclInternal() {}
  fileprivate func extFuncInternal() {}
  private func extImplInternal() {}
}
fileprivate extension PrivateStruct { // expected-error {{extension of private struct cannot be declared fileprivate}} {{1-13=}}
  public func extMemberFilePrivate() {}
  package func extMethodFilePrivate() {}
  internal func extDeclFilePrivate() {}
  fileprivate func extFuncFilePrivate() {}
  private func extImplFilePrivate() {}
}
private extension PrivateStruct {
  public func extMemberPrivate() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'private'}} {{none}}
  package func extMethodPrivate() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'private'}} {{none}}
  internal func extDeclPrivate() {} // expected-warning {{'internal' modifier conflicts with extension's default access of 'private'}} {{none}}
  fileprivate func extFuncPrivate() {} // expected-warning {{'fileprivate' modifier is redundant for instance method declared in a private (equivalent to fileprivate) extension}} {{3-15=}}
  private func extImplPrivate() {}
}

public struct PublicStructDefaultMethods: PublicProto, PackageProto, InternalProto, PrivateProto {
  func publicReq() {} // expected-error {{method 'publicReq()' must be declared public because it matches a requirement in public protocol 'PublicProto'}} {{none}} expected-note {{mark the instance method as 'public' to satisfy the requirement}} {{3-3=public }}
  func packageReq() {} // expected-error {{method 'packageReq()' must be declared package because it matches a requirement in package protocol 'PackageProto'}} {{none}} expected-note {{mark the instance method as 'package' to satisfy the requirement}} {{3-3=package }}
  func internalReq() {}
  func privateReq() {}
}

/// Test subclassing and overrides
public class Base {
  required public init() {}
  // expected-note@+1 * {{overridden declaration is here}}
  public func foo() {}
  // expected-note@+1 * {{overridden declaration is here}}
  public internal(set) var bar: Int = 0
  // expected-note@+1 * {{overridden declaration is here}}
  public subscript () -> () { return () }
}


public extension Base {
  open func extMemberPublic() {} // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}}
}
package extension Base {
  open func extMemberPackage() {} // expected-warning {{'open' modifier conflicts with extension's default access of 'package'}}
}
internal extension Base {
  open func extMemberInternal() {} // expected-warning {{'open' modifier conflicts with extension's default access of 'internal'}}
}

public class PublicSub: Base {
  private required init() {} // expected-error {{'required' initializer must be accessible wherever class 'PublicSub' can be subclassed}} {{3-10=internal}}
  override func foo() {} // expected-error {{overriding instance method must be as accessible as the declaration it overrides}} {{3-3=public }}
  override var bar: Int { // expected-error {{overriding property must be as accessible as the declaration it overrides}} {{3-3=public }}
    get { return 0 }
    set {}
  }
  override subscript () -> () { return () } // expected-error {{overriding subscript must be as accessible as the declaration it overrides}} {{3-3=public }}
}

public class PublicSubGood: Base {
  required init() {} // okay
}

package class PackageSub: Base {
  private required init() {} // expected-error {{'required' initializer must be accessible wherever class 'PackageSub' can be subclassed}} {{3-10=internal}}
  override func foo() {} // expected-error {{overriding instance method must be as accessible as its enclosing type}} {{3-3=package }}
  override var bar: Int { // expected-error {{overriding property must be as accessible as its enclosing type}} {{3-3=package }}
    get { return 0 }
    set {}
  }
  override subscript () -> () { return () } // expected-error {{overriding subscript must be as accessible as its enclosing type}} {{3-3=package }}
}

package class PackageSubGood: Base {
  package required init() {} // no-warning
  package override func foo() {}
  package override var bar: Int {
    get { return 0 }
    set {}
  }
  package override subscript () -> () { return () }
}

package class PackageSubPrivateSet: Base {
  required init() {}
  private(set) override var bar: Int { // expected-error {{overriding property must be as accessible as its enclosing type}} {{16-16=package }} // expected-error {{overriding _modify accessor must be as accessible as the declaration it overrides}}
    get { return 0 }
    set {} // expected-error {{overriding setter must be as accessible as the declaration it overrides}}
  }
  private(set) override subscript () -> () { // expected-error {{overriding subscript must be as accessible as its enclosing type}}
    get { return () }
    set {}
  }
}

internal class InternalSub: Base {
  required private init() {} // expected-error {{'required' initializer must be accessible wherever class 'InternalSub' can be subclassed}} {{12-19=internal}}
  private override func foo() {} // expected-error {{overriding instance method must be as accessible as its enclosing type}} {{3-10=internal}}
  private override var bar: Int { // expected-error {{overriding property must be as accessible as its enclosing type}} {{3-10=internal}}
    get { return 0 }
    set {}
  }
  private override subscript () -> () { return () } // expected-error {{overriding subscript must be as accessible as its enclosing type}} {{3-10=internal}}
}

internal class InternalSubGood: Base {
  required init() {} // no-warning
  override func foo() {}
  override var bar: Int {
    get { return 0 }
    set {}
  }
  override subscript () -> () { return () }
}

internal class InternalSubPrivateSet: Base {
  required init() {}
  private(set) override var bar: Int { // expected-error {{overriding property must be as accessible as its enclosing type}} {{3-16=}}
    get { return 0 }
    set {}
  }
  private(set) override subscript () -> () { // okay; read-only in base class
    get { return () }
    set {}
  }
}

fileprivate class FilePrivateSub: Base {
  required private init() {} // expected-error {{'required' initializer must be accessible wherever class 'FilePrivateSub' can be subclassed}} {{12-19=fileprivate}}
  private override func foo() {} // expected-error {{overriding instance method must be as accessible as its enclosing type}} {{3-10=fileprivate}}
  private override var bar: Int { // expected-error {{overriding property must be as accessible as its enclosing type}} {{3-10=fileprivate}}
    get { return 0 }
    set {}
  }
  private override subscript () -> () { return () } // expected-error {{overriding subscript must be as accessible as its enclosing type}} {{3-10=fileprivate}}
}

fileprivate class FilePrivateSubGood: Base {
  required init() {} // no-warning
  override func foo() {}
  override var bar: Int {
    get { return 0 }
    set {}
  }
  override subscript () -> () { return () }
}

fileprivate class FilePrivateSubGood2: Base {
  fileprivate required init() {} // no-warning
  fileprivate override func foo() {}
  fileprivate override var bar: Int {
    get { return 0 }
    set {}
  }
  fileprivate override subscript () -> () { return () }
}

fileprivate class FilePrivateSubPrivateSet: Base {
  required init() {}
  private(set) override var bar: Int { // expected-error {{overriding property must be as accessible as its enclosing type}} {{3-10=fileprivate}}
    get { return 0 }
    set {}
  }
  private(set) override subscript () -> () { // okay; read-only in base class
    get { return () }
    set {}
  }
}

private class PrivateSub: Base {
  required private init() {} // expected-error {{'required' initializer must be accessible wherever class 'PrivateSub' can be subclassed}} {{12-19=fileprivate}}
  private override func foo() {} // expected-error {{overriding instance method must be as accessible as its enclosing type}} {{3-10=fileprivate}}
  private override var bar: Int { // expected-error {{overriding property must be as accessible as its enclosing type}} {{3-10=fileprivate}}
    get { return 0 }
    set {}
  }
  private override subscript () -> () { return () } // expected-error {{overriding subscript must be as accessible as its enclosing type}} {{3-10=fileprivate}}
}

private class PrivateSubGood: Base {
  required fileprivate init() {}
  fileprivate override func foo() {}
  fileprivate override var bar: Int {
    get { return 0 }
    set {}
  }
  fileprivate override subscript () -> () { return () }
}

private class PrivateSubPrivateSet: Base {
  required fileprivate init() {}
  fileprivate override func foo() {}
  private(set) override var bar: Int { // expected-error {{setter of overriding property must be as accessible as its enclosing type}}
    get { return 0 }
    set {}
  }
  private(set) override subscript () -> () { // okay; read-only in base class
    get { return () }
    set {}
  }
}

/// Test typealiasing
public typealias PublicTA1 = PublicStruct
public typealias PublicTA11 = PackageStruct // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
public typealias PublicTA2 = InternalStruct // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
public typealias PublicTA3 = FilePrivateStruct // expected-error {{type alias cannot be declared public because its underlying type uses a fileprivate type}}
public typealias PublicTA4 = PrivateStruct // expected-error {{type alias cannot be declared public because its underlying type uses a private type}}

// expected-note@+1 {{type declared here}}
package typealias PackageTA1 = PublicStruct
package typealias PackageTA11 = PackageStruct
package typealias PackageTA2 = InternalStruct // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}
package typealias PackageTA3 = FilePrivateStruct // expected-error {{type alias cannot be declared package because its underlying type uses a fileprivate type}}
package typealias PackageTA4 = PrivateStruct // expected-error {{type alias cannot be declared package because its underlying type uses a private type}}

// expected-note@+1 2 {{type declared here}}
internal typealias InternalTA1 = PublicStruct
internal typealias InternalTA11 = PackageStruct
internal typealias InternalTA2 = InternalStruct
internal typealias InternalTA3 = FilePrivateStruct // expected-error {{type alias cannot be declared internal because its underlying type uses a fileprivate type}}
internal typealias InternalTA4 = PrivateStruct // expected-error {{type alias cannot be declared internal because its underlying type uses a private type}}

public typealias PublicFromInternal = InternalTA1 // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
public typealias PublicFromPackage = PackageTA1 // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
package typealias PackageFromInternal = InternalTA1 // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}

typealias FunctionType1 = (PrivateStruct) -> PublicStruct // expected-error {{type alias must be declared private or fileprivate because its underlying type uses a private type}}
typealias FunctionType2 = (PublicStruct) -> PrivateStruct // expected-error {{type alias must be declared private or fileprivate because its underlying type uses a private type}}
typealias FunctionType3 = (PrivateStruct) -> PrivateStruct // expected-error {{type alias must be declared private or fileprivate because its underlying type uses a private type}}
typealias FunctionType4 = (PackageStruct) -> PrivateStruct // expected-error {{type alias must be declared private or fileprivate because its underlying type uses a private type}}
typealias FunctionType5 = (PublicStruct) -> PackageStruct // no-warning
public typealias FunctionType6 = (PackageStruct) -> PublicStruct // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
public typealias FunctionType7 = (InternalStruct) -> PublicStruct // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}

typealias ArrayType = [PrivateStruct] // expected-error {{type alias must be declared private or fileprivate because its underlying type uses a private type}}
typealias DictType = [String : PrivateStruct] // expected-error {{type alias must be declared private or fileprivate because its underlying type uses a private type}}
typealias GenericArgs = Optional<PrivateStruct> // expected-error {{type alias must be declared private or fileprivate because its underlying type uses a private type}}

package typealias ArrayTypePkg1 = [PrivateStruct] // expected-error {{type alias cannot be declared package because its underlying type uses a private type}}
package typealias DictTypePkg1 = [String : PrivateStruct] // expected-error {{type alias cannot be declared package because its underlying type uses a private type}}
package typealias GenericArgsPkg1 = Optional<PrivateStruct> // expected-error {{type alias cannot be declared package because its underlying type uses a private type}}

package typealias ArrayTypePkg2 = [InternalStruct] // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}
package typealias DictTypePkg2 = [String : InternalStruct] // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}
package typealias GenericArgsPkg2 = Optional<InternalStruct> // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}

public typealias ArrayTypePkg3 = [PackageStruct] // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
public typealias DictTypePkg3 = [String : PackageStruct] // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
public typealias GenericArgsPkg3 = Optional<PackageStruct> // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}

/// Test associated type reference
public protocol HasAssocType {
  associatedtype Inferred
  func test(input: Inferred)
}

public struct AssocTypeImpl: HasAssocType {
  public func test(input: Bool) {}
}
public let _: AssocTypeImpl.Inferred?

protocol HasAssocTypeInternal {
  associatedtype Inferred
  func test(input: Inferred)
}

// expected-note@+1 2 {{type declared here}}
struct AssocTypeImplInternal: HasAssocTypeInternal {
  func test(input: Bool) {}
}
public let inferredInternal1: AssocTypeImplInternal.Inferred? // expected-error {{constant cannot be declared public because its type uses an internal type}}
package let inferredInternal2: AssocTypeImplInternal.Inferred? // expected-error {{constant cannot be declared package because its type uses an internal type}}

package protocol HasAssocTypePkg {
  associatedtype Inferred
  func test(input: Inferred)
}

// expected-note@+1 {{type declared here}}
package struct AssocTypeImplPkg: HasAssocTypePkg {
  package func test(input: Bool) {}
}
public let inferredPackage: AssocTypeImplPkg.Inferred? // expected-error {{constant cannot be declared public because its type uses a package type}}


/// Test variables and constants
public let x: PrivateStruct = PrivateStruct() // expected-error {{constant cannot be declared public because its type uses a private type}}
public var a: PrivateStruct?, b: PrivateStruct? // expected-error 2 {{variable cannot be declared public because its type uses a private type}}
public var (c, d): (PrivateStruct?, PrivateStruct?) // expected-error {{variable cannot be declared public because its type uses a private type}}

public let publicVar: PackageStruct = PackageStruct() // expected-error {{constant cannot be declared public because its type uses a package type}}
public var publicVar1: PackageStruct?, publicVar2: PackageStruct? // expected-error 2 {{variable cannot be declared public because its type uses a package type}}
public var (publicVarA, publicVarB): (PackageStruct?, PackageStruct?) // expected-error {{variable cannot be declared public because its type uses a package type}}

package let packageVar1: InternalStruct = InternalStruct() // expected-error {{constant cannot be declared package because its type uses an internal type}}
package var packageVarA: InternalStruct?, packageVarB: InternalStruct? // expected-error 2 {{variable cannot be declared package because its type uses an internal type}}
package var (packageVarC, packageVarD): (InternalStruct?, InternalStruct?) // expected-error {{variable cannot be declared package because its type uses an internal type}}

package let packageVar2: PrivateStruct = PrivateStruct() // expected-error {{constant cannot be declared package because its type uses a private type}}
package var packageVarE: PrivateStruct?, packageVarF: PrivateStruct? // expected-error 2 {{variable cannot be declared package because its type uses a private type}}
package var (packageVarG, packageVarH): (PrivateStruct?, PrivateStruct?) // expected-error {{variable cannot be declared package because its type uses a private type}}

var packageVar: PrivateStruct? // expected-error {{variable must be declared private or fileprivate because its type uses a private type}}
var internalVar: PrivateStruct? // expected-error {{variable must be declared private or fileprivate because its type uses a private type}}

let internalConstant = PrivateStruct() // expected-error {{constant must be declared private or fileprivate because its type 'PrivateStruct' uses a private type}}
public let publicConstantA = [InternalStruct]() // expected-error {{constant cannot be declared public because its type '[InternalStruct]' uses an internal type}}
public let publicConstantB = [PackageStruct]() // expected-error {{constant cannot be declared public because its type '[PackageStruct]' uses a package type}}
package let packageConstant = [InternalStruct]() // expected-error {{constant cannot be declared package because its type '[InternalStruct]' uses an internal type}}

/// Test properties, subscripts, and methods
public struct Properties {
  public let x: PrivateStruct = PrivateStruct() // expected-error {{property cannot be declared public because its type uses a private type}}
  public let p: PackageStruct = PackageStruct() // expected-error {{property cannot be declared public because its type uses a package type}}
  public var a: PrivateStruct?, b: PrivateStruct? // expected-error 2 {{property cannot be declared public because its type uses a private type}}
  public var (c, d): (PrivateStruct?, PrivateStruct?) // expected-error {{property cannot be declared public because its type uses a private type}}
  public var (e, f): (PackageStruct?, PackageStruct?) // expected-error {{property cannot be declared public because its type uses a package type}}

  package let packageVar: InternalStruct = InternalStruct() // expected-error {{property cannot be declared package because its type uses an internal type}}
  package var packageVarA: InternalStruct?, packageVarB: InternalStruct? // expected-error 2 {{property cannot be declared package because its type uses an internal type}}
  package var (packageVarC, packageVarD): (InternalStruct?, InternalStruct?) // expected-error {{property cannot be declared package because its type uses an internal type}}

  let y = PrivateStruct() // expected-error {{property must be declared fileprivate because its type 'PrivateStruct' uses a private type}}
}

public struct Subscripts {
  subscript (a: PrivateStruct) -> Int { return 0 } // expected-error {{subscript must be declared fileprivate because its index uses a private type}}
  subscript (a: Int) -> PrivateStruct { return PrivateStruct() } // expected-error {{subscript must be declared fileprivate because its element type uses a private type}}

  public subscript (a: PrivateStruct, b: Int) -> Int { return 0 } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  public subscript (a: Int, b: PrivateStruct) -> Int { return 0 } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  public subscript (a: InternalStruct, b: PrivateStruct) -> InternalStruct { return InternalStruct() } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  public subscript (a: PrivateStruct, b: InternalStruct) -> PrivateStruct { return PrivateStruct() } // expected-error {{subscript cannot be declared public because its index uses a private type}}
  public subscript (a: Int, b: Int) -> InternalStruct { return InternalStruct() } // expected-error {{subscript cannot be declared public because its element type uses an internal type}}
  public subscript (a: Int, b: PackageStruct) -> Int { return 0 } // expected-error {{subscript cannot be declared public because its index uses a package type}}
  public subscript (a: String, b: Int) -> PackageStruct { return PackageStruct() } // expected-error {{subscript cannot be declared public because its element type uses a package type}}

  package subscript (x: PrivateStruct, y: String) -> String { return "" } // expected-error {{subscript cannot be declared package because its index uses a private type}}
  package subscript (x: String, y: PrivateStruct) -> String { return "" } // expected-error {{subscript cannot be declared package because its index uses a private type}}
  package subscript (x: InternalStruct, y: FilePrivateStruct) -> InternalStruct { return InternalStruct() } // expected-error {{subscript cannot be declared package because its index uses a fileprivate type}}
  package subscript (x: FilePrivateStruct, y: InternalStruct) -> PrivateStruct { return PrivateStruct() } // expected-error {{subscript cannot be declared package because its index uses a fileprivate type}}
  package subscript (x: String, y: String) -> InternalStruct { return InternalStruct() } // expected-error {{subscript cannot be declared package because its element type uses an internal type}}
}

public struct Methods {
  func foo(a: PrivateStruct) -> Int { return 0 } // expected-error {{method must be declared fileprivate because its parameter uses a private type}}
  func bar(a: Int) -> PrivateStruct { return PrivateStruct() } // expected-error {{method must be declared fileprivate because its result uses a private type}}

  public func a(a: PrivateStruct, b: Int) -> Int { return 0 } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  public func b(a: Int, b: PrivateStruct) -> Int { return 0 } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  public func c(a: InternalStruct, b: PrivateStruct) -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  public func d(a: PrivateStruct, b: InternalStruct) -> PrivateStruct { return PrivateStruct() } // expected-error {{method cannot be declared public because its parameter uses a private type}}
  public func e(a: Int, b: Int) -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared public because its result uses an internal type}}
  public func f(a: Int, b: PackageStruct) -> PackageStruct { return PackageStruct() } // expected-error {{method cannot be declared public because its result uses a package type}}

  package func x(x: PrivateStruct, y: String) -> String { return "" } // expected-error {{method cannot be declared package because its parameter uses a private type}}
  package func y(x: String, y: PrivateStruct) -> String { return "" } // expected-error {{method cannot be declared package because its parameter uses a private type}}
  package func z(x: InternalStruct, y: FilePrivateStruct) -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared package because its parameter uses a fileprivate type}}
  package func w(x: FilePrivateStruct, y: InternalStruct) -> PrivateStruct { return PrivateStruct() } // expected-error {{method cannot be declared package because its parameter uses a fileprivate type}}
  package func v(x: String, y: String) -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared package because its result uses an internal type}}
  package func q() -> InternalStruct { return InternalStruct() } // expected-error {{method cannot be declared package because its result uses an internal type}}
}

public func publicFunc(a: PackageStruct) {} // expected-error {{function cannot be declared public because its parameter uses a package type}}
package func packageFunc1(a: InternalStruct) {} // expected-error {{function cannot be declared package because its parameter uses an internal type}}
package func packageFunc2(a: PrivateStruct) {} // expected-error {{function cannot be declared package because its parameter uses a private type}}
func privateParam(a: PrivateStruct) {} // expected-error {{function must be declared private or fileprivate because its parameter uses a private type}}

/// Test initializers
public struct Initializers1 {
  init(a: PrivateStruct) {} // expected-error {{initializer must be declared fileprivate because its parameter uses a private type}}

  public init(a: PrivateStruct, b: Int) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  public init(a: Int, b: PrivateStruct) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  public init(a: InternalStruct, b: PrivateStruct) {} // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  public init(a: PrivateStruct, b: InternalStruct) { } // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  public init(a: PackageStruct, b: InternalStruct) { } // expected-error {{initializer cannot be declared public because its parameter uses an internal type}}
  public init(a: PackageStruct) { } // expected-error {{initializer cannot be declared public because its parameter uses a package type}}

  package init(x: PrivateStruct, y: Int) {} // expected-error {{initializer cannot be declared package because its parameter uses a private type}}
  package init(x: Int, y: PrivateStruct) {} // expected-error {{initializer cannot be declared package because its parameter uses a private type}}
  package init(x: InternalStruct, y: PrivateStruct) {} // expected-error {{initializer cannot be declared package because its parameter uses a private type}}
  package init(x: PrivateStruct, y: InternalStruct) { } // expected-error {{initializer cannot be declared package because its parameter uses a private type}}
}

// expected-note@+1 * {{superclass is declared here}}
public class PublicClass {}
// expected-note@+2 * {{type declared here}}
// expected-note@+1 * {{superclass is declared here}}
package class PackageClass {}
// expected-note@+2 * {{type declared here}}
// expected-note@+1 * {{superclass is declared here}}
internal class InternalClass {}
// expected-note@+1 * {{type declared here}}
private class PrivateClass {}

/// Test protocols with associated types
public protocol AssocTypes {
  associatedtype Foo

  associatedtype Package: PackageClass // expected-error {{associated type in a public protocol uses a package type in its requirement}}
  associatedtype PackageConformer: PackageProto // expected-error {{associated type in a public protocol uses a package type in its requirement}}

  associatedtype Internal: InternalClass // expected-error {{associated type in a public protocol uses an internal type in its requirement}}
  associatedtype InternalConformer: InternalProto // expected-error {{associated type in a public protocol uses an internal type in its requirement}}

  associatedtype PrivateConformer: PrivateProto // expected-error {{associated type in a public protocol uses a private type in its requirement}}
  associatedtype PI: PrivateProto, InternalProto // expected-error {{associated type in a public protocol uses a private type in its requirement}}
  associatedtype IP: InternalProto, PrivateProto // expected-error {{associated type in a public protocol uses a private type in its requirement}}

  associatedtype PrivateDefault = PrivateStruct // expected-error {{associated type in a public protocol uses a private type in its default definition}}
  associatedtype PublicDefault = PublicStruct
  associatedtype PrivateDefaultConformer: PublicProto = PrivateStruct // expected-error {{associated type in a public protocol uses a private type in its default definition}}
  associatedtype PublicDefaultConformer: PrivateProto = PublicStruct // expected-error {{associated type in a public protocol uses a private type in its requirement}}

  associatedtype PackageDefault = PackageStruct // expected-error {{associated type in a public protocol uses a package type in its default definition}}
  associatedtype PackagePrivateConformer: PackageProto = PrivateStruct // expected-error {{associated type in a public protocol uses a private type in its default definition}}
  associatedtype PrivatePackageConformer: PrivateProto = PackageStruct // expected-error {{associated type in a public protocol uses a private type in its requirement}}

  associatedtype PrivatePrivateDefaultConformer: PrivateProto = PrivateStruct // expected-error {{associated type in a public protocol uses a private type in its requirement}}
  associatedtype PackagePackageDefaultConformer: PackageProto = PackageStruct // expected-error {{associated type in a public protocol uses a package type in its default definition}}

  associatedtype PublicPublicDefaultConformer: PublicProto = PublicStruct
}

public protocol RequirementTypes {
  var x: PrivateStruct { get } // expected-error {{property cannot be declared public because its type uses a private type}}
  var y: PackageStruct { get } // expected-error {{property cannot be declared public because its type uses a package type}}
  subscript(x: Int) -> InternalStruct { get set } // expected-error {{subscript cannot be declared public because its element type uses an internal type}}
  subscript(x: String) -> PackageClass { get set } // expected-error {{subscript cannot be declared public because its element type uses a package type}}
  func foo() -> PrivateStruct // expected-error {{method cannot be declared public because its result uses a private type}}
  func bar() -> PackageProto // expected-error {{method cannot be declared public because its result uses a package type}}
  init(x: PrivateStruct) // expected-error {{initializer cannot be declared public because its parameter uses a private type}}
  init(y: PackageStruct) // expected-error {{initializer cannot be declared public because its parameter uses a package type}}
}

/// Test conformance and assignment scopes
protocol DefaultRefinesPrivate : PrivateProto {} // expected-error {{protocol must be declared private or fileprivate because it refines a private protocol}}
protocol ConformsToPackage : PackageProto {}
public protocol PublicRefinesPrivate : PrivateProto {} // expected-error {{public protocol cannot refine a private protocol}}
public protocol PublicRefinesPackage : PackageProto {} // expected-error {{public protocol cannot refine a package protocol}}
public protocol PublicRefinesInternal : InternalProto {} // expected-error {{public protocol cannot refine an internal protocol}}
public protocol PublicRefinesPI : PrivateProto, InternalProto {} // expected-error {{public protocol cannot refine a private protocol}}
public protocol PublicRefinesIP : InternalProto, PrivateProto {} // expected-error {{public protocol cannot refine a private protocol}}

package protocol PackageRefinesPrivate : PrivateProto {} // expected-error {{package protocol cannot refine a private protocol}}
package protocol PackageRefinesInternal : InternalProto {} // expected-error {{package protocol cannot refine an internal protocol}}
package protocol PackageRefinesPI : PrivateProto, InternalProto {} // expected-error {{package protocol cannot refine a private protocol}}
package protocol PackageRefinesIP : InternalProto, PrivateProto {} // expected-error {{package protocol cannot refine a private protocol}}

private typealias PrivateTypeAlias = PublicProto; // expected-note {{type declared here}}
private typealias PrivateCompoundTypeAlias = PublicProto & AnyObject // expected-note {{type declared here}}

protocol DefaultRefinesPrivateClass: PrivateClass {} // expected-error {{protocol must be declared private or fileprivate because it refines a private class}}
public protocol PublicRefinesPrivateClass: PrivateClass {} // expected-error {{public protocol cannot refine a private class}}
public protocol PublicRefinesPrivateTypeAlias: PrivateTypeAlias {} // expected-error {{public protocol cannot refine a private type alias}}
public protocol PublicRefinesPrivateCompoundTypeAlias: PrivateCompoundTypeAlias {} // expected-error {{public protocol cannot refine a private type alias}}

private typealias PackageAsPrivateTypeAlias = PackageProto; // expected-note {{type declared here}}
private typealias PackageAsPrivateCompoundTypeAlias = PackageProto & AnyObject // expected-note {{type declared here}}

package protocol PackageRefinesInternalClass: InternalClass {} // expected-error {{package protocol cannot refine an internal class}}
package protocol PackageRefinesPrivateClass: PrivateClass {} // expected-error {{package protocol cannot refine a private class}}
package protocol PackageRefinesPrivateTypeAlias: PackageAsPrivateTypeAlias {} // expected-error {{package protocol cannot refine a private type alias}}
package protocol PackageRefinesPrivateCompoundTypeAlias: PackageAsPrivateCompoundTypeAlias {} // expected-error {{package protocol cannot refine a private type alias}}

/// Test enum scopes

// expected-note@+1 * {{type declared here}}
private typealias PrivateInt = Int
// expected-note@+1 * {{type declared here}}
package typealias PackageInt = Int

enum DefaultRawPrivate : PrivateInt { // expected-error {{enum must be declared private or fileprivate because its raw type uses a private type}}
  case A
}

// Note: fileprivate is the most visible valid access level for
// Outer.DefaultRawPrivate, so the diagnostic should say that.
class Outer {
  enum DefaultRawPrivate : PrivateInt { // expected-error {{enum must be declared fileprivate because its raw type uses a private type}}
    case A
  }
}

public class PackageOuter {
  public enum PublicRawPackage : PackageInt { // expected-error {{enum cannot be declared public because its raw type uses a package type}}
    case P
  }
  package enum PackageRawPackage : PackageInt {
    case P
  }
}

package enum PackageRawPublic: Int {
  case P
}

package enum PackageRawPackage: PackageInt {
  case P
}

package enum PackageRawPrivate : PrivateInt { // expected-error {{enum cannot be declared package because its raw type uses a private type}}
  case A
}

public enum PublicRawPackage : PackageInt { // expected-error {{enum cannot be declared public because its raw type uses a package type}}
  case A
}
public enum PublicRawPrivate : PrivateInt { // expected-error {{enum cannot be declared public because its raw type uses a private type}}
  case A
}
public enum MultipleConformance : PrivateProto, PrivateInt { // expected-error {{enum cannot be declared public because its raw type uses a private type}} expected-error {{must appear first}} {{35-35=PrivateInt, }} {{47-59=}}
  case A
  func privateReq() {}
}
public enum MultipleConformancePackage : PackageProto, PackageInt { // expected-error {{enum cannot be declared public because its raw type uses a package type}} expected-error {{must appear first}} {{42-42=PackageInt, }} {{54-66=}}
  case P
  package func packageReq() {}
}
package enum PackageMultipleConformToPackage : PackageInt, PackageProto  {
  case P
  package func packageReq() {}
}
package enum PackageMultipleConformToPrivate : PrivateProto, PrivateInt { // expected-error {{enum cannot be declared package because its raw type uses a private type}} expected-error {{must appear first}} {{48-48=PrivateInt, }} {{60-72=}}
  case A
  func privateReq() {}
}

/// Test class inheritance

open class OpenSubclassInternal : InternalClass {} // expected-error {{class cannot be declared open because its superclass is internal}} expected-error {{superclass 'InternalClass' of open class must be open}}
open class OpenSubclassPackage : PackageClass {} // expected-error {{class cannot be declared open because its superclass is package}} expected-error {{superclass 'PackageClass' of open class must be open}}

public class PublicSubclassPublic : PublicClass {}
public class PublicSubclassPackage : PackageClass {} // expected-error {{class cannot be declared public because its superclass is package}}
public class PublicSubclassInternal : InternalClass {} // expected-error {{class cannot be declared public because its superclass is internal}}
public class PublicSubclassPrivate : PrivateClass {} // expected-error {{class cannot be declared public because its superclass is private}}

package class PackageSubclassPublic : PublicClass {}
package class PackageSubclassPackage : PackageClass {}
package class PackageSubclassInternal : InternalClass {} // expected-error {{class cannot be declared package because its superclass is internal}}
package class PackageSubclassPrivate : PrivateClass {} // expected-error {{class cannot be declared package because its superclass is private}}

class DefaultSubclassPublic : PublicClass {}
class DefaultSubclassPackage : PackageClass {}
class DefaultSubclassInternal : InternalClass {}
class DefaultSubclassPrivate : PrivateClass {} // expected-error {{class must be declared private or fileprivate because its superclass is private}}

/// Test generics

// expected-note@+1 * {{superclass is declared here}}
public class PublicGenericClass<T> {}
// expected-note@+2 * {{type declared here}}
// expected-note@+1 * {{superclass is declared here}}
package class PackageGenericClass<T> {}
// expected-note@+2 * {{type declared here}}
// expected-note@+1 * {{superclass is declared here}}
internal class InternalGenericClass<T> {}
// expected-note@+1 * {{type declared here}}
private class PrivateGenericClass<T> {}

open class OpenConcreteSubclassInternal : InternalGenericClass<Int> {} // expected-error {{class cannot be declared open because its superclass is internal}} expected-error {{superclass 'InternalGenericClass<Int>' of open class must be open}}
open class OpenConcreteSubclassPackage : PackageGenericClass<Int> {} // expected-error {{class cannot be declared open because its superclass is package}} expected-error {{superclass 'PackageGenericClass<Int>' of open class must be open}}
public class PublicConcreteSubclassPublic : PublicGenericClass<Int> {}
public class PublicConcreteSubclassPackage : PackageGenericClass<Int> {} // expected-error {{class cannot be declared public because its superclass is package}}
public class PublicConcreteSubclassInternal : InternalGenericClass<Int> {} // expected-error {{class cannot be declared public because its superclass is internal}}
public class PublicConcreteSubclassPrivate : PrivateGenericClass<Int> {} // expected-error {{class cannot be declared public because its superclass is private}}
public class PublicConcreteSubclassPublicPrivateArg : PublicGenericClass<PrivateStruct> {} // expected-error {{class cannot be declared public because its superclass uses a private type as a generic parameter}}
public class PublicConcreteSubclassPublicPackageArg : PublicGenericClass<PackageStruct> {} // expected-error {{class cannot be declared public because its superclass uses a package type as a generic parameter}}
public class PublicConcreteSubclassPublicInternalArg : PublicGenericClass<InternalStruct> {} // expected-error {{class cannot be declared public because its superclass uses an internal type as a generic parameter}}
open class OpenConcreteSubclassPublicFilePrivateArg : PublicGenericClass<FilePrivateStruct> {} // expected-error {{class cannot be declared open because its superclass uses a fileprivate type as a generic parameter}} expected-error {{superclass 'PublicGenericClass<FilePrivateStruct>' of open class must be open}}
open class OpenConcreteSubclassPackageInternalArg : PackageGenericClass<InternalStruct> {} // expected-error {{class cannot be declared open because its superclass uses an internal type as a generic parameter}} expected-error {{superclass 'PackageGenericClass<InternalStruct>' of open class must be open}}
internal class InternalConcreteSubclassPublicFilePrivateArg : InternalGenericClass<PrivateStruct> {} // expected-error {{class cannot be declared internal because its superclass uses a private type as a generic parameter}}

open class OpenGenericSubclassInternal<T> : InternalGenericClass<T> {} // expected-error {{class cannot be declared open because its superclass is internal}} expected-error {{superclass 'InternalGenericClass<T>' of open class must be open}}
open class OpenGenericSubclassPackage<T> : PackageGenericClass<T> {} // expected-error {{class cannot be declared open because its superclass is package}} expected-error {{superclass 'PackageGenericClass<T>' of open class must be open}}
public class PublicGenericSubclassPublic<T> : PublicGenericClass<T> {}
public class PublicGenericSubclassPackage<T> : PackageGenericClass<T> {} // expected-error {{class cannot be declared public because its superclass is package}}
public class PublicGenericSubclassInternal<T> : InternalGenericClass<T> {} // expected-error {{class cannot be declared public because its superclass is internal}}
public class PublicGenericSubclassPrivate<T> : PrivateGenericClass<T> {} // expected-error {{class cannot be declared public because its superclass is private}}

/// Test enum with associated values
public enum PublicEnumPrivate {
  case A(PrivateStruct) // expected-error {{enum case in a public enum uses a private type}}
}
public enum PublicEnumPackage {
  case A(PackageStruct) // expected-error {{enum case in a public enum uses a package type}}
}
package enum PackageEnumPrivate {
  case A(PrivateStruct) // expected-error {{enum case in a package enum uses a private type}}
}
enum DefaultEnumPrivate {
  case A(PrivateStruct) // expected-error {{enum case in an internal enum uses a private type}}
}
public enum PublicEnumPI {
  case A(InternalStruct) // expected-error {{enum case in a public enum uses an internal type}}
  case B(PrivateStruct, InternalStruct) // expected-error {{enum case in a public enum uses a private type}} expected-error {{enum case in a public enum uses an internal type}}
  case C(InternalStruct, PrivateStruct) // expected-error {{enum case in a public enum uses an internal type}} expected-error {{enum case in a public enum uses a private type}}
  case D(PackageStruct, InternalStruct) // expected-error {{enum case in a public enum uses a package type}} expected-error {{enum case in a public enum uses an internal type}}
}
enum DefaultEnumPublic {
  case A(PublicStruct) // no-warning
  case B(PackageStruct) // no-warning
}

/// Test generic params

struct DefaultGeneric<T> {}

struct DefaultGenericPrivate<T: PrivateProto> {} // expected-error {{generic struct must be declared private or fileprivate because its generic parameter uses a private type}}
struct DefaultGenericPrivate2<T: PrivateClass> {} // expected-error {{generic struct must be declared private or fileprivate because its generic parameter uses a private type}}
struct DefaultGenericPrivateReq<T> where T == PrivateClass {} // expected-warning  {{same-type requirement makes generic parameter 'T' non-generic}}
// expected-error@-1 {{generic struct must be declared private or fileprivate because its generic requirement uses a private type}}
struct DefaultGenericPrivateReq2<T> where T: PrivateProto {} // expected-error {{generic struct must be declared private or fileprivate because its generic requirement uses a private type}}

public struct PublicGenericInternal<T: InternalProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses an internal type}}
public struct PublicGenericPI<T: PrivateProto, U: InternalProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a private type}}
public struct PublicGenericPkg<T: PackageProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a package type}}
public struct PublicGenericPkgWhere<T: PackageProto> where T: PublicProto {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a package type}}
public struct PublicGenericIP<T: InternalProto, U: PrivateProto> {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a private type}}
public struct PublicGenericPIReq<T: PrivateProto> where T: InternalProto {} // expected-error {{generic struct cannot be declared public because its generic parameter uses a private type}}
public struct PublicGenericIPReq<T: InternalProto> where T: PrivateProto {} // expected-error {{generic struct cannot be declared public because its generic requirement uses a private type}}

package struct PackageGenericInternal<T: InternalProto> {} // expected-error {{generic struct cannot be declared package because its generic parameter uses an internal type}}
package struct PackageGenericPI<T: PrivateProto, U: InternalProto> {} // expected-error {{generic struct cannot be declared package because its generic parameter uses a private type}}
package struct PackageGenericIP<T: InternalProto, U: PrivateProto> {} // expected-error {{generic struct cannot be declared package because its generic parameter uses a private type}}
package struct PackageGenericPIReq<T: PrivateProto> where T: InternalProto {} // expected-error {{generic struct cannot be declared package because its generic parameter uses a private type}}
package struct PackageGenericIPReq<T: InternalProto> where T: PrivateProto {} // expected-error {{generic struct cannot be declared package because its generic requirement uses a private type}}


public func genericFunc<T: InternalProto>(_: T) {} // expected-error {{function cannot be declared public because its generic parameter uses an internal type}} {}
public func genericFuncPackage<T: PackageProto>(_: T) {} // expected-error {{function cannot be declared public because its generic parameter uses a package type}} {}
public class GenericClass<T: InternalProto> { // expected-error {{generic class cannot be declared public because its generic parameter uses an internal type}}
  public init<U: PrivateProto>(_: U) {} // expected-error {{initializer cannot be declared public because its generic parameter uses a private type}}
  public func genericMethod<U: PrivateProto>(_: U) {} // expected-error {{instance method cannot be declared public because its generic parameter uses a private type}}
  public func genericMethodPackage<U: PackageProto>(_: U) {} // expected-error {{instance method cannot be declared public because its generic parameter uses a package type}}
}
public enum GenericEnum<T: InternalProto> { // expected-error {{generic enum cannot be declared public because its generic parameter uses an internal type}}
  case A
}
public enum GenericEnumPackage<T: PackageProto> { // expected-error {{generic enum cannot be declared public because its generic parameter uses a package type}}
  case A
}

package func packageGenericFunc<T: InternalProto>(_: T) {} // expected-error {{function cannot be declared package because its generic parameter uses an internal type}} {}
package class PackageGenericClassT<T: InternalProto> { // expected-error {{generic class cannot be declared package because its generic parameter uses an internal type}}
  package init<U: PrivateProto>(_: U) {} // expected-error {{initializer cannot be declared package because its generic parameter uses a private type}}
  package func packageGenericMethod<U: PrivateProto>(_: U) {} // expected-error {{instance method cannot be declared package because its generic parameter uses a private type}}
}
package enum PackageGenericEnumT<T: InternalProto> { // expected-error {{generic enum cannot be declared package because its generic parameter uses an internal type}}
  case A
}

/// Test setters with less accss level
public protocol PublicMutationOperations {
  var size: Int { get set }
  subscript (_: Int) -> Int { get set }
}

package protocol PackageMutationOperations {
  var size: Int { get set }
  subscript (_: Int) -> Int { get set }
}

internal protocol InternalMutationOperations {
  var size: Int { get set }
  subscript (_: Int) -> Int { get set }
}

public struct AccessorsControl : InternalMutationOperations {
  private var size = 0 // expected-error {{property 'size' must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}} {{none}} expected-note {{mark the property as 'internal' to satisfy the requirement}} {{3-10=internal}}
  private subscript (_: Int) -> Int { // expected-error {{subscript must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}} {{none}} expected-note {{mark the subscript as 'internal' to satisfy the requirement}} {{3-10=internal}}
    get { return 42 }
    set {}
  }
}

public struct AccessorsControlPackage : PackageMutationOperations {
  private var size = 0 // expected-error {{property 'size' must be declared package because it matches a requirement in package protocol 'PackageMutationOperations'}} {{none}} expected-note {{mark the property as 'package' to satisfy the requirement}} {{3-10=package}}
  private subscript (_: Int) -> Int { // expected-error {{subscript must be declared package because it matches a requirement in package protocol 'PackageMutationOperations'}} {{none}} expected-note {{mark the subscript as 'package' to satisfy the requirement}} {{3-10=package}}
    get { return 42 }
    set {}
  }
}

package struct PackageAccessorsControl : InternalMutationOperations {
  private var size = 0 // expected-error {{property 'size' must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}} {{none}} expected-note {{mark the property as 'internal' to satisfy the requirement}} {{3-10=internal}}
  private subscript (_: Int) -> Int { // expected-error {{subscript must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}} {{none}} expected-note {{mark the subscript as 'internal' to satisfy the requirement}} {{3-10=internal}}
    get { return 42 }
    set {}
  }
}

public struct PublicWithPrivateSettersConformInternal : InternalMutationOperations {
  // Please don't change the formatting here; it's a precise fix-it test.
  public private(set) var size = 0 // expected-error {{setter for property 'size' must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}} {{none}} expected-note {{mark the property as 'internal' to satisfy the requirement}} {{10-17=internal}}
  public private(set) subscript (_: Int) -> Int { // expected-error {{subscript setter must be declared internal because it matches a requirement in internal protocol 'InternalMutationOperations'}} {{none}} expected-note {{mark the subscript as 'internal' to satisfy the requirement}} {{10-17=internal}}
    get { return 42 }
    set {}
  }
}

public struct PublicWithPrivateSettersConformPackage : PackageMutationOperations {
  public private(set) var size = 0 // expected-error {{setter for property 'size' must be declared package because it matches a requirement in package protocol 'PackageMutationOperations'}} {{none}} expected-note {{mark the property as 'package' to satisfy the requirement}} {{10-17=package}}
  public private(set) subscript (_: Int) -> Int { // expected-error {{subscript setter must be declared package because it matches a requirement in package protocol 'PackageMutationOperations'}} {{none}} expected-note {{mark the subscript as 'package' to satisfy the requirement}} {{10-17=package}}
    get { return 42 }
    set {}
  }
}

public struct PublicWithInternalSettersConformPublic : PublicMutationOperations {
  public internal(set) var size = 0 // expected-error {{setter for property 'size' must be declared public because it matches a requirement in public protocol 'PublicMutationOperations'}} {{none}} expected-note {{mark the property as 'public' to satisfy the requirement}} {{10-24=}}
  public internal(set) subscript (_: Int) -> Int { // expected-error {{subscript setter must be declared public because it matches a requirement in public protocol 'PublicMutationOperations'}} {{none}} expected-note {{mark the subscript as 'public' to satisfy the requirement}} {{10-24=}}
    get { return 42 }
    set {}
  }
}

public struct PublicWithPackageSettersConformPublic : PublicMutationOperations {
  public package(set) var size = 0 // expected-error {{setter for property 'size' must be declared public because it matches a requirement in public protocol 'PublicMutationOperations'}} {{none}} expected-note {{mark the property as 'public' to satisfy the requirement}} {{10-23=}}
  public package(set) subscript (_: Int) -> Int { // expected-error {{subscript setter must be declared public because it matches a requirement in public protocol 'PublicMutationOperations'}} {{none}} expected-note {{mark the subscript as 'public' to satisfy the requirement}} {{10-23=}}
    get { return 42 }
    set {}
  }
}

public struct PublicWithInternalSettersConformInternal : InternalMutationOperations {
  public internal(set) var size = 0 // no-warning
  public internal(set) subscript (_: Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}

package struct PackageWithPrivateSettersConformPublic : PublicMutationOperations {
  package private(set) var size = 0 // expected-error {{setter for property 'size' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicMutationOperations'}} {{none}} expected-note {{mark the property as 'package' to satisfy the requirement}} {{11-24=}}

  package internal(set) subscript (_: Int) -> Int { // expected-error {{subscript setter must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicMutationOperations'}} {{none}} expected-note {{mark the subscript as 'package' to satisfy the requirement}} {{11-25=}}
    get { return 42 }
    set {}
  }
}

internal struct InternalWithPrivateSettersConformPublic : PublicMutationOperations {
  // Please don't change the formatting here; it's a precise fix-it test.
  private(set)var size = 0 // expected-error {{setter for property 'size' must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicMutationOperations'}} {{none}} expected-note {{mark the property as 'internal' to satisfy the requirement}} {{3-15=}}

  internal private(set)subscript (_: Int) -> Int { // expected-error {{subscript setter must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicMutationOperations'}} {{none}} expected-note {{mark the subscript as 'internal' to satisfy the requirement}} {{12-24=}}
    get { return 42 }
    set {}
  }
}

public protocol PublicReadOnlyOperations {
  var size: Int { get }
  subscript (_: Int) -> Int { get }
}

package protocol PackageReadOnlyOperations {
  var size: Int { get }
  subscript (_: Int) -> Int { get }
}

package struct PackageWithPrivateSettersConformPublicGet : PublicReadOnlyOperations {
  public private(set) var size = 0
  internal private(set) subscript (_: Int) -> Int { // expected-error {{subscript must be as accessible as its enclosing type because it matches a requirement in protocol 'PublicReadOnlyOperations'}} {{none}} expected-note {{mark the subscript as 'package' to satisfy the requirement}} {{3-11=package}}
    get { return 42 }
    set {}
  }
}

internal struct InternalWithPrivateSettersConformPackageGet : PackageReadOnlyOperations {
  public private(set) var size = 0
  internal private(set) subscript (_: Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}

internal struct InternalWithPrivateSettersConformPublicGet : PublicReadOnlyOperations {
  public private(set) var size = 0
  internal private(set) subscript (_: Int) -> Int { // no-warning
    get { return 42 }
    set {}
  }
}

public struct PublicWithPrivateSettersConformPublicGet : PublicReadOnlyOperations {
  public private(set) var size = 0 // no-warning
  internal private(set) subscript (_: Int) -> Int { // expected-error {{subscript must be declared public because it matches a requirement in public protocol 'PublicReadOnlyOperations'}} {{none}} expected-note {{mark the subscript as 'public' to satisfy the requirement}} {{3-11=public}}
    get { return 42 }
    set {}
  }
}

package struct PackageWithPrivateSettersConformPackageGet : PackageReadOnlyOperations {
  package private(set) var size = 0 // no-warning
  internal private(set) subscript (_: Int) -> Int { // expected-error {{subscript must be declared package because it matches a requirement in package protocol 'PackageReadOnlyOperations'}} {{none}} expected-note {{mark the subscript as 'package' to satisfy the requirement}} {{3-11=package}}
    get { return 42 }
    set {}
  }
}

/// Test operators
public protocol PublicOperatorProto {
  static prefix func !(_: Self) -> Self
}

package protocol PackageOperatorProto {
  static prefix func !(_: Self) -> Self
}

internal protocol InternalOperatorProto {
  static prefix func !(_: Self) -> Self
}

fileprivate protocol FilePrivateOperatorProto {
  static prefix func !(_: Self) -> Self
}

private protocol PrivateOperatorProto {
  static prefix func !(_: Self) -> Self
}

public struct PublicOperatorAdopter : PublicOperatorProto {
  // expected-error@-1 {{method '!' must be declared public because it matches a requirement in public protocol 'PublicOperatorProto'}}
  fileprivate struct Inner : PublicOperatorProto {
  }
}
private prefix func !(input: PublicOperatorAdopter) -> PublicOperatorAdopter { // expected-note {{mark the operator function as 'public' to satisfy the requirement}} {{1-8=public}}
  return input
}
private prefix func !(input: PublicOperatorAdopter.Inner) -> PublicOperatorAdopter.Inner {
  return input
}

public struct PackageOperatorAdopter : PackageOperatorProto {
  // expected-error@-1 {{method '!' must be declared package because it matches a requirement in package protocol 'PackageOperatorProto'}}
  fileprivate struct Inner : PackageOperatorProto {
  }
}
private prefix func !(input: PackageOperatorAdopter) -> PackageOperatorAdopter { // expected-note {{mark the operator function as 'package' to satisfy the requirement}} {{1-8=package}}
  return input
}
private prefix func !(input: PackageOperatorAdopter.Inner) -> PackageOperatorAdopter.Inner {
  return input
}

public struct InternalOperatorAdopter : InternalOperatorProto {
  // expected-error@-1 {{method '!' must be declared internal because it matches a requirement in internal protocol 'InternalOperatorProto'}}
  fileprivate struct Inner : InternalOperatorProto {
  }
}
private prefix func !(input: InternalOperatorAdopter) -> InternalOperatorAdopter { // expected-note {{mark the operator function as 'internal' to satisfy the requirement}} {{1-8=internal}}
  return input
}
private prefix func !(input: InternalOperatorAdopter.Inner) -> InternalOperatorAdopter.Inner {
  return input
}

public struct FilePrivateOperatorAdopter : FilePrivateOperatorProto {
  fileprivate struct Inner : FilePrivateOperatorProto {
  }
}
private prefix func !(input: FilePrivateOperatorAdopter) -> FilePrivateOperatorAdopter {
  return input
}
private prefix func !(input: FilePrivateOperatorAdopter.Inner) -> FilePrivateOperatorAdopter.Inner {
  return input
}

public struct PrivateOperatorAdopter : PrivateOperatorProto {
  fileprivate struct Inner : PrivateOperatorProto {
  }
}
private prefix func !(input: PrivateOperatorAdopter) -> PrivateOperatorAdopter {
  return input
}
private prefix func !(input: PrivateOperatorAdopter.Inner) -> PrivateOperatorAdopter.Inner {
  return input
}

public protocol Equatablish {
  static func ==(lhs: Self, rhs: Self) /* -> bool */ // expected-note {{protocol requires function '=='}}
}

fileprivate struct EquatablishOuter {
  internal struct Inner : Equatablish {}
}
private func ==(lhs: EquatablishOuter.Inner, rhs: EquatablishOuter.Inner) {}

fileprivate struct EquatablishOuter2 {
  internal struct Inner : Equatablish {
    fileprivate static func ==(lhs: Inner, rhs: Inner) {}
  }
}

fileprivate struct EquatablishOuterPkg {
  package struct Inner : Equatablish {}
}
private func ==(lhs: EquatablishOuterPkg.Inner, rhs: EquatablishOuterPkg.Inner) {}

fileprivate struct EquatablishOuterPkg2 {
  package struct Inner : Equatablish {
    fileprivate static func ==(lhs: Inner, rhs: Inner) {}
  }
}

fileprivate struct EquatablishOuterProblem {
  internal struct Inner : Equatablish { // expected-error {{type 'EquatablishOuterProblem.Inner' does not conform to protocol 'Equatablish'}} expected-note {{add stubs for conformance}}
    private static func ==(lhs: Inner, rhs: Inner) {}
  }
}
internal struct EquatablishOuterProblem2 {
  public struct Inner : Equatablish {
    fileprivate static func ==(lhs: Inner, rhs: Inner) {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
    // expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{5-16=internal}}
  }
}
internal struct EquatablishOuterProblemPkg2 {
  package struct Inner : Equatablish {
    fileprivate static func ==(lhs: Inner, rhs: Inner) {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
    // expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{5-16=internal}}
  }
}
package struct EquatablishOuterProblemPkg22 {
  public struct Inner : Equatablish {
    fileprivate static func ==(lhs: Inner, rhs: Inner) {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
    // expected-note@-1 {{mark the operator function as 'package' to satisfy the requirement}} {{5-16=package}}
  }
}

internal struct EquatablishOuterProblem3 {
  public struct Inner : Equatablish { // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
  }
}

private func ==(lhs: EquatablishOuterProblem3.Inner, rhs: EquatablishOuterProblem3.Inner) {}
// expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{1-8=internal}}

internal struct EquatablishOuterProblemPkg3 {
  package struct Inner : Equatablish { // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
  }
}

private func ==(lhs: EquatablishOuterProblemPkg3.Inner, rhs: EquatablishOuterProblemPkg3.Inner) {}
// expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{1-8=internal}}

package struct EquatablishOuterProblemPkg33 {
  public struct Inner : Equatablish { // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
  }
}

private func ==(lhs: EquatablishOuterProblemPkg33.Inner, rhs: EquatablishOuterProblemPkg33.Inner) {}
// expected-note@-1 {{mark the operator function as 'package' to satisfy the requirement}} {{1-8=package}}


internal struct EquatablishOuterProblem4 {
  public struct Inner : Equatablish {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
}
internal extension EquatablishOuterProblem4.Inner {
  fileprivate static func ==(lhs: EquatablishOuterProblem4.Inner, rhs: EquatablishOuterProblem4.Inner) {}
  // expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{3-15=}}
}

internal struct EquatablishOuterProblemPkg4 {
  package struct Inner : Equatablish {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
}
internal extension EquatablishOuterProblemPkg4.Inner {
  fileprivate static func ==(lhs: EquatablishOuterProblemPkg4.Inner, rhs: EquatablishOuterProblemPkg4.Inner) {}
  // expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{3-15=}}
}

package struct EquatablishOuterProblemPkg44 {
  package struct Inner : Equatablish {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
}
package extension EquatablishOuterProblemPkg44.Inner {
  fileprivate static func ==(lhs: EquatablishOuterProblemPkg44.Inner, rhs: EquatablishOuterProblemPkg44.Inner) {}
  // expected-note@-1 {{mark the operator function as 'package' to satisfy the requirement}} {{3-15=}}
}

internal struct EquatablishOuterProblem5 {
  public struct Inner : Equatablish {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
}
private extension EquatablishOuterProblem5.Inner {
  static func ==(lhs: EquatablishOuterProblem5.Inner, rhs: EquatablishOuterProblem5.Inner) {}
  // expected-note@-1 {{move the operator function to another extension where it can be declared 'internal' to satisfy the requirement}} {{none}}
}

internal struct EquatablishOuterProblemPkg5 {
  package struct Inner : Equatablish {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
}
private extension EquatablishOuterProblemPkg5.Inner {
  static func ==(lhs: EquatablishOuterProblemPkg5.Inner, rhs: EquatablishOuterProblemPkg5.Inner) {}
  // expected-note@-1 {{move the operator function to another extension where it can be declared 'internal' to satisfy the requirement}} {{none}}
}

package struct EquatablishOuterProblemPkg55 {
  public struct Inner : Equatablish {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'Equatablish'}} {{none}}
}
private extension EquatablishOuterProblemPkg55.Inner {
  static func ==(lhs: EquatablishOuterProblemPkg55.Inner, rhs: EquatablishOuterProblemPkg55.Inner) {}
  // expected-note@-1 {{move the operator function to another extension where it can be declared 'package' to satisfy the requirement}} {{none}}
}


package protocol EquatablishPackage {
  static func ==(lhs: Self, rhs: Self) /* -> bool */ // expected-note {{protocol requires function '=='}}
}

fileprivate struct EquatablishPackageOuter {
  internal struct Inner : EquatablishPackage {}
}
private func ==(lhs: EquatablishPackageOuter.Inner, rhs: EquatablishPackageOuter.Inner) {}

fileprivate struct EquatablishPackageOuter2 {
  internal struct Inner : EquatablishPackage {
    fileprivate static func ==(lhs: Inner, rhs: Inner) {}
  }
}

fileprivate struct EquatablishPackageOuterProblem {
  internal struct Inner : EquatablishPackage { // expected-error {{type 'EquatablishPackageOuterProblem.Inner' does not conform to protocol 'EquatablishPackage'}} expected-note {{add stubs for conformance}}
    private static func ==(lhs: Inner, rhs: Inner) {}
  }
}
internal struct EquatablishPackageOuterProblem2 {
  package struct Inner : EquatablishPackage {
    fileprivate static func ==(lhs: Inner, rhs: Inner) {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'EquatablishPackage'}} {{none}}
    // expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{5-16=internal}}
  }
}

internal struct EquatablishPackageOuterProblem3 {
  package struct Inner : EquatablishPackage { // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'EquatablishPackage'}} {{none}}
  }
}

private func ==(lhs: EquatablishPackageOuterProblem3.Inner, rhs: EquatablishPackageOuterProblem3.Inner) {}
// expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{1-8=internal}}

internal struct EquatablishPackageOuterProblem4 {
  package struct Inner : EquatablishPackage {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'EquatablishPackage'}} {{none}}
}
internal extension EquatablishPackageOuterProblem4.Inner {
  fileprivate static func ==(lhs: EquatablishPackageOuterProblem4.Inner, rhs: EquatablishPackageOuterProblem4.Inner) {}
  // expected-note@-1 {{mark the operator function as 'internal' to satisfy the requirement}} {{3-15=}}
}

internal struct EquatablishPackageOuterProblem5 {
  package struct Inner : EquatablishPackage {} // expected-error {{method '==' must be as accessible as its enclosing type because it matches a requirement in protocol 'EquatablishPackage'}} {{none}}
}
private extension EquatablishPackageOuterProblem5.Inner {
  static func ==(lhs: EquatablishPackageOuterProblem5.Inner, rhs: EquatablishPackageOuterProblem5.Inner) {}
  // expected-note@-1 {{move the operator function to another extension where it can be declared 'internal' to satisfy the requirement}} {{none}}
}

/// Test associatedtype scope
public protocol AssocTypeProto {
  associatedtype Assoc
}

fileprivate struct AssocTypeOuter {
  internal struct Inner : AssocTypeProto {
    fileprivate typealias Assoc = Int
  }
}

fileprivate struct AssocTypeOuterProblem {
  internal struct Inner : AssocTypeProto {
    private typealias Assoc = Int // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}} expected-note {{mark the type alias as 'fileprivate' to satisfy the requirement}} {{5-12=fileprivate}}
  }
}

internal struct AssocTypeOuterProblem2 {
  public struct Inner : AssocTypeProto {
    fileprivate typealias Assoc = Int // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}} expected-note {{mark the type alias as 'internal' to satisfy the requirement}} {{5-16=internal}}
  }
}

internal struct AssocTypeOuterProblemPkg2 {
  package struct Inner : AssocTypeProto {
    fileprivate typealias Assoc = Int // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}} expected-note {{mark the type alias as 'internal' to satisfy the requirement}} {{5-16=internal}}
  }
}

package struct AssocTypeOuterProblemPkg22 {
  public struct Inner : AssocTypeProto {
    fileprivate typealias Assoc = Int // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}} expected-note {{mark the type alias as 'package' to satisfy the requirement}} {{5-16=package}}
  }
}

internal struct AssocTypeOuterProblem3 {
  public struct Inner : AssocTypeProto {} // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}}
}
internal extension AssocTypeOuterProblem3.Inner {
  fileprivate typealias Assoc = Int // expected-note {{mark the type alias as 'internal' to satisfy the requirement}} {{3-15=}}
}

internal struct AssocTypeOuterProblemPkg3 {
  package struct Inner : AssocTypeProto {} // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}}
}
internal extension AssocTypeOuterProblemPkg3.Inner {
  fileprivate typealias Assoc = Int // expected-note {{mark the type alias as 'internal' to satisfy the requirement}} {{3-15=}}
}

package struct AssocTypeOuterProblemPkg33 {
  public struct Inner : AssocTypeProto {} // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}}
}
package extension AssocTypeOuterProblemPkg33.Inner {
  fileprivate typealias Assoc = Int // expected-note {{mark the type alias as 'package' to satisfy the requirement}} {{3-15=}}
}

internal struct AssocTypeOuterProblem4 {
  public struct Inner : AssocTypeProto {} // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}}
}
private extension AssocTypeOuterProblem4.Inner {
  typealias Assoc = Int // expected-note {{move the type alias to another extension where it can be declared 'internal' to satisfy the requirement}} {{none}}
}

internal struct AssocTypeOuterProblemPkg4 {
  package struct Inner : AssocTypeProto {} // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}}
}
private extension AssocTypeOuterProblemPkg4.Inner {
  typealias Assoc = Int // expected-note {{move the type alias to another extension where it can be declared 'internal' to satisfy the requirement}} {{none}}
}
package struct AssocTypeOuterProblemPkg44 {
  public struct Inner : AssocTypeProto {} // expected-error {{type alias 'Assoc' must be as accessible as its enclosing type because it matches a requirement in protocol 'AssocTypeProto'}} {{none}}
}
private extension AssocTypeOuterProblemPkg44.Inner {
  typealias Assoc = Int // expected-note {{move the type alias to another extension where it can be declared 'package' to satisfy the requirement}} {{none}}
}

/// Test inheritance of composition types
internal typealias InternalComposition = PublicClass & PublicProto // expected-note {{declared here}}
public class DerivedFromInternalComposition : InternalComposition { // expected-error {{class cannot be declared public because its superclass is internal}}
  public func publicReq() {}
}

internal typealias InternalCompositionPkg = PackageClass & PackageProto // expected-note {{declared here}}
package class DerivedFromInternalCompositionPkg : InternalCompositionPkg { // expected-error {{class cannot be declared package because its superclass is internal}}
  public func packageReq() {}
}

package typealias PackageComposition = PublicClass & PublicProto // expected-note {{declared here}}
public class DerivedFromPackageComposition : PackageComposition { // expected-error {{class cannot be declared public because its superclass is package}}
  public func publicReq() {}
}

internal typealias InternalGenericComposition<T> = PublicGenericClass<T> & PublicProto // expected-note {{declared here}}
public class DerivedFromInternalGenericComposition : InternalGenericComposition<Int> { // expected-error {{class cannot be declared public because its superclass is internal}}
  public func publicReq() {}
}

internal typealias InternalGenericCompositionPkg<T> = PackageGenericClass<T> & PackageProto // expected-note {{declared here}}
package class DerivedFromInternalGenericCompositionPkg : InternalGenericCompositionPkg<Int> { // expected-error {{class cannot be declared package because its superclass is internal}}
  public func packageReq() {}
}

package typealias PackageGenericComposition<T> = PublicGenericClass<T> & PublicProto // expected-note {{declared here}}
public class DerivedFromPackageGenericComposition : PackageGenericComposition<Int> { // expected-error {{class cannot be declared public because its superclass is package}}
  public func publicReq() {}
}

internal typealias InternalConcreteGenericComposition = PublicGenericClass<Int> & PublicProto // expected-note {{declared here}}
public class DerivedFromInternalConcreteGenericComposition : InternalConcreteGenericComposition { // expected-error {{class cannot be declared public because its superclass is internal}}
  public func publicReq() {}
}

fileprivate typealias FilePrivateConcreteGenericCompositionPkg = InternalGenericClass<Int> & InternalProto
public class DerivedFromFilePrivateConcreteGenericCompositionPkg : FilePrivateConcreteGenericCompositionPkg { // expected-error {{class cannot be declared public because its superclass uses an internal type as a generic parameter}}
  public func internalReq() {}
}

internal typealias InternalConcreteGenericCompositionPkg = PackageGenericClass<Int> & PackageProto
public class DerivedFromInternalConcreteGenericCompositionPkg : InternalConcreteGenericCompositionPkg { // expected-error {{class cannot be declared public because its superclass uses a package type as a generic parameter}}
  public func packageReq() {}
}

package typealias PackageConcreteGenericComposition = PublicGenericClass<Int> & PublicProto // expected-note {{declared here}}
public class DerivedFromPacakgeConcreteGenericComposition : PackageConcreteGenericComposition { // expected-error {{class cannot be declared public because its superclass is package}}
  public func publicReq() {}
}

package typealias PackageConcreteGenericCompositionStr = PackageGenericClass<String> & PackageProto // expected-note {{declared here}}
public class DerivedFromPacakgeConcreteGenericComposition2 : PackageConcreteGenericCompositionStr { // expected-error {{class cannot be declared public because its superclass is package}}
  public func packageReq() {}
}

package class DerivedFromPacakgeConcreteGenericComposition3 : PackageConcreteGenericCompositionStr { // no-warning
  public func packageReq() {}
}
internal class DerivedFromPacakgeConcreteGenericComposition4 : PackageConcreteGenericCompositionStr { // no-warning
  public func packageReq() {}
}

/// Test typealiasing composition types
public typealias BadPublicComposition1 = InternalClass & PublicProto // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
public typealias BadPublicComposition2 = PublicClass & InternalProto // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
public typealias BadPublicComposition3<T> = InternalGenericClass<T> & PublicProto // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
public typealias BadPublicComposition4 = InternalGenericClass<Int> & PublicProto // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}
public typealias BadPublicComposition5 = PublicGenericClass<InternalStruct> & PublicProto // expected-error {{type alias cannot be declared public because its underlying type uses an internal type}}

package typealias BadPublicCompositionPkg1 = InternalClass & PackageProto // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}
package typealias BadPublicCompositionPkg2 = PackageClass & InternalProto // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}
package typealias BadPublicCompositionPkg3<T> = InternalGenericClass<T> & PackageProto // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}
package typealias BadPublicCompositionPkg4 = InternalGenericClass<Int> & PackageProto // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}
package typealias BadPublicCompositionPkg5 = PackageGenericClass<InternalStruct> & PackageProto // expected-error {{type alias cannot be declared package because its underlying type uses an internal type}}

public typealias BadPublicCompositionPkg6 = PackageClass & PublicProto // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
public typealias BadPublicCompositionPkg7 = PublicClass & PackageProto // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
public typealias BadPublicCompositionPkg8<T> = PackageGenericClass<T> & PublicProto // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
public typealias BadPublicCompositionPkg9 = PackageGenericClass<Int> & PublicProto // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}
public typealias BadPublicCompositionPkg10 = PublicGenericClass<PackageStruct> & PublicProto // expected-error {{type alias cannot be declared public because its underlying type uses a package type}}

/// Test redundant modifiers
open class ClassWithProperties {
  open open(set) var openProp = 0 // expected-warning {{'open(set)' modifier is redundant for an open property}} {{8-18=}}
  public public(set) var publicProp = 0 // expected-warning {{'public(set)' modifier is redundant for a public property}} {{10-22=}}
  package package(set) var packageProp = 0 // expected-warning {{'package(set)' modifier is redundant for a package property}} {{11-24=}}
  internal internal(set) var internalProp = 0 // expected-warning {{'internal(set)' modifier is redundant for an internal property}} {{12-26=}}
  fileprivate fileprivate(set) var fileprivateProp = 0 // expected-warning {{'fileprivate(set)' modifier is redundant for a fileprivate property}} {{15-32=}}
  private private(set) var privateProp = 0 // expected-warning {{'private(set)' modifier is redundant for a private property}} {{11-24=}}
  internal(set) var defaultProp = 0 // expected-warning {{'internal(set)' modifier is redundant for an internal property}} {{3-17=}}
}

extension ClassWithProperties {
  // expected-warning@+1 {{'internal(set)' modifier is redundant for an internal property}} {{12-26=}}
  internal internal(set) var defaultExtProp: Int {
    get { return 42 }
    set {}
  }
  // expected-warning@+1 {{'internal(set)' modifier is redundant for an internal property}} {{3-17=}}
  internal(set) var defaultExtProp2: Int {
    get { return 42 }
    set {}
  }
}

public extension ClassWithProperties {
  // expected-warning@+2 {{'public' modifier is redundant for property declared in a public extension}} {{3-10=}}
  // expected-warning@+1 {{'public(set)' modifier is redundant for a public property}} {{10-22=}}
  public public(set) var publicExtProp: Int {
    get { return 42 }
    set {}
  }
  // expected-warning@+1 {{'public(set)' modifier is redundant for a public property}} {{3-15=}}
  public(set) var publicExtProp2: Int {
    get { return 42 }
    set {}
  }
}

package extension ClassWithProperties {
  // expected-warning@+2 {{'package' modifier is redundant for property declared in a package extension}} {{3-11=}}
  // expected-warning@+1 {{'package(set)' modifier is redundant for a package property}} {{11-24=}}
  package package(set) var packageExtProp: Int {
    get { return 42 }
    set {}
  }
  // expected-warning@+1 {{'package(set)' modifier is redundant for a package property}} {{3-16=}}
  package(set) var packageExtProp2: Int {
    get { return 42 }
    set {}
  }
}

internal extension ClassWithProperties {
  // expected-warning@+2 {{'internal' modifier is redundant for property declared in an internal extension}} {{3-12=}}
  // expected-warning@+1 {{'internal(set)' modifier is redundant for an internal property}} {{12-26=}}
  internal internal(set) var internalExtProp: Int {
    get { return 42 }
    set {}
  }
  // expected-warning@+1 {{'internal(set)' modifier is redundant for an internal property}} {{3-17=}}
  internal(set) var internalExtProp2: Int {
    get { return 42 }
    set {}
  }
}

fileprivate extension ClassWithProperties {
  // expected-warning@+2 {{'fileprivate' modifier is redundant for property declared in a fileprivate extension}} {{3-15=}}
  // expected-warning@+1 {{'fileprivate(set)' modifier is redundant for a fileprivate property}} {{15-32=}}
  fileprivate fileprivate(set) var fileprivateExtProp: Int {
    get { return 42 }
    set {}
  }
  // expected-warning@+1 {{'fileprivate(set)' modifier is redundant for a fileprivate property}} {{3-20=}}
  fileprivate(set) var fileprivateExtProp2: Int {
    get { return 42 }
    set {}
  }
  private(set) var fileprivateExtProp3: Int {
    get { return 42 }
    set {}
  }
}

private extension ClassWithProperties {
  // expected-warning@+2 {{'fileprivate' modifier is redundant for property declared in a private (equivalent to fileprivate) extension}} {{3-15=}}
  // expected-warning@+1 {{'fileprivate(set)' modifier is redundant for a fileprivate property}} {{15-32=}}
  fileprivate fileprivate(set) var privateExtProp: Int {
    get { return 42 }
    set {}
  }
  // expected-warning@+1 {{'fileprivate(set)' modifier is redundant for a fileprivate property}} {{3-20=}}
  fileprivate(set) var privateExtProp2: Int {
    get { return 42 }
    set {}
  }
  private(set) var privateExtProp3: Int {
    get { return 42 }
    set {}
  }
}

public var inferredType = PrivateStruct() // expected-error {{variable cannot be declared public because its type 'PrivateStruct' uses a private type}}
public var inferredGenericParameters: Optional = PrivateStruct() // expected-error {{variable cannot be declared public because its type uses a private type}}
public var explicitType: Optional<PrivateStruct> = PrivateStruct() // expected-error {{variable cannot be declared public because its type uses a private type}}

package var pkgInferredType = PrivateStruct() // expected-error {{variable cannot be declared package because its type 'PrivateStruct' uses a private type}}
package var pkgInferredGenericParameters: Optional = PrivateStruct() // expected-error {{variable cannot be declared package because its type uses a private type}}
package var pkgExplicitType: Optional<PrivateStruct> = PrivateStruct() // expected-error {{variable cannot be declared package because its type uses a private type}}

public var inferredTypePkg = PackageStruct() // expected-error {{variable cannot be declared public because its type 'PackageStruct' uses a package type}}
public var inferredGenericParametersPkg: Optional = PackageStruct() // expected-error {{variable cannot be declared public because its type uses a package type}}
public var explicitTypePkg: Optional<PackageStruct> = PackageStruct() // expected-error {{variable cannot be declared public because its type uses a package type}}

/// Test extension of @objc class
// rdar://problem/47557376
@objc open class ObjCBase {
  init() {}
  @objc open dynamic func foo() {}
  @objc open dynamic var bar: Int = 0
  @objc open dynamic func abc() {}
  @objc open dynamic var def: Int = 0
}
open class ObjCSub: ObjCBase {}

public extension ObjCSub {
  // Don't try to remove the 'open', since it's needed to be a valid 'override'.
  open override func foo() {} // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}} {{none}}
  open override var bar: Int { // expected-warning {{'open' modifier conflicts with extension's default access of 'public'}} {{none}}
    get { return 0 }
    set {}
  }
}

package extension ObjCSub {
  // Don't try to remove the 'open', since it's needed to be a valid 'override'.
  open override func abc() {} // expected-warning {{'open' modifier conflicts with extension's default access of 'package'}} {{none}}
  open override var def: Int { // expected-warning {{'open' modifier conflicts with extension's default access of 'package'}} {{none}}
    get { return 1 }
    set {}
  }
}

@objc public class ObjCBasePub {
  init() {}
  @objc open dynamic func foo() {}
  @objc open dynamic var bar: Int = 0
  @objc open dynamic func abc() {}
  @objc open dynamic var def: Int = 0
}
public class ObjCSubPub: ObjCBasePub {}
package class ObjCSubPkg: ObjCBasePub {}

public extension ObjCSubPub {
  override func foo() {} // no-warning
  override var bar: Int { // no-warning
    get { return 0 }
    set {}
  }
}

package extension ObjCSubPub {
  public override func abc() {} // expected-warning {{'public' modifier conflicts with extension's default access of 'package'}} {{none}}
  public override var def: Int { // expected-warning {{'public' modifier conflicts with extension's default access of 'package'}} {{none}}
    get { return 1 }
    set {}
  }
}

package extension ObjCSubPkg {
  override func foo() {} // no-warning
  override var bar: Int { // no-warning
    get { return 2 }
    set {}
  }
}

internal extension ObjCSubPkg {
  package override func abc() {} // expected-warning {{'package' modifier conflicts with extension's default access of 'internal'}} {{none}}
  package override var def: Int { // expected-warning {{'package' modifier conflicts with extension's default access of 'internal'}} {{none}}
    get { return 3 }
    set {}
  }
}

/// Test generic params with constraints
public struct TestGenericSubscripts {
  // We'd like these to be errors in a future version of Swift, but they weren't
  // in Swift 5.0.
  public subscript<T: PrivateProto>(_: T) -> Int { return 0 } // expected-warning {{subscript should not be declared public because its generic parameter uses a private type}} {{none}}
  public subscript<T>(where _: T) -> Int where T: PrivateProto { return 0 } // expected-warning {{subscript should not be declared public because its generic requirement uses a private type}} {{none}}
  public subscript<T: PackageProto>(_: T) -> String { return "" } // expected-warning {{subscript should not be declared public because its generic parameter uses a package type}} {{none}}
  public subscript<T>(where _: T) -> String where T: PackageProto { return "" } // expected-warning {{subscript should not be declared public because its generic requirement uses a package type}} {{none}}
}

package struct TestGenericSubscriptsPkg {
  package subscript<T: PrivateProto>(_: T) -> Int { return 0 } // expected-warning {{subscript should not be declared package because its generic parameter uses a private type}} {{none}}
  package subscript<T>(where _: T) -> Int where T: PrivateProto { return 0 } // expected-warning {{subscript should not be declared package because its generic requirement uses a private type}} {{none}}
}

public typealias TestGenericAlias<T: PrivateProto> = T // expected-warning {{type alias should not be declared public because its generic parameter uses a private type}}
public typealias TestGenericAliasWhereClause<T> = T where T: PrivateProto // expected-warning {{type alias should not be declared public because its generic requirement uses a private type}}

public typealias TestGenericAliasPkg<T: PackageProto> = T // expected-warning {{type alias should not be declared public because its generic parameter uses a package type}}
public typealias TestGenericAliasWhereClausePkg<T> = T where T: PackageProto // expected-warning {{type alias should not be declared public because its generic requirement uses a package type}}

package typealias PackageTestGenericAlias<T: PrivateProto> = T // expected-warning {{type alias should not be declared package because its generic parameter uses a private type}}
package typealias PackageTestGenericAliasWhereClause<T> = T where T: PrivateProto // expected-warning {{type alias should not be declared package because its generic requirement uses a private type}}
