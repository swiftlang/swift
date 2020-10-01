// Recover from missing types hidden behind an importation-only when indexing
// a system module.
// rdar://problem/52837313

// RUN: %empty-directory(%t)

//// Build the private module, the public module and the client app normally.
//// Force the public module to be system with an underlying Clang module.
// RUN: %target-swift-frontend -emit-module -DPRIVATE_LIB %s -module-name private_lib -emit-module-path %t/private_lib.swiftmodule
// RUN: %target-swift-frontend -emit-module -DPUBLIC_LIB %s -module-name public_lib -emit-module-path %t/public_lib.swiftmodule -I %t -I %S/Inputs/implementation-only-missing -import-underlying-module

//// The client app should build OK without the private module. Removing the
//// private module is superfluous but makes sure that it's not somehow loaded.
// RUN: rm %t/private_lib.swiftmodule
// RUN: %target-swift-frontend -typecheck -DCLIENT_APP -primary-file %s -I %t -index-system-modules -index-store-path %t
// RUN: %target-swift-frontend -emit-sil -DCLIENT_APP -primary-file %s -I %t -module-name client

//// Printing the public module should not crash when checking for overrides of
//// methods from the private module.
// RUN: %target-swift-ide-test -print-module -module-to-print=public_lib -source-filename=x -skip-overrides -I %t

#if PRIVATE_LIB

@propertyWrapper
public struct IoiPropertyWrapper<V> {
  var content: V

  public init(_ v: V) {
    content = v
  }

  public var wrappedValue: V {
    return content
  }
}

public struct HiddenGenStruct<A: HiddenProtocol> {
  public init() {}
}

public protocol HiddenProtocol {
  associatedtype Value
}

public protocol HiddenProtocolWithOverride {
  func hiddenOverride()
}

#elseif PUBLIC_LIB

@_implementationOnly import private_lib

struct LibProtocolConstraint { }

protocol LibProtocolTABound { }
struct LibProtocolTA: LibProtocolTABound { }

protocol LibProtocol {
  associatedtype TA: LibProtocolTABound = LibProtocolTA

  func hiddenRequirement<A>(
      param: HiddenGenStruct<A>
  ) where A.Value == LibProtocolConstraint
}

extension LibProtocol where TA == LibProtocolTA {
  func hiddenRequirement<A>(
      param: HiddenGenStruct<A>
  ) where A.Value == LibProtocolConstraint { }
}

public struct PublicStruct: LibProtocol {
  typealias TA = LibProtocolTA

  public init() { }

  public var nonWrappedVar: String = "some text"
}

struct StructWithOverride: HiddenProtocolWithOverride {
  func hiddenOverride() {}
}

internal protocol RefinesHiddenProtocol: HiddenProtocol {

}

public struct PublicStructConformsToHiddenProtocol: RefinesHiddenProtocol {
  public typealias Value = Int

  public init() { }
}

#elseif CLIENT_APP

import public_lib

var s = PublicStruct()
print(s.nonWrappedVar)

var p = PublicStructConformsToHiddenProtocol()
print(p)

#endif
