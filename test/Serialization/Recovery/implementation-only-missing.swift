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

#if PRIVATE_LIB

public struct HiddenGenStruct<A: HiddenProtocol> {
  public init() {}
}

public protocol HiddenProtocol {
  associatedtype Value
}

#elseif PUBLIC_LIB

@_implementationOnly import private_lib

struct LibProtocolContraint { }

protocol LibProtocolTABound { }
struct LibProtocolTA: LibProtocolTABound { }

protocol LibProtocol {
  associatedtype TA: LibProtocolTABound = LibProtocolTA

  func hiddenRequirement<A>(
      param: HiddenGenStruct<A>
  ) where A.Value == LibProtocolContraint
}

extension LibProtocol where TA == LibProtocolTA {
  func hiddenRequirement<A>(
      param: HiddenGenStruct<A>
  ) where A.Value == LibProtocolContraint { }
}

public struct PublicStruct: LibProtocol {
  typealias TA = LibProtocolTA
  public init() { }
}

#elseif CLIENT_APP

import public_lib

#endif
