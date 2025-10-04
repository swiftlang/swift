// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/indirects.swiftmodule \
// RUN:   %S/../Sema/Inputs/implementation-only-imports/indirects.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-module -o %t/directs.swiftmodule -I %t\
// RUN:    %S/../Sema/Inputs/implementation-only-imports/directs.swift \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

// RUN: %target-swift-frontend -emit-sil %s -I %t \
// RUN:   -swift-version 5 -target arm64-apple-none-macho \
// RUN:   -enable-experimental-feature Embedded

// REQUIRES: swift_feature_Embedded
// REQUIRES: embedded_stdlib_cross_compiling

@_implementationOnly import directs
import indirects

internal func localInternalFunc() {}

public func implicitlyInlinablePublic() {
  localInternalFunc()
}

private func implicitlyInlinablePrivate() {
  localInternalFunc()
}

@_neverEmitIntoClient
public func explicitNonInliable() {
  _ = StructFromDirect()

  if (true) {
    _ = StructFromDirect()
  }

  @_neverEmitIntoClient
  func nested() {
    _ = StructFromDirect()
  }
  nested()

  localInternalFunc()
}

@_alwaysEmitIntoClient
public func legalAccessToIndirect() {
  _ = StructFromIndirect()

  if (true) {
    _ = StructFromIndirect()
  }

  func nested() {
    _ = StructFromIndirect()
  }
  nested()
}

extension Array {
  @_alwaysEmitIntoClient
  public var myMutableSpan: Int {
    get {
      return 0
    }
  }
}
