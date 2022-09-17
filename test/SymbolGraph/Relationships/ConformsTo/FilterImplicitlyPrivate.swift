// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/ExternalUnderscored.swift -module-name ExternalUnderscored -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name FilterImplicitlyPrivate -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name FilterImplicitlyPrivate -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/FilterImplicitlyPrivate.symbols.json
// RUN: %{python} -c 'import os.path; import sys; sys.exit(1 if os.path.exists(sys.argv[1]) else 0)' %t/FilterImplicitlyPrivate@ExternalUnderscored.symbols.json

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/ExternalUnderscored.swift -module-name ExternalUnderscored -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name FilterImplicitlyPrivate -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name FilterImplicitlyPrivate -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/FilterImplicitlyPrivate.symbols.json
// RUN: %{python} -c 'import os.path; import sys; sys.exit(1 if os.path.exists(sys.argv[1]) else 0)' %t/FilterImplicitlyPrivate@ExternalUnderscored.symbols.json

// Make sure extensions on implicitly private (< public or underscored, or inside one of those)
// don't emit relationships (or symbols)

struct Internal {}

extension Internal: CustomDebugStringConvertible {
  var debugDescription: String {
    return ""
  }
}

public struct _PublicUnderscored {}

extension _PublicUnderscored: CustomDebugStringConvertible {
  public var debugDescription: String {
    return ""
  }
}

extension _PublicUnderscored {
  public struct PublicInner {}
  public struct _PublicUnderscoredInner {}
  struct InternalInner {}
}

extension _PublicUnderscored.PublicInner: CustomDebugStringConvertible {
  public var debugDescription: String {
    return ""
  }
}

extension _PublicUnderscored._PublicUnderscoredInner: CustomDebugStringConvertible {
  public var debugDescription: String {
    return ""
  }
}

extension _PublicUnderscored.InternalInner: CustomDebugStringConvertible {
  public var debugDescription: String {
    return ""
  }
}

import ExternalUnderscored

extension _ExternalUnderscored: CustomDebugStringConvertible {
  public var debugDescription: String {
    return ""
  }
}

// CHECK: "symbols": []
// CHECK: "relationships": []
