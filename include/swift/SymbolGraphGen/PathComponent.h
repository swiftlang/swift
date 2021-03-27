//===--- SymbolGraphPathComponent.h - Swift SymbolGraph Path Component ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYMBOLGRAPHGEN_PATHCOMPONENT_H
#define SWIFT_SYMBOLGRAPHGEN_PATHCOMPONENT_H

#include "llvm/ADT/SmallString.h"

namespace swift {
class ValueDecl;

namespace symbolgraphgen {

/// Summary information for a node along a path through a symbol graph.
struct PathComponent {
  /// The title of the corresponding symbol graph node.
  SmallString<32> Title;
  /// The kind of the corresponding symbol graph node.
  StringRef Kind;
  /// The swift decl associated with the corresponding symbol graph node.
  const ValueDecl *VD;
};

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_PATHCOMPONENT_H
