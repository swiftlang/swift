//===--- FragmentInfo.h - Swift SymbolGraph Declaration Fragment Info -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYMBOLGRAPHGEN_FRAGMENTINFO_H
#define SWIFT_SYMBOLGRAPHGEN_FRAGMENTINFO_H

#include "llvm/ADT/SmallVector.h"
#include "PathComponent.h"

namespace swift {
class ValueDecl;

namespace symbolgraphgen {

/// Summary information for a symbol referenced in a symbol graph declaration fragment.
struct FragmentInfo {
  /// The swift decl of the referenced symbol.
  const ValueDecl *VD;
  /// The path components of the refereced symbol.
  SmallVector<PathComponent, 4> ParentContexts;
};

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_FRAGMENTINFO_H
