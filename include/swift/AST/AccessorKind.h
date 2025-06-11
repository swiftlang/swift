//===-- AST/AccessorKind.h --------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ACCESSOR_KIND_H
#define SWIFT_AST_ACCESSOR_KIND_H

/// This header is included in a bridging header. Be *very* careful with what
/// you include here! See include caveats in `ASTBridging.h`.
#include "swift/Basic/SwiftBridging.h"

namespace swift {

// Note that the values of these enums line up with %select values in
// diagnostics.
enum class ENUM_EXTENSIBILITY_ATTR(closed) AccessorKind {
#define ACCESSOR(ID, KEYWORD) ID SWIFT_NAME(#KEYWORD),
#define LAST_ACCESSOR(ID) Last = ID
#include "swift/AST/AccessorKinds.def"
#undef ACCESSOR
#undef LAST_ACCESSOR
};

} // namespace swift

#endif // SWIFT_AST_ACCESSOR_KIND_H
