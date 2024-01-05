//===--- AttrKind.h - Enumerate attribute kinds  ----------------*- C++ -*-===//
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
//
// This file defines enumerations related to declaration attributes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ATTRKIND_H
#define SWIFT_ATTRKIND_H

#include "swift/Basic/InlineBitfield.h"
#include "swift/Basic/LLVM.h"
#include "swift/Config.h"
#include "llvm/Support/DataTypes.h"

namespace swift {

/// The associativity of a binary operator.
enum class Associativity : uint8_t {
  /// Non-associative operators cannot be written next to other
  /// operators with the same precedence.  Relational operators are
  /// typically non-associative.
  None,

  /// Left-associative operators associate to the left if written next
  /// to other left-associative operators of the same precedence.
  Left,

  /// Right-associative operators associate to the right if written
  /// next to other right-associative operators of the same precedence.
  Right
};

/// Returns the in-source spelling of the given associativity.
StringRef getAssociativitySpelling(Associativity value);

/// The kind of unary operator, if any.
enum class UnaryOperatorKind : uint8_t {
  None,
  Prefix,
  Postfix
};

/// Access control levels.
// These are used in diagnostics and with < and similar operations,
// so please do not reorder existing values.
enum class AccessLevel : uint8_t {
  /// Private access is limited to the current scope.
  Private = 0,
  /// File-private access is limited to the current file.
  FilePrivate,
  /// Internal access is limited to the current module.
  Internal,
  /// Package access is not limited, but some capabilities may be
  /// restricted outside of the current package containing modules.
  /// It's similar to Public in that it's accessible from other modules
  /// and subclassable only within the defining module as long as
  /// the modules are in the same package.
  Package,
  /// Public access is not limited, but some capabilities may be
  /// restricted outside of the current module.
  Public,
  /// Open access is not limited, and all capabilities are unrestricted.
  Open,
};

/// Returns the in-source spelling of the given access level.
StringRef getAccessLevelSpelling(AccessLevel value);

enum class InlineKind : uint8_t {
  Never = 0,
  Always = 1,
  Last_InlineKind = Always
};

enum : unsigned { NumInlineKindBits =
  countBitsUsed(static_cast<unsigned>(InlineKind::Last_InlineKind)) };


/// This enum represents the possible values of the @_effects attribute.
/// These values are ordered from the strongest guarantee to the weakest,
/// so please do not reorder existing values.
enum class EffectsKind : uint8_t {
  ReadNone,
  ReadOnly,
  ReleaseNone,
  ReadWrite,
  Unspecified,
  Custom,
  Last_EffectsKind = Unspecified
};

enum : unsigned { NumEffectsKindBits =
  countBitsUsed(static_cast<unsigned>(EffectsKind::Last_EffectsKind)) };

/// This enum represents the possible values of the @_expose attribute.
enum class ExposureKind: uint8_t {
  Cxx,
  Wasm,
  Last_ExposureKind = Wasm
};

enum : unsigned { NumExposureKindBits =
  countBitsUsed(static_cast<unsigned>(ExposureKind::Last_ExposureKind)) };
  
/// This enum represents the possible values of the @_extern attribute.
enum class ExternKind: uint8_t {
  /// Reference an externally defined C function.
  /// The imported function has C function pointer representation,
  /// and is called using the C calling convention.
  C,
  /// Reference an externally defined function through WebAssembly's
  /// import mechanism.
  /// This does not specify the calling convention and can be used
  /// with other extern kinds together.
  /// Effectively, this is no-op on non-WebAssembly targets.
  Wasm,
  Last_ExternKind = Wasm
};

enum : unsigned { NumExternKindBits =
  countBitsUsed(static_cast<unsigned>(ExternKind::Last_ExternKind)) };

enum DeclAttrKind : unsigned {
#define DECL_ATTR(_, NAME, ...) DAK_##NAME,
#include "swift/AST/Attr.def"
  DAK_Count
};

enum : unsigned { NumDeclAttrKindBits =
  countBitsUsed(static_cast<unsigned>(DeclAttrKind::DAK_Count - 1)) };

// Define enumerators for each type attribute, e.g. TAK_weak.
enum TypeAttrKind {
#define TYPE_ATTR(X) TAK_##X,
#include "swift/AST/Attr.def"
  TAK_Count
};

} // end namespace swift

#endif
