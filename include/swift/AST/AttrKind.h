//===--- AttrKind.h - Enumerate attribute kinds  ----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
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

/// This header is included in a bridging header. Be *very* careful with what
/// you include here! See include caveats in `ASTBridging.h`.
#include "swift/Basic/SwiftBridging.h"
#include <stdint.h>

namespace llvm {
class StringRef;
}

namespace swift {

/// The associativity of a binary operator.
enum class ENUM_EXTENSIBILITY_ATTR(closed) Associativity : uint8_t {
  /// Non-associative operators cannot be written next to other
  /// operators with the same precedence.  Relational operators are
  /// typically non-associative.
  None SWIFT_NAME("none"),

  /// Left-associative operators associate to the left if written next
  /// to other left-associative operators of the same precedence.
  Left SWIFT_NAME("left"),

  /// Right-associative operators associate to the right if written
  /// next to other right-associative operators of the same precedence.
  Right SWIFT_NAME("right")
};

/// Returns the in-source spelling of the given associativity.
SWIFT_UNAVAILABLE("Unavailable in Swift")
llvm::StringRef getAssociativitySpelling(Associativity value);

/// Access control levels.
// These are used in diagnostics and with < and similar operations,
// so please do not reorder existing values.
enum class ENUM_EXTENSIBILITY_ATTR(closed) AccessLevel : uint8_t {
  /// Private access is limited to the current scope.
  Private SWIFT_NAME("private") = 0,
  /// File-private access is limited to the current file.
  FilePrivate SWIFT_NAME("fileprivate"),
  /// Internal access is limited to the current module.
  Internal SWIFT_NAME("internal"),
  /// Package access is not limited, but some capabilities may be
  /// restricted outside of the current package containing modules.
  /// It's similar to Public in that it's accessible from other modules
  /// and subclassable only within the defining module as long as
  /// the modules are in the same package.
  Package SWIFT_NAME("package"),
  /// Public access is not limited, but some capabilities may be
  /// restricted outside of the current module.
  Public SWIFT_NAME("public"),
  /// Open access is not limited, and all capabilities are unrestricted.
  Open SWIFT_NAME("open"),
};

/// Returns the in-source spelling of the given access level.
SWIFT_UNAVAILABLE("Unavailable in Swift")
llvm::StringRef getAccessLevelSpelling(AccessLevel value);

enum class ENUM_EXTENSIBILITY_ATTR(closed) InlineKind : uint8_t {
  Never SWIFT_NAME("never") = 0,
  Always SWIFT_NAME("always") = 1,
  Last_InlineKind = Always
};

/// This enum represents the possible values of the @_effects attribute.
/// These values are ordered from the strongest guarantee to the weakest,
/// so please do not reorder existing values.
enum class ENUM_EXTENSIBILITY_ATTR(closed) EffectsKind : uint8_t {
  ReadNone SWIFT_NAME("readnone"),
  ReadOnly SWIFT_NAME("readonly"),
  ReleaseNone SWIFT_NAME("releasenone"),
  ReadWrite SWIFT_NAME("readwrite"),
  Unspecified SWIFT_NAME("unspecified"),
  Custom SWIFT_NAME("custom"),
  Last_EffectsKind = Custom
};

/// This enum represents the possible values of the @_expose attribute.
enum class ENUM_EXTENSIBILITY_ATTR(closed) ExposureKind : uint8_t {
  Cxx SWIFT_NAME("cxx"),
  Wasm SWIFT_NAME("wasm"),
  Last_ExposureKind = Wasm
};

/// This enum represents the possible values of the @_extern attribute.
enum class ENUM_EXTENSIBILITY_ATTR(closed) ExternKind : uint8_t {
  /// Reference an externally defined C function.
  /// The imported function has C function pointer representation,
  /// and is called using the C calling convention.
  C SWIFT_NAME("c"),
  /// Reference an externally defined function through WebAssembly's
  /// import mechanism.
  /// This does not specify the calling convention and can be used
  /// with other extern kinds together.
  /// Effectively, this is no-op on non-WebAssembly targets.
  Wasm SWIFT_NAME("wasm"),
  Last_ExternKind = Wasm
};

enum class ENUM_EXTENSIBILITY_ATTR(closed) NonIsolatedModifier : uint8_t {
  None SWIFT_NAME("none") = 0,
  Unsafe SWIFT_NAME("unsafe"),
  NonSending SWIFT_NAME("nonsending"),
  Last_NonIsolatedModifier = NonSending
};

enum class ENUM_EXTENSIBILITY_ATTR(closed)
    InheritActorContextModifier : uint8_t {
      /// Inherit the actor execution context if the isolated parameter was
      /// captured by the closure, context is nonisolated or isolated to a
      /// global actor.
      None SWIFT_NAME("none") = 0,
      /// Always inherit the actor context, even when the isolated parameter
      /// for the context is not closed over explicitly.
      Always SWIFT_NAME("always"),
      Last_InheritActorContextKind = Always
    };

enum class ENUM_EXTENSIBILITY_ATTR(closed) NonexhaustiveMode : uint8_t {
  Error SWIFT_NAME("error") = 0,
  Warning SWIFT_NAME("warning") = 1,
  Last_NonexhaustiveMode = Warning
};

enum class ENUM_EXTENSIBILITY_ATTR(closed) DeclAttrKind : unsigned {
#define DECL_ATTR(_, CLASS, ...) CLASS,
#define LAST_DECL_ATTR(CLASS) Last_DeclAttr = CLASS,
#include "swift/AST/DeclAttr.def"
};

SWIFT_UNAVAILABLE("Unavailable in Swift")
llvm::StringRef getDeclAttrKindID(DeclAttrKind kind);

enum : unsigned {
  NumDeclAttrKinds = static_cast<unsigned>(DeclAttrKind::Last_DeclAttr) + 1
};

// Define enumerators for each type attribute, e.g. TypeAttrKind::Weak.
enum class ENUM_EXTENSIBILITY_ATTR(closed) TypeAttrKind {
#define TYPE_ATTR(_, CLASS) CLASS,
#define LAST_TYPE_ATTR(CLASS) Last_TypeAttr = CLASS,
#include "swift/AST/TypeAttr.def"
};

enum : unsigned {
  NumTypeAttrKinds = static_cast<unsigned>(TypeAttrKind::Last_TypeAttr) + 1
};

} // end namespace swift

#endif
