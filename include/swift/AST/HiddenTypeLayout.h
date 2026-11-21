//===--- HiddenTypeLayout.h - Hidden type layout analysis ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_HIDDENTYPELAYOUT_H
#define SWIFT_AST_HIDDENTYPELAYOUT_H

#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"

namespace swift {

class Decl;
class FileUnit;
class ModuleDecl;
class NominalTypeDecl;
class ValueDecl;

enum class HiddenTypeLayoutOrigin {
  ImplementationOnly,
  InternalBridgingHeader,
  RecoveredHiddenType,
};

/// The hidden representation needed to preserve an ABI-contributing type.
enum class HiddenTypeRepresentationKind {
  /// The hidden declaration's identity is needed to resolve a type expression,
  /// but its layout does not contribute to the enclosing type's layout. If the
  /// type expression cannot be resolved, deserialization may drop the
  /// declaration that uses it and produce an incorrect enclosing layout.
  ///
  /// For example, suppose `HiddenProtocol` and `HiddenValue` come from a
  /// hidden dependency:
  ///
  /// \code
  /// struct Storage<T: HiddenProtocol> {
  ///   var value: Int
  /// }
  ///
  /// public struct Wrapper {
  ///   private var storage: Storage<HiddenValue>
  /// }
  /// \endcode
  ///
  /// The client must resolve the hidden references to preserve `storage`, but
  /// their layouts do not contribute to `Storage` in this example.
  Opaque,

  /// The declaration's layout contributes to client-visible storage.
  AbstractTypeLayout,
};

struct HiddenTypeLayoutRequirement {
  const Decl *LayoutDecl;
  Type HiddenType;
  HiddenTypeLayoutOrigin Origin;
  HiddenTypeRepresentationKind Kind;
  NominalTypeDecl *ABIExposedType;
  ValueDecl *LayoutAffectingStorage;
};

/// Find hidden declarations needed to deserialize this module's
/// client-visible ABI.
void forEachRequiredHiddenTypeLayout(
    const ModuleDecl *module, ArrayRef<const FileUnit *> files,
    llvm::function_ref<void(const HiddenTypeLayoutRequirement &)> callback);

} // end namespace swift

#endif // SWIFT_AST_HIDDENTYPELAYOUT_H
