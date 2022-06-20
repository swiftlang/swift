//===--- IRABIDetailsProvider.h - Get ABI details for decls -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_IRABIDETAILSPROVIDER_H
#define SWIFT_IRGEN_IRABIDETAILSPROVIDER_H

#include "swift/AST/Type.h"
#include "clang/AST/CharUnits.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include <cstdint>
#include <memory>
#include <utility>

namespace swift {

class ASTContext;
class IRGenOptions;
class ModuleDecl;
class NominalTypeDecl;

class IRABIDetailsProviderImpl;

/// Provides access to the IRGen-based queries that can be performed on
/// declarations to get their various ABI details.
class IRABIDetailsProvider {
public:
  IRABIDetailsProvider(ModuleDecl &mod, const IRGenOptions &opts);
  ~IRABIDetailsProvider();

  using SizeType = uint64_t;

  struct SizeAndAlignment {
    SizeType size;
    SizeType alignment;
  };

  /// Returns the size and alignment for the given type, or \c None if the type
  /// is not a fixed layout type.
  llvm::Optional<SizeAndAlignment>
  getTypeSizeAlignment(const NominalTypeDecl *TD);

  /// Returns true if the given type should be passed indirectly into a swiftcc
  /// function.
  bool shouldPassIndirectly(Type t);

  /// Returns true if the given type should be returned indirectly from a
  /// swiftcc function.
  bool shouldReturnIndirectly(Type t);

  /// Enumerates all of the members of the underlying record in terms of their
  /// primitive types that needs to be stored in a Clang/LLVM record when this
  /// type is passed or returned directly to/from swiftcc function.
  ///
  /// Returns true if an error occurred when a particular member can't be
  /// represented with an AST type.
  bool enumerateDirectPassingRecordMembers(
      Type t, llvm::function_ref<void(clang::CharUnits, clang::CharUnits, Type)>
                  callback);

  /// An representation of a single type, or a C struct with multiple members
  /// with specified types. The C struct is expected to be passed via swiftcc
  /// functions.
  class TypeRecordABIRepresentation {
  public:
    ArrayRef<Type> getMembers() const { return members; }

    using MemberVectorTy = SmallVector<Type, 4>;

  private:
    friend class IRABIDetailsProviderImpl;
    TypeRecordABIRepresentation(MemberVectorTy members) : members(members) {}

    MemberVectorTy members;
  };

  struct FunctionABISignature {
    TypeRecordABIRepresentation returnType;
    SmallVector<TypeRecordABIRepresentation, 4> parameterTypes;
  };

  /// Returns the function signature that is used for the the type metadata
  /// access function.
  FunctionABISignature getTypeMetadataAccessFunctionSignature();

private:
  std::unique_ptr<IRABIDetailsProviderImpl> impl;
};

} // namespace swift

#endif
