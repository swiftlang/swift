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

#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/IRGen/GenericRequirement.h"
#include "clang/AST/CharUnits.h"
#include "llvm/ADT/MapVector.h"
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
class ParamDecl;

class IRABIDetailsProviderImpl;

namespace irgen {

class SignatureExpansionABIDetails;
class TypeInfo;

} // namespace irgen

/// Describes the lowered Swift function signature.
class LoweredFunctionSignature {
public:
  class DirectResultType {
  public:
    /// Enumerates all of the members of the underlying record in terms of
    /// their primitive types that needs to be stored in a Clang/LLVM record
    /// when this type is passed or returned directly to/from swiftcc
    /// function.
    ///
    /// Returns true if an error occurred when a particular member can't be
    /// represented with an AST type.
    bool enumerateRecordMembers(
        llvm::function_ref<void(clang::CharUnits, clang::CharUnits, Type)>
            callback) const;

  private:
    DirectResultType(IRABIDetailsProviderImpl &owner,
                     const irgen::TypeInfo &typeDetails);
    IRABIDetailsProviderImpl &owner;
    const irgen::TypeInfo &typeDetails;
    friend class LoweredFunctionSignature;
  };

  /// Represents a result value returned indirectly out of a function.
  class IndirectResultValue {
  public:
    /// Returns true if this indirect result type uses the `sret` LLVM
    /// attribute.
    inline bool hasSRet() const { return hasSRet_; }

  private:
    inline IndirectResultValue(bool hasSRet_) : hasSRet_(hasSRet_) {}
    bool hasSRet_;
    friend class LoweredFunctionSignature;
  };

  /// Represents a parameter passed directly to the function.
  class DirectParameter {
  public:
    /// Enumerates all of the members of the underlying record in terms of
    /// their primitive types that needs to be stored in a Clang/LLVM record
    /// when this type is passed or returned directly to/from swiftcc
    /// function.
    ///
    /// Returns true if an error occurred when a particular member can't be
    /// represented with an AST type.
    bool enumerateRecordMembers(
        llvm::function_ref<void(clang::CharUnits, clang::CharUnits, Type)>
            callback) const;

    inline const ParamDecl &getParamDecl() const { return paramDecl; }

  private:
    DirectParameter(IRABIDetailsProviderImpl &owner,
                    const irgen::TypeInfo &typeDetails,
                    const ParamDecl &paramDecl);
    IRABIDetailsProviderImpl &owner;
    const irgen::TypeInfo &typeDetails;
    const ParamDecl &paramDecl;
    friend class LoweredFunctionSignature;
  };

  /// Represents a parameter passed indirectly to the function.
  class IndirectParameter {
  public:
    inline const ParamDecl &getParamDecl() const { return paramDecl; }

  private:
    IndirectParameter(const ParamDecl &paramDecl);
    const ParamDecl &paramDecl;
    friend class LoweredFunctionSignature;
  };

  /// Represents a generic requirement paremeter that must be passed to the
  /// function.
  class GenericRequirementParameter {
  public:
    inline GenericRequirement getRequirement() const { return requirement; }

  private:
    GenericRequirementParameter(const GenericRequirement &requirement);
    GenericRequirement requirement;
    friend class LoweredFunctionSignature;
  };

  /// Represents a parameter which is a Swift type pointer sourced from a
  /// valid metadata source, like the type of another argument.
  class MetadataSourceParameter {
  public:
    inline CanType getType() const { return type; }

  private:
    MetadataSourceParameter(const CanType &type);
    CanType type;
    friend class LoweredFunctionSignature;
  };

  /// Represents a context parameter passed to the call.
  class ContextParameter {};

  /// Represents an out error parameter passed indirectly to the call.
  class ErrorResultValue {};

  /// Returns lowered direct result details, or \c None if direct result is
  /// void.
  llvm::Optional<DirectResultType> getDirectResultType() const;

  /// Returns the number of indirect result values in this function signature.
  size_t getNumIndirectResultValues() const;

  /// Traverse the entire parameter list of the function signature.
  ///
  /// The parameter list can include actual Swift function parameters, result
  /// values returned indirectly, and additional values, like generic
  /// requirements for polymorphic calls and the error parameter as well.
  void visitParameterList(
      llvm::function_ref<void(const IndirectResultValue &)>
          indirectResultVisitor,
      llvm::function_ref<void(const DirectParameter &)> directParamVisitor,
      llvm::function_ref<void(const IndirectParameter &)> indirectParamVisitor,
      llvm::function_ref<void(const GenericRequirementParameter &)>
          genericRequirementVisitor,
      llvm::function_ref<void(const MetadataSourceParameter &)>
          metadataSourceVisitor,
      llvm::function_ref<void(const ContextParameter &)> contextParamVisitor,
      llvm::function_ref<void(const ErrorResultValue &)> errorResultVisitor)
      const;

private:
  LoweredFunctionSignature(
      const AbstractFunctionDecl *FD, IRABIDetailsProviderImpl &owner,
      const irgen::SignatureExpansionABIDetails &abiDetails);
  const AbstractFunctionDecl *FD;
  IRABIDetailsProviderImpl &owner;
  const irgen::SignatureExpansionABIDetails &abiDetails;
  SmallVector<CanType, 1> metadataSourceTypes;
  friend class IRABIDetailsProviderImpl;
};

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

  /// Returns the function signature lowered to its C / LLVM - like
  /// representation, or \c None if such representation could not be computed.
  llvm::Optional<LoweredFunctionSignature>
  getFunctionLoweredSignature(AbstractFunctionDecl *fd);

  /// Returns the size and alignment for the given type, or \c None if the type
  /// is not a fixed layout type.
  llvm::Optional<SizeAndAlignment>
  getTypeSizeAlignment(const NominalTypeDecl *TD);

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

  /// Returns additional generic requirement parameters that are required to
  /// call the type metadata access function for the given type.
  SmallVector<GenericRequirement, 2>
  getTypeMetadataAccessFunctionGenericRequirementParameters(
      NominalTypeDecl *nominal);

  struct EnumElementInfo {
    unsigned tag;
    StringRef globalVariableName;
  };

  /// Returns EnumElementDecls (enum cases) in their declaration order with
  /// their tag indices from the given EnumDecl
  llvm::MapVector<EnumElementDecl *, EnumElementInfo>
  getEnumTagMapping(const EnumDecl *ED);

private:
  std::unique_ptr<IRABIDetailsProviderImpl> impl;
};

} // namespace swift

#endif
