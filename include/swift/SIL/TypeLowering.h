//===--- TypeLowering.h - Convert Swift Types to SILTypes -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SIL_TypeLowering_h
#define SIL_TypeLowering_h

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/ArrayRef.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILModule.h"

namespace swift {
  class ValueDecl;
namespace Lowering {

/// Given a function type or polymorphic function type, returns the same type
/// with the [thin] attribute added.
/// FIXME: The thinness of func decls should be checked by the Swift
/// typechecker.
Type getThinFunctionType(Type t);

/// Given a function type or polymorphic function type, returns the same type
/// with the [thin] attribute removed.
/// FIXME: The thinness of func decls should be checked by the Swift
/// typechecker.
Type getThickFunctionType(Type t);

/// CaptureKind - Different ways in which a function can capture context.
enum class CaptureKind {
  /// A local value captured as a mutable box.
  LValue,
  /// A local value captured by value.
  Constant,
  /// A byref argument captured by address.
  Byref,
  /// A getter-only property.
  Getter,
  /// A settable property.
  GetterSetter
};
  
/// getDeclCaptureKind - Return the CaptureKind to use when capturing a decl.
CaptureKind getDeclCaptureKind(ValueDecl *capture);
  
/// ReferenceTypeElement - a path to a reference type element within a loadable
/// aggregate type at an arbitrary depth.
struct ReferenceTypePath {
  /// A component of the reference type path, comprising the index of an
  /// element and its type.
  struct Component {
    Type type;
    unsigned index;
  };
  
  /// path - The index chain leading to the reference type element. For
  /// example, {0} refers to element zero, {0, 1} refers to element
  /// one of element zero, etc. An empty index list {} refers to the value
  /// itself, for reference types.
  llvm::SmallVector<Component, 4> path;
};

/// TypeLoweringInfo - Extended type information used by SILGen.
class TypeLoweringInfo {
  friend class TypeConverter;
  friend class LoadableTypeLoweringInfoVisitor;

  /// referenceTypeElements - For a loadable type, this contains element index
  /// paths to every element inside the aggregate that must be retained and
  /// released.
  llvm::SmallVector<ReferenceTypePath, 4> referenceTypeElements;
  
  /// loweredType - The SIL type of values with this Swift type.
  SILType loweredType;
  
public:
  TypeLoweringInfo() = default;

  TypeLoweringInfo(TypeLoweringInfo const &) = delete;
  TypeLoweringInfo &operator=(TypeLoweringInfo const &) = delete;
  TypeLoweringInfo(TypeLoweringInfo &&) = default;
  TypeLoweringInfo &operator=(TypeLoweringInfo &&) = default;
  
  /// isAddressOnly - Returns true if the type is an address-only type. A type
  /// is address-only if it is a resilient value type, or if it is a fragile
  /// value type with a resilient member. In either case, the full layout of
  /// values of the type is unavailable to the compiler.
  bool isAddressOnly() const { return loweredType.isAddressOnly(); }
  /// isLoadable - Returns true if the type is loadable, in other words, its
  /// full layout is available to the compiler. This is the inverse of
  /// isAddressOnly.
  bool isLoadable() const { return loweredType.isLoadable(); }
  
  /// isTrivial - Returns true if the type is trivial, meaning it is a loadable
  /// value type with no reference type members that require releasing.
  bool isTrivial() const {
    return loweredType.isLoadable() && referenceTypeElements.empty();
  }
  
  /// getReferenceTypeElements - For a nontrivial loadable value type, returns
  /// an array of ReferenceTypePaths addressing the reference type elements.
  llvm::ArrayRef<ReferenceTypePath> getReferenceTypeElements() const {
    return referenceTypeElements;
  }
  
  /// getLoweredType - Get the type used to represent values of the Swift type
  /// in SIL.
  SILType getLoweredType() const {
    return loweredType;
  }
};

/// TypeConverter - helper class for creating and managing TypeLoweringInfos.
class TypeConverter {
  llvm::BumpPtrAllocator TypeLoweringInfoBPA;
  llvm::DenseMap<TypeBase *, TypeLoweringInfo *> types;
  llvm::DenseMap<SILConstant, SILType> constantTypes;
  
  TypeLoweringInfo const &makeTypeLoweringInfo(CanType t,
                                               unsigned uncurryLevel);
  SILTypeInfo *makeSILTypeInfo(TypeLoweringInfo &theInfo);
  void makeLayoutForDecl(SmallVectorImpl<SILCompoundTypeInfo::Element> &theInfo,
                         NominalTypeDecl *decl);
  SILFunctionTypeInfo *makeInfoForFunctionType(AnyFunctionType *ft,
                                               unsigned uncurryLevel);

  Type makeConstantType(SILConstant constant);
  
public:
  SILModule &M;
  ASTContext &Context;

  TypeConverter(SILModule &sgm);
  ~TypeConverter();
  TypeConverter(TypeConverter const &) = delete;
  TypeConverter &operator=(TypeConverter const &) = delete;

  /// Returns the SIL TypeLoweringInfo for a SIL type.
  TypeLoweringInfo const &getTypeLoweringInfo(Type t,
                                              unsigned uncurryLevel = 0);
  
  // Returns the lowered SIL type for a Swift type.
  SILType getLoweredType(Type t, unsigned uncurryLevel = 0) {
    return getTypeLoweringInfo(t, uncurryLevel).loweredType;
  }
  
  /// Returns the SIL type of a constant reference.
  SILType getConstantType(SILConstant constant);
  
  /// Returns the type of the "this" parameter to methods of a type.
  Type getMethodThisType(Type thisType) const;
  /// Returns the type of a property accessor, () -> T for a getter,
  /// or (value:T) -> () for a setter. 'kind' must be one of the Kind constants
  /// from SILConstant, SILConstant::Getter or SILConstant::Setter.
  Type getPropertyType(SILConstant::Kind kind, Type propType) const;
  /// Returns the type of a subscript property accessor, Index -> () -> T
  /// for a getter, or Index -> (value:T) -> () for a setter.
  /// 'kind' must be one of the Kind constants
  /// from SILConstant, SILConstant::Getter or SILConstant::Setter.
  Type getSubscriptPropertyType(SILConstant::Kind kind,
                                Type indexType,
                                Type elementType) const;

  /// Get the type of a method of function type M for a type:
  ///   This -> M for a concrete This,
  ///   <T,U,...> This -> M for an unbound generic This,
  ///   or the type M of the function itself if the context type is null.
  Type getMethodTypeInContext(Type /*nullable*/ contextType,
                              Type methodType,
                              GenericParamList *genericParams = nullptr) const;
  
};

} // namespace Lowering
} // namespace swift

#endif
