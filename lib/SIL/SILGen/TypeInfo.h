//===--- TypeInfo.h - Type information relevant to SILGen -------*- C++ -*-===//
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

#ifndef SILGen_TypeInfo_h
#define SILGen_TypeInfo_h

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/ArrayRef.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILModule.h"

namespace swift {
namespace Lowering {
  class SILGenFunction;
  class SILGenModule;

/// FragileElement - a description of the index of a fragile aggregate element
/// and its type.
struct FragileElement {
  Type type;
  unsigned index;
};

/// ReferenceTypeElement - a path to a reference type element within a loadable
/// aggregate type at an arbitrary depth.
struct ReferenceTypeElement {
  /// path - The index chain leading to the reference type element. For
  /// example, {0} refers to element zero, {0, 1} refers to element
  /// one of element zero, etc. An empty index list {} refers to the value
  /// itself, for reference types.
  llvm::SmallVector<FragileElement, 4> path;
};

/// TypeInfo - Extended type information used by SILGen.
class LLVM_LIBRARY_VISIBILITY TypeInfo {
  friend class TypeConverter;
  friend class LoadableTypeInfoVisitor;

  /// referenceTypeElements - For a loadable type, this contains element index
  /// paths to every element inside the aggregate that must be retained and
  /// released.
  llvm::SmallVector<ReferenceTypeElement, 4> referenceTypeElements;
  
  /// fragileElements - For a loadable struct type, this contains mappings from
  /// member identifiers to SIL element indexes. Empty for address-only or
  /// non-struct types.
  llvm::DenseMap<Identifier, FragileElement> fragileElements;
  
  /// loweredType - The SIL type of values with this Swift type.
  SILType loweredType;
  
public:
  TypeInfo() = default;

  TypeInfo(TypeInfo const &) = delete;
  void operator=(TypeInfo const &) = delete;
  TypeInfo(TypeInfo &&) = default;
  TypeInfo &operator=(TypeInfo &&) = default;
  
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
  bool isTrivial() const { return referenceTypeElements.empty(); }
  
  /// getReferenceTypeElements - For a nontrivial loadable value type, returns
  /// an array of ReferenceTypeElements addressing the reference type elements
  llvm::ArrayRef<ReferenceTypeElement> getReferenceTypeElements() const {
    return referenceTypeElements;
  }
  
  /// hasFragileElement - Returns true if this TypeInfo represents a loadable
  /// struct type and it has a physical member with the given name.
  bool hasFragileElement(Identifier name) const {
    return fragileElements.count(name);
  }
  
  /// getLoweredType - Get the type used to represent values of the Swift type
  /// in SIL.
  ///
  /// - For address-only types, this is the address type pointing to the Swift
  ///   type. For example, Swift::
  ///
  ///     struct [API] AddressOnly {}
  ///
  ///   lowers to SIL `*AddressOnly`.
  /// - For function types, the argument types are lowered, and if the return
  ///   type is address-only, it is rewritten as an address parameter, and the
  ///   lowered return value is the empty tuple. Function types are lowered
  ///   recursively. For example, these function types::
  ///
  ///     struct [API] AddressOnly {}
  ///     struct Fragile {}
  ///
  ///     (a:AddressOnly, b:Fragile) -> Fragile
  ///     (a:Fragile, b:(c:AddressOnly) -> Fragile) -> AddressOnly
  ///
  ///   lower to:
  ///
  ///     (a:*AddressOnly, b:Fragile) -> Fragile
  ///     (a:Fragile, b:(c:*AddressOnly) -> Fragile, _:*AddressOnly) -> ()
  ///
  SILType getLoweredType() const { return loweredType; }
  
  /// getFragileElement - For a loadable struct type, returns the index and type
  /// of the element named by the given identifier. The named element must exist
  /// in the type and be a physical member.
  FragileElement getFragileElement(Identifier name) const {
    auto found = fragileElements.find(name);
    assert(found != fragileElements.end() &&
           "element name does not exist in type, or type isn't loadable");
    return found->second;
  }
};

/// TypeConverter - helper class for creating and managing TypeInfos.
class LLVM_LIBRARY_VISIBILITY TypeConverter {
  llvm::DenseMap<TypeBase *, TypeInfo> types;
  llvm::DenseMap<SILConstant, SILType> constantTypes;
  
  TypeInfo const &makeTypeInfo(CanType t);
  void makeFragileElements(TypeInfo &theInfo, CanType t);
  void makeFragileElementsForDecl(TypeInfo &theInfo, NominalTypeDecl *decl);

  Type makeConstantType(SILConstant constant);
  
public:
  ASTContext &Context;

  TypeConverter(SILGenModule &sgm);

  /// Returns the SIL TypeInfo for a type.
  TypeInfo const &getTypeInfo(Type t);
  
  /// Returns the SIL type of a constant reference.
  SILType getConstantType(SILConstant constant);
  
  /// Returns the type of the "this" parameter to methods of a type.
  Type getMethodThisType(Type thisType) const;
  /// Returns the type of a property accessor, () -> T for a getter,
  /// or (value:T) -> () for a setter.
  Type getPropertyType(unsigned id, Type propType) const;
  /// Returns the type of a subscript property accessor, Index -> () -> T
  /// for a getter, or Index -> (value:T) -> () for a setter.
  Type getSubscriptPropertyType(unsigned id,
                                Type indexType,
                                Type elementType) const;

  /// Get the type of a method of function type M for a type:
  ///   This -> M for a concrete This,
  ///   <T,U,...> This -> M for an unbound generic This,
  ///   or the type M of the function itself if the context type is null.
  Type getMethodTypeInContext(Type /*nullable*/ contextType,
                              Type methodType) const;
  
};

} // namespace Lowering
} // namespace swift

#endif
