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
  
  bool addressOnly : 1;

public:
  TypeInfo() : addressOnly(false) {}

  TypeInfo(TypeInfo const &) = delete;
  void operator=(TypeInfo const &) = delete;
  TypeInfo(TypeInfo &&) = default;
  TypeInfo &operator=(TypeInfo &&) = default;
  
  /// isAddressOnly - Returns true if the type is an address-only type. A type
  /// is address-only if it is a resilient value type, or if it is a fragile
  /// value type with a resilient member. In either case, the full layout of
  /// values of the type is unavailable to the compiler.
  bool isAddressOnly() const { return addressOnly; }
  /// isLoadable - Returns true if the type is loadable, in other words, its
  /// full layout is available to the compiler. This is the inverse of
  /// isAddressOnly.
  bool isLoadable() const { return !addressOnly; }
  
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
  
  TypeInfo const &makeTypeInfo(CanType t);
  void makeFragileElements(TypeInfo &theInfo, CanType t);
  void makeFragileElementsForDecl(TypeInfo &theInfo, NominalTypeDecl *decl);

public:
  TypeConverter(SILGenModule &sgm) { }
  
  TypeInfo const &getTypeInfo(Type t);
};

} // namespace Lowering
} // namespace swift

#endif
