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

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/ArrayRef.h>
#include <initializer_list>
#include "swift/AST/Types.h"

namespace swift {
namespace Lowering {
  class SILGen;

/// ReferenceTypeElement - a path to a reference type element within a loadable
/// aggregate type at an arbitrary depth.
struct ReferenceTypeElement {
  struct Component {
    Type type;
    unsigned index;
  };
  /// index - The index chain leading to the reference type element. For
  /// example, {0} refers to element zero, {0, 1} refers to element
  /// one of element zero, etc. An empty index list {} refers to the value
  /// itself, for reference types.
  llvm::SmallVector<Component, 4> path;
};

class LLVM_LIBRARY_VISIBILITY TypeInfo {
  friend class TypeConverter;
  friend class LoadableTypeInfoVisitor;

  llvm::SmallVector<ReferenceTypeElement, 4> referenceTypeElements;
  
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
};

/// TypeConverter - helper class for creating and managing TypeInfos.
class LLVM_LIBRARY_VISIBILITY TypeConverter {
  SILGen &gen;
  llvm::DenseMap<TypeBase *, TypeInfo> types;
  
  TypeInfo const &makeTypeInfo(CanType t);
  
public:
  TypeConverter(SILGen &gen) : gen(gen) {}
  
  TypeInfo const &getTypeInfo(Type t);
};

} // namespace Lowering
} // namespace swift

#endif
