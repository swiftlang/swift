//===--- SILBase.cpp - SIL Memory Allocation utilities --------------------===//
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

#include "swift/SIL/Value.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
using namespace swift;

namespace swift {

/// SILTypeList - The uniqued backing store for the Value type list.  This
/// is only expose out of Value as an ArrayRef of types, so it should never be
/// used outside of libSIL.
class SILTypeList : public llvm::FoldingSetNode {
public:
  unsigned NumTypes;
  Type Types[1];  // Actually variable sized.

  void Profile(llvm::FoldingSetNodeID &ID) const {
    for (unsigned i = 0, e = NumTypes; i != e; ++i)
      ID.AddPointer(Types[i].getPointer());
  }
};
}

ArrayRef<Type> Value::getTypes() const {
  // No results.
  if (Types.isNull())
    return ArrayRef<Type>();
  // Arbitrary list of results.
  if (auto TypeList = Types.dyn_cast<SILTypeList*>())
    return ArrayRef<Type>(TypeList->Types, TypeList->NumTypes);
  // Single result.
  return Types.get<Type>();
}

// FIXME: temporary.
Type Value::getType() const {
  assert(getTypes().size() == 1);
  return getTypes()[0];
}


/// SILTypeListUniquingType - This is the type of the folding set maintained by
/// SILBase that these things are uniqued into.
typedef llvm::FoldingSet<SILTypeList> SILTypeListUniquingType;

SILBase::SILBase() {
  TypeListUniquing = new SILTypeListUniquingType();
}

SILBase::~SILBase() {
  delete (SILTypeListUniquingType*)TypeListUniquing;
}


/// getSILTypeList - Get a uniqued pointer to a SIL type list.  This can only
/// be used by Value.
SILTypeList *SILBase::getSILTypeList(ArrayRef<Type> Types) const {
  assert(Types.size() > 1 && "Shouldn't use type list for 0 or 1 types");
  auto UniqueMap = (SILTypeListUniquingType*)TypeListUniquing;

  llvm::FoldingSetNodeID ID;
  for (auto T : Types)
    ID.AddPointer(T.getPointer());

  // If we already have this type list, just return it.
  void *InsertPoint = 0;
  if (SILTypeList *TypeList = UniqueMap->FindNodeOrInsertPos(ID, InsertPoint))
    return TypeList;

  // Otherwise, allocate a new one.
  void *NewListP = BPA.Allocate(sizeof(SILTypeList)+
                                sizeof(Type)*(Types.size()-1),
                                llvm::AlignOf<SILTypeList>::Alignment);
  SILTypeList *NewList = new (NewListP) SILTypeList();
  NewList->NumTypes = Types.size();
  std::copy(Types.begin(), Types.end(), NewList->Types);

  UniqueMap->InsertNode(NewList, InsertPoint);
  return NewList;
}


