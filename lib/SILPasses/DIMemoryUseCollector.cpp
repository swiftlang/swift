//===--- DefiniteInitialization.cpp - Perform definite init analysis ------===//
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

#define DEBUG_TYPE "definite-init"
#include "DIMemoryUseCollector.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/StringExtras.h"
using namespace swift;

//===----------------------------------------------------------------------===//
//                  Tuple Element Flattening/Counting Logic
//===----------------------------------------------------------------------===//

/// getElementCount - Return the number of elements in the flattened type.
/// For tuples, this is the (recursive) count of the fields it contains,
/// otherwise this is 1.
unsigned TF::getElementCount(CanType T) {
  CanTupleType TT = dyn_cast<TupleType>(T);
  
  // If this isn't a tuple, it is a single element.
  if (!TT) return 1;
  
  unsigned NumElements = 0;
  for (auto EltTy : TT.getElementTypes())
    NumElements += TF::getElementCount(EltTy);
  return NumElements;
}

/// Given a symbolic element number, return the type of the element.
CanType TF::getElementType(CanType T, unsigned EltNo) {
  TupleType *TT = T->getAs<TupleType>();
  
  // If this isn't a tuple, it is a leaf element.
  if (!TT) {
    assert(EltNo == 0);
    return T;
  }
  
  for (auto &Elt : TT->getFields()) {
    auto FieldType = Elt.getType()->getCanonicalType();
    unsigned NumFields = TF::getElementCount(FieldType);
    if (EltNo < NumFields)
      return TF::getElementType(FieldType, EltNo);
    EltNo -= NumFields;
  }
  
  assert(0 && "invalid element number");
  abort();
}

/// computeTupleElementAddress - Given a tuple element number (in the flattened
/// sense) return a pointer to a leaf element of the specified number.
SILValue TF::emitElementAddress(SILValue Ptr, unsigned TupleEltNo,
                                SILLocation Loc, SILBuilder &B) {
  CanType PointeeType = Ptr.getType().getSwiftRValueType();
  while (1) {
    // Have we gotten to our leaf element?
    CanTupleType TT = dyn_cast<TupleType>(PointeeType);
    if (TT == 0) {
      assert(TupleEltNo == 0 && "Element count problem");
      return Ptr;
    }
    
    // Figure out which field we're walking into.
    unsigned FieldNo = 0;
    for (auto EltTy : TT.getElementTypes()) {
      unsigned NumSubElt = TF::getElementCount(EltTy);
      if (TupleEltNo < NumSubElt) {
        Ptr = B.createTupleElementAddr(Loc, Ptr, FieldNo);
        PointeeType = EltTy;
        break;
      }
      
      TupleEltNo -= NumSubElt;
      ++FieldNo;
    }
  }
}


/// Push the symbolic path name to the specified element number onto the
/// specified std::string.
void TF::getPathStringToElement(CanType T, unsigned Element,
                                std::string &Result) {
  CanTupleType TT = dyn_cast<TupleType>(T);
  if (!TT) return;
  
  unsigned FieldNo = 0;
  for (auto &Field : TT->getFields()) {
    CanType FieldTy(Field.getType());
    unsigned ElementsForField = TF::getElementCount(FieldTy);
    
    if (Element < ElementsForField) {
      Result += '.';
      if (Field.hasName())
        Result += Field.getName().str();
      else
        Result += llvm::utostr(FieldNo);
      return TF::getPathStringToElement(FieldTy, Element, Result);
    }
    
    Element -= ElementsForField;
    
    ++FieldNo;
  }
  assert(0 && "Element number is out of range for this type!");
}


//===----------------------------------------------------------------------===//
//                        DIMemoryUse Implementation
//===----------------------------------------------------------------------===//


/// onlyTouchesTrivialElements - Return true if all of the accessed elements
/// have trivial type.
bool DIMemoryUse::onlyTouchesTrivialElements(SILType MemoryType) const {
  CanType MemoryCType = MemoryType.getSwiftRValueType();
  auto &Module = Inst->getModule();
  
  for (unsigned i = FirstTupleElement, e = i+NumTupleElements; i != e; ++i){
    auto EltTy = TF::getElementType(MemoryCType, i);
    if (!SILType::getPrimitiveObjectType(EltTy).isTrivial(Module))
      return false;
  }
  return true;
}
