//===--- NameLookup.cpp - Swift Name Lookup Routines ----------------------===//
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
//
// This file implements interfaces for performing name lookup.
//
//===----------------------------------------------------------------------===//


#include "NameLookup.h"
#include "swift/AST/AST.h"
using namespace swift;

MemberLookup::MemberLookup(Type BaseTy, Identifier Name, Module &M) {
  doIt(BaseTy, Name, M);
}

/// doIt - Lookup a member 'Name' in 'BaseTy' within the context
/// of a given module 'M'.  This operation corresponds to a standard "dot" 
/// lookup operation like "a.b" where 'this' is the type of 'a'.  This
/// operation is only valid after name binding.
void MemberLookup::doIt(Type BaseTy, Identifier Name, Module &M) {
  typedef MemberLookupResult Result;
  
  // Just look through l-valueness.  It doesn't affect name lookup.
  if (LValueType *LV = BaseTy->getAs<LValueType>())
    BaseTy = LV->getObjectType();

  // Type check metatype references, as in "some_type.some_member".  These are
  // special and can't have extensions.
  if (MetaTypeType *MTT = BaseTy->getAs<MetaTypeType>()) {
    // The metatype represents an arbitrary named type: dig through the
    // TypeAlias to see what we're dealing with.  If the typealias was erroneous
    // then silently squish this erroneous subexpression.
    Type Ty = MTT->getTypeDecl()->getUnderlyingType();
    
    // Handle references the to constructors of a oneof.
    if (OneOfType *OOTy = Ty->getAs<OneOfType>()) {
      OneOfElementDecl *Elt = OOTy->getElement(Name);
      if (Elt) {
        Results.push_back(Result::getIgnoreBase(Elt));
        return;
      }
    }
    
    // Otherwise, just perform normal dot lookup on the type with the specified
    // member name to see if we find extensions or anything else.  For example,
    // If type SomeTy.SomeMember can look up plus functions, and can even look
    // up non-plus functions as well (thus getting the address of the member).
    doIt(Ty, Name, M);

    // If we find anything that requires 'this', reset it back because we don't
    // have a this.
    for (Result &R : Results)
      // No 'this' to pass.
      if (R.Kind == Result::PassBase)
        R.Kind = Result::IgnoreBase;
    
    return;
  }
  
  // Lookup module references, as on some_module.some_member.  These are
  // special and can't have extensions.
  if (ModuleType *MT = BaseTy->getAs<ModuleType>()) {
    SmallVector<ValueDecl*, 8> Decls;
    MT->getModule()->lookupValue(Module::AccessPathTy(), Name,
                                 NLKind::QualifiedLookup, Decls);
    for (ValueDecl *VD : Decls)
      Results.push_back(Result::getIgnoreBase(VD));
    return;
  }

  // If the base is a protocol, see if this is a reference to a declared
  // protocol member.
  if (ProtocolType *PT = BaseTy->getAs<ProtocolType>()) {
    for (ValueDecl *VD : PT->Elements) {
      if (VD->getName() != Name) continue;
      
      // If this is a 'plus' function, then just ignore the base expression.
      if (FuncDecl *FD = dyn_cast<FuncDecl>(VD))
        if (FD->isPlus()) {
          Results.push_back(Result::getIgnoreBase(FD));
          return;
        }
      
      Results.push_back(Result::getPassBase(VD));
      return;
    }
  }
  
  // Check to see if this is a reference to a tuple field.
  if (TupleType *TT = BaseTy->getAs<TupleType>())
    doTuple(TT, Name, false);

  // If this is a member access to a oneof with a single element constructor
  // (e.g. a struct), allow direct access to the type underlying the single
  // element.
  if (OneOfType *OneOf = BaseTy->getAs<OneOfType>())
    if (OneOf->isTransparentType())
      if (TupleType *TT = OneOf->getTransparentType()->getAs<TupleType>())
        doTuple(TT, Name, true);
  

  // Look in any extensions that add methods to the base type.
  SmallVector<ValueDecl*, 8> ExtensionMethods;
  M.lookupGlobalExtensionMethods(BaseTy, Name, ExtensionMethods);

  for (ValueDecl *VD : ExtensionMethods) {
    if (FuncDecl *FD = dyn_cast<FuncDecl>(VD))
      if (FD->isPlus()) {
        Results.push_back(Result::getIgnoreBase(FD));
        continue;
      }
    Results.push_back(Result::getPassBase(VD));
  }
}

void MemberLookup::doTuple(TupleType *TT, Identifier Name, bool IsStruct) {
  // If the field name exists, we win.  Otherwise, if the field name is a
  // dollarident like $4, process it as a field index.
  int FieldNo = TT->getNamedElementId(Name);
  if (FieldNo != -1) {
    Results.push_back(MemberLookupResult::getTupleElement(FieldNo, IsStruct));
    return;
  }
  
  StringRef NameStr = Name.str();
  if (NameStr.startswith("$")) {
    unsigned Value = 0;
    if (!NameStr.substr(1).getAsInteger(10, Value) &&
        Value < TT->getFields().size())
      Results.push_back(MemberLookupResult::getTupleElement(Value, IsStruct));
  }
}


