//===--- TypeCheckType.cpp - Type Validation ------------------------------===//
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
// This file implements validation for Swift types, emitting semantic errors as
// appropriate and checking default initializer values.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// validateType - Recursively check to see if the type of a decl is valid.  If
/// not, diagnose the problem and collapse it to an ErrorType.
bool TypeChecker::validateType(ValueDecl *VD) {
  if (!validateType(VD->getType())) return false;
  
  VD->overwriteType(ErrorType::get(Context));
  return true;
}

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(Type InTy) {
  assert(InTy && "Cannot validate null types!");

  TypeBase *T = InTy.getPointer();
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If a type has a canonical type, then it is known safe.
  if (T->hasCanonicalTypeComputed()) return false;

  bool IsInvalid = false;
  
  switch (T->Kind) {
  case TypeKind::Error:
    // Error already diagnosed.
    return true;
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::Dependent:
    // These types are already canonical anyway.
    return false;
  case TypeKind::OneOf:
    for (OneOfElementDecl *Elt : cast<OneOfType>(T)->Elements) {
      // Ignore element decls that have no associated type.
      if (Elt->getArgumentType().isNull())
        continue;
      
      IsInvalid = validateType(Elt);
      if (IsInvalid) break;
    }
    break;
  case TypeKind::NameAlias:
    IsInvalid = validateType(cast<NameAliasType>(T)->TheDecl
                               ->getUnderlyingType());
    if (IsInvalid)
      cast<NameAliasType>(T)->TheDecl
        ->overwriteUnderlyingType(ErrorType::get(Context));
    break;
  case TypeKind::Paren:
    return validateType(cast<ParenType>(T)->getUnderlyingType());
  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->Fields[i].Ty;
      if (EltTy && validateType(EltTy)) {
        IsInvalid = true;
        break;
      }

      Expr *EltInit = TT->Fields[i].Init;
      if (EltInit == 0) continue;
      
      Expr *OldInit = EltInit;
      if (typeCheckExpression(EltInit, EltTy)) {
        diagnose(OldInit->getLoc(),diag::while_converting_default_tuple_value,
                 EltTy);
        IsInvalid = true;
        break;
      }
        
      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      assert(EltTy.isNull() || EltTy->isEqual(EltInit->getType()));
      EltTy = EltInit->getType();

      TT->updateInitializedElementType(i, EltTy, EltInit);
    }
    break;
  }
      
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(T);
    IsInvalid = validateType(FT->Input) || validateType(FT->Result);
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(T);
    IsInvalid = validateType(AT->Base);
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }
      
  case TypeKind::Protocol:
    // TODO: Validate Protocol types.
    break;
  }

  // If we determined that this type is invalid, erase it in the caller.
  if (IsInvalid) {
    InTy = ErrorType::get(Context);
    return true;
  }

  // Now that we decided that this type is ok, get the canonical type for it so
  // that we never reanalyze it again.
  // If it is ever a performance win to avoid computing canonical types, we can
  // just keep a SmallPtrSet of analyzed Types in TypeChecker.
  InTy->getCanonicalType();
  
  // FIXME: This isn't good enough: top-level stuff can have these as well and
  // their types need to be resolved at the end of name binding.  Perhaps we
  // should require them to have explicit types even if they have values and 
  // let the value mismatch be detected at typechecking time? 
  return false;
}

