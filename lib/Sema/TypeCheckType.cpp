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
#include "swift/AST/ASTWalker.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

/// Given an array slice type with a valid base type and no
/// implementation type, find its canonicalization.
static bool buildArraySliceType(TypeChecker &TC, ArraySliceType *sliceTy) {
  Type baseTy = sliceTy->getBaseType();

  SourceLoc loc = sliceTy->getFirstRefLoc();

  // As our current hack for array slices, require the base type to be
  // a nominal type X, then look for a type SliceX.
  Identifier name;
  if (OneOfType *oneof = baseTy->getAs<OneOfType>()) {
    name = oneof->getDecl()->getName();
  } else {
    TC.diagnose(loc, diag::base_of_array_slice_not_nominal, baseTy);
    return true;
  }

  llvm::SmallString<32> nameBuffer("Slice");
  nameBuffer.append(name.str());

  SmallVector<ValueDecl*, 8> Decls;
  TC.TU.lookupGlobalValue(TC.Context.getIdentifier(nameBuffer),
                          NLKind::UnqualifiedLookup, Decls);
  if (Decls.size() != 1 || !isa<TypeAliasDecl>(Decls.back())) {
    TC.diagnose(loc, diag::slice_type_not_found, nameBuffer);
    return true;
  }

  // FIXME: there's no reason to think that the alias type will have
  // been validated in general.
  TypeAliasDecl *TAD = cast<TypeAliasDecl>(Decls.back());
  sliceTy->setImplementationType(TAD->getAliasType());
  return false;
}

Type TypeChecker::getArraySliceType(SourceLoc loc, Type elementType) {
  ArraySliceType *sliceTy = ArraySliceType::get(elementType, loc, Context);
  if (sliceTy->hasCanonicalTypeComputed()) return sliceTy;
  if (buildArraySliceType(*this, sliceTy)) return Type();
  sliceTy->getCanonicalType();
  return sliceTy;
}

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
  
  switch (T->getKind()) {
  case TypeKind::Error:
    // Error already diagnosed.
    return true;
  case TypeKind::BuiltinFloat:
  case TypeKind::BuiltinInteger:
  case TypeKind::BuiltinRawPointer:
  case TypeKind::BuiltinObjectPointer:
  case TypeKind::UnstructuredDependent:
    // These types are already canonical anyway.
    return false;
  case TypeKind::OneOf:
    for (OneOfElementDecl *Elt : cast<OneOfType>(T)->getElements())
      typeCheckDecl(Elt);
    break;
      
  case TypeKind::MetaType:
  case TypeKind::Module:
    // Nothing to validate.
    break;

  case TypeKind::NameAlias: {
    TypeAliasDecl *D = cast<NameAliasType>(T)->getDecl();
    IsInvalid = !D->hasUnderlyingType() || validateType(D->getUnderlyingType());
    if (IsInvalid)
      D->overwriteUnderlyingType(ErrorType::get(Context));
    break;
  }
  case TypeKind::Identifier:
    return validateType(cast<IdentifierType>(T)->getMappedType());
  case TypeKind::Paren:
    return validateType(cast<ParenType>(T)->getUnderlyingType());
  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->getFields()[i].getType();
      if (EltTy && validateType(EltTy)) {
        IsInvalid = true;
        break;
      }

      Expr *EltInit = TT->getFields()[i].getInit();
      if (EltInit == 0) continue;
      
      Expr *OldInit = EltInit;
      if (typeCheckExpression(EltInit, EltTy)) {
        diagnose(OldInit->getLoc(),diag::while_converting_default_tuple_value,
                 EltTy);
        IsInvalid = true;
        break;
      }

      if (!EltTy)
        EltInit = convertToMaterializable(EltInit);

      struct CheckForLocalRef : public ASTWalker {
        TypeChecker &TC;

        CheckForLocalRef(TypeChecker &TC) : TC(TC) {}

        bool walkToExprPre(Expr *E) {
          if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
            if (DRE->getDecl()->getDeclContext()->isLocalContext()) {
              TC.diagnose(E->getLoc(), diag::tuple_default_local_ref);
            }
          }
          return true;
        }
      };

      EltInit->walk(CheckForLocalRef(*this));

      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      assert(EltTy.isNull() || EltTy->isEqual(EltInit->getType()));
      EltTy = EltInit->getType();

      TT->updateInitializedElementType(i, EltTy, EltInit);
    }
    break;
  }

  case TypeKind::LValue:
    // FIXME: diagnose non-materializability of object type!
    IsInvalid = validateType(cast<LValueType>(T)->getObjectType());
    break;
      
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(T);
    IsInvalid = validateType(FT->getInput()) || validateType(FT->getResult());
    // FIXME: diagnose non-materializability of result type!
    break;
  }
  case TypeKind::Array: {
    ArrayType *AT = cast<ArrayType>(T);
    IsInvalid = validateType(AT->getBaseType());
    // FIXME: diagnose non-materializability of element type!
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }
  case TypeKind::ArraySlice: {
    ArraySliceType *AT = cast<ArraySliceType>(T);
    IsInvalid = validateType(AT->getBaseType());
    // FIXME: diagnose non-materializability of element type?
    if (!IsInvalid)
      IsInvalid = buildArraySliceType(*this, AT);
    break;
  }
      
  case TypeKind::Protocol: {
    ProtocolType *PT = cast<ProtocolType>(T);
    for (auto *member : PT->Elements)
      typeCheckDecl(member);
    break;
  }

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
