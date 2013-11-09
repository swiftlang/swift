//===--- SIL.cpp - Implements random SIL functionality --------------------===//
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

#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILUndef.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
using namespace swift;


void SILValue::replaceAllUsesWith(SILValue V) {
  assert(*this != V && "Cannot RAUW a value with itself");
  while (!use_empty())
    (**use_begin()).set(V);
}

SILUndef *SILUndef::get(SILType Ty, SILModule *M) {
  // Unique these.
  SILUndef *&Entry = M->UndefValues[Ty];
  if (Entry == nullptr)
    Entry = new (*M) SILUndef(Ty);
  return Entry;
}



static unsigned getFuncNaturalUncurryLevel(AnyFunctionRef AFR) {
  assert(AFR.getArgParamPatterns().size() >= 1 && "no arguments for func?!");
  unsigned Level = AFR.getArgParamPatterns().size() - 1;
  // Functions with captures have an extra uncurry level for the capture
  // context.
  if (AFR.getCaptureInfo().hasLocalCaptures())
    Level += 1;
  return Level;
}

SILDeclRef::SILDeclRef(ValueDecl *vd, SILDeclRef::Kind kind,
                         unsigned atUncurryLevel,
                         bool isForeign)
  : loc(vd), kind(kind), isForeign(isForeign), defaultArgIndex(0)
{
  unsigned naturalUncurryLevel;

  // FIXME: restructure to use a "switch".

  if (auto *func = dyn_cast<FuncDecl>(vd)) {
    assert(!func->isGetterOrSetter() &&
           "cannot create a Func SILDeclRef for a property accessor");
    assert(kind == Kind::Func &&
           "can only create a Func SILDeclRef for a function decl");
    naturalUncurryLevel = getFuncNaturalUncurryLevel(func);
  } else if (isa<ConstructorDecl>(vd)) {
    assert((kind == Kind::Allocator || kind == Kind::Initializer)
           && "can only create Allocator or Initializer SILDeclRef for ctor");
    naturalUncurryLevel = 1;
  } else if (auto *ed = dyn_cast<EnumElementDecl>(vd)) {
    assert(kind == Kind::EnumElement
           && "can only create EnumElement SILDeclRef for enum element");
    naturalUncurryLevel = ed->hasArgumentType() ? 1 : 0;
  } else if (isa<ClassDecl>(vd)) {
    assert(kind == Kind::Destroyer
           && "can only create Destructor SILDeclRef for class");
    naturalUncurryLevel = 0;
  } else if (auto *var = dyn_cast<VarDecl>(vd)) {
    assert((kind == Kind::Getter
            || kind == Kind::Setter
            || kind == Kind::GlobalAccessor)
           && "can only create Getter, Setter, GlobalAccessor, or GlobalAddress "
              "SILDeclRef for var");
    
    bool isGlobal = kind == Kind::GlobalAccessor;
    
    assert(!(isGlobal && var->isComputed())
           && "can't reference computed var as global var");
    assert(!(isGlobal && var->getDeclContext()->isLocalContext())
           && "can't reference local var as global var");
    
    if (isGlobal) {
      naturalUncurryLevel = 0;
    } else {
      // Member computed vars have a 'self' curry.
      // FIXME: What about static vars?
      if (var->isInstanceMember())
        naturalUncurryLevel = 1;
      // Local computed vars may have captures that affect the natural uncurry
      // level.
      else if (kind == Kind::Getter && var->getGetter())
        naturalUncurryLevel = getFuncNaturalUncurryLevel(var->getGetter());
      else if (kind == Kind::Setter && var->getSetter())
        naturalUncurryLevel = getFuncNaturalUncurryLevel(var->getSetter());
      // An accessor for a non-instance variable without getters and
      // setters must be a resilient variable, so it can't have context.
      else
        naturalUncurryLevel = 0;
    }
    
  } else if (isa<SubscriptDecl>(vd)) {
    assert((kind == Kind::Getter || kind == Kind::Setter)
           && "can only create Getter or Setter SILDeclRef for subscript");
    // Subscript accessors have
    //   getter type (T)(Index)() -> U and
    //   setter type (T)(Index)(U) -> ()
    naturalUncurryLevel = 2;
  } else {
    llvm_unreachable("Unhandled ValueDecl for SILDeclRef");
  }
  
  assert((atUncurryLevel == ConstructAtNaturalUncurryLevel
          || atUncurryLevel <= naturalUncurryLevel)
         && "can't emit SILDeclRef below natural uncurry level");
  uncurryLevel = atUncurryLevel == ConstructAtNaturalUncurryLevel
    ? naturalUncurryLevel
    : atUncurryLevel;
  isCurried = uncurryLevel != naturalUncurryLevel;
}

SILDeclRef::SILDeclRef(SILDeclRef::Loc baseLoc,
                         unsigned atUncurryLevel,
                         bool asForeign) 
 : defaultArgIndex(0)
{
  unsigned naturalUncurryLevel;
  if (ValueDecl *vd = baseLoc.dyn_cast<ValueDecl*>()) {
    if (FuncDecl *fd = dyn_cast<FuncDecl>(vd)) {
      // Map getter or setter FuncDecls to Getter or Setter SILDeclRefs of the
      // variable.
      if (fd->isGetterOrSetter()) {
        ValueDecl *variable = cast<ValueDecl>(fd->getGetterOrSetterDecl());
        loc = variable;
        if (fd->getGetterDecl()) {
          kind = Kind::Getter;
        } else if (fd->getSetterDecl()) {
          kind = Kind::Setter;
        } else {
          llvm_unreachable("no getter or setter decl?!");
        }
      }
      // Map other FuncDecls directly to Func SILDeclRefs.
      else {
        loc = fd;
        kind = Kind::Func;
      }
      naturalUncurryLevel = getFuncNaturalUncurryLevel(fd);
    }
    // Map DestructorDecls to Destroyer SILDeclRefs of the ClassDecl.
    else if (DestructorDecl *dd = dyn_cast<DestructorDecl>(vd)) {
      ClassDecl *cd = cast<ClassDecl>(dd->getParent());
      loc = cd;
      kind = Kind::Destroyer;
      naturalUncurryLevel = 0;
    }
    // Map ConstructorDecls to the Allocator SILDeclRef of the constructor.
    else if (ConstructorDecl *cd = dyn_cast<ConstructorDecl>(vd)) {
      loc = cd;
      kind = Kind::Allocator;
      naturalUncurryLevel = 1;

      // FIXME: Should we require the caller to think about this?
      asForeign = false;
    }
    // Map EnumElementDecls to the EnumElement SILDeclRef of the element.
    else if (EnumElementDecl *ed = dyn_cast<EnumElementDecl>(vd)) {
      loc = ed;
      kind = Kind::EnumElement;
      naturalUncurryLevel = ed->hasArgumentType() ? 1 : 0;
    }
    // VarDecl constants require an explicit kind.
    else if (isa<VarDecl>(vd)) {
      llvm_unreachable("must create SILDeclRef for VarDecl with explicit kind");
    }
    else {
      llvm_unreachable("invalid loc decl for SILDeclRef!");
    }
  } else if (auto *ACE = baseLoc.dyn_cast<AbstractClosureExpr *>()) {
    loc = ACE;
    kind = Kind::Func;
    assert(ACE->getParamPatterns().size() >= 1 &&
           "no param patterns for function?!");
    naturalUncurryLevel = getFuncNaturalUncurryLevel(ACE);
  } else {
    llvm_unreachable("impossible SILDeclRef loc");
  }

  // Set the uncurry level.
  assert((atUncurryLevel == ConstructAtNaturalUncurryLevel
          || atUncurryLevel <= naturalUncurryLevel)
         && "can't emit SILDeclRef below natural uncurry level");
  uncurryLevel = atUncurryLevel == ConstructAtNaturalUncurryLevel
    ? naturalUncurryLevel
    : atUncurryLevel;
  
  isCurried = uncurryLevel != naturalUncurryLevel;  
  isForeign = asForeign;
}

SILDeclRef SILDeclRef::getDefaultArgGenerator(Loc loc,
                                              unsigned defaultArgIndex) {
  SILDeclRef result;
  result.loc = loc;
  result.kind = Kind::DefaultArgGenerator;
  result.defaultArgIndex = defaultArgIndex;
  return result;
}

/// \brief True if the function should be treated as transparent.
bool SILDeclRef::isTransparent() const {
  if (isEnumElement())
    return true;

  if (hasDecl()) {
    const ValueDecl *D = getDecl();
    if (isAccessor()) {
      if (kind == Kind::Getter) {
        if (const SubscriptDecl *SD = dyn_cast<SubscriptDecl>(getDecl()))
          D = SD->getGetter();
        else
          D = cast<VarDecl>(getDecl())->getGetter();
      } else if (kind == Kind::Setter) {
        if (const SubscriptDecl *SD = dyn_cast<SubscriptDecl>(getDecl()))
          D = SD->getSetter();
        else
          D = cast<VarDecl>(getDecl())->getSetter();
      } else {
        llvm_unreachable("Accessor is neither a getter nor a setter.");
      }
    }
    
    return D ? D->isTransparent() : false;
  }
  return false;
}

bool SILDeclRef::isForeignThunk() const {
  // Non-decl entry points are never thunks.
  if (!hasDecl())
    return false;
  // Otherwise, match whether we have a clang node with whether we're foreign.
  if (isa<FuncDecl>(getDecl()) && getDecl()->hasClangNode())
    return !isForeign;
  return false;
}

SILType SILType::getObjectPointerType(const ASTContext &C) {
  return SILType(CanType(C.TheObjectPointerType), SILValueCategory::Object);
}

SILType SILType::getObjCPointerType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(C.TheObjCPointerType));
}

SILType SILType::getRawPointerType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(C.TheRawPointerType));
}

SILType SILType::getBuiltinIntegerType(unsigned bitWidth,
                                       const ASTContext &C) {
  return getPrimitiveObjectType(CanType(BuiltinIntegerType::get(bitWidth, C)));
}

SILType SILType::getBuiltinFloatType(BuiltinFloatType::FPKind Kind,
                                     const ASTContext &C) {
  Type ty;
  switch (Kind) {
  case BuiltinFloatType::IEEE16:  ty = C.TheIEEE16Type; break;
  case BuiltinFloatType::IEEE32:  ty = C.TheIEEE32Type; break;
  case BuiltinFloatType::IEEE64:  ty = C.TheIEEE64Type; break;
  case BuiltinFloatType::IEEE80:  ty = C.TheIEEE80Type; break;
  case BuiltinFloatType::IEEE128: ty = C.TheIEEE128Type; break;
  case BuiltinFloatType::PPC128:  ty = C.ThePPC128Type; break;
  }
  return getPrimitiveObjectType(CanType(ty));
}
