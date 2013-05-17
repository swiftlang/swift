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
#include "swift/SIL/SILConstant.h"
#include "swift/SIL/SILType.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
using namespace swift;


static unsigned getNaturalUncurryLevel(CapturingExpr *func) {
  assert(func && "no function body?!");
  assert(func->getParamPatterns().size() >= 1 && "no arguments for func?!");
  unsigned level = func->getParamPatterns().size() - 1;
  // Functions with captures have an extra uncurry level for the capture
  // context.
  if (!func->getCaptures().empty())
    level += 1;
  return level;
}

static unsigned getNaturalUncurryLevel(FuncDecl *fd) {
  if (fd->getBody())
    return getNaturalUncurryLevel(fd->getBody());
  // Assume func decls without bodies (e.g., builtins) have uncurry level zero.
  return 0;
}

SILConstant::SILConstant(ValueDecl *vd, SILConstant::Kind kind,
                         unsigned atUncurryLevel,
                         bool isObjC)
  : loc(vd), kind(kind), isObjC(isObjC)
{
  unsigned naturalUncurryLevel;
  
  if (auto *func = dyn_cast<FuncDecl>(vd)) {
    assert(!func->isGetterOrSetter() &&
           "cannot create a Func SILConstant for a property accessor");
    assert(kind == Kind::Func &&
           "can only create a Func SILConstant for a func decl");
    naturalUncurryLevel = getNaturalUncurryLevel(func);
  } else if (isa<ConstructorDecl>(vd)) {
    assert((kind == Kind::Allocator || kind == Kind::Initializer)
           && "can only create Allocator or Initializer SILConstant for ctor");
    naturalUncurryLevel = 1;
  } else if (auto *ed = dyn_cast<OneOfElementDecl>(vd)) {
    assert(kind == Kind::OneOfElement
           && "can only create OneOfElement SILConstant for oneof element");
    naturalUncurryLevel = ed->hasArgumentType() ? 1 : 0;
  } else if (isa<ClassDecl>(vd)) {
    assert(kind == Kind::Destroyer
           && "can only create Destructor SILConstant for class");
    naturalUncurryLevel = 0;
  } else if (auto *var = dyn_cast<VarDecl>(vd)) {
    assert((kind == Kind::Getter
            || kind == Kind::Setter
            || kind == Kind::GlobalAccessor)
           && "can only create Getter, Setter, GlobalAccessor, or GlobalAddress "
              "SILConstant for var");
    
    bool isGlobal = kind == Kind::GlobalAccessor;
    
    assert(!(isGlobal && var->isProperty())
           && "can't reference property as global var");
    assert(!(isGlobal && var->getDeclContext()->isLocalContext())
           && "can't reference local var as global var");
    
    if (isGlobal) {
      naturalUncurryLevel = 0;
    } else {
      // Instance properties have a 'this' curry.
      if (var->isInstanceMember())
        naturalUncurryLevel = 1;
      // Local properties may have captures that affect the natural uncurry
      // level.
      else if (kind == Kind::Getter && var->getGetter())
        naturalUncurryLevel = getNaturalUncurryLevel(var->getGetter());
      else if (kind == Kind::Setter && var->getSetter())
        naturalUncurryLevel = getNaturalUncurryLevel(var->getSetter());
      // A property accessor for a non-instance variable without getters and
      // setters must be a resilient property, so it can't have context.
      else
        naturalUncurryLevel = 0;
    }
    
  } else if (isa<SubscriptDecl>(vd)) {
    assert((kind == Kind::Getter || kind == Kind::Setter)
           && "can only create Getter or Setter SILConstant for subscript");
    // Subscript accessors have
    //   getter type (T)(Index)() -> U and
    //   setter type (T)(Index)(U) -> ()
    naturalUncurryLevel = 2;
  }
  
  assert((atUncurryLevel == ConstructAtNaturalUncurryLevel
          || atUncurryLevel <= naturalUncurryLevel)
         && "can't emit SILConstant below natural uncurry level");
  uncurryLevel = atUncurryLevel == ConstructAtNaturalUncurryLevel
    ? naturalUncurryLevel
    : atUncurryLevel;
}

SILConstant::SILConstant(SILConstant::Loc baseLoc,
                         unsigned atUncurryLevel,
                         bool asObjC) {
  unsigned naturalUncurryLevel;
  if (ValueDecl *vd = baseLoc.dyn_cast<ValueDecl*>()) {
    if (FuncDecl *fd = dyn_cast<FuncDecl>(vd)) {
      // Map getter or setter FuncDecls to Getter or Setter SILConstants of the
      // property.
      if (fd->isGetterOrSetter()) {
        ValueDecl *property = cast<ValueDecl>(fd->getGetterOrSetterDecl());
        loc = property;
        if (fd->getGetterDecl()) {
          kind = Kind::Getter;
        } else if (fd->getSetterDecl()) {
          kind = Kind::Setter;
        } else {
          llvm_unreachable("no getter or setter decl?!");
        }
      }
      // Map other FuncDecls directly to Func SILConstants.
      else {
        loc = fd;
        kind = Kind::Func;
      }
      naturalUncurryLevel = getNaturalUncurryLevel(fd);
    }
    // Map DestructorDecls to Destroyer SILConstants of the ClassDecl.
    else if (DestructorDecl *dd = dyn_cast<DestructorDecl>(vd)) {
      ClassDecl *cd = cast<ClassDecl>(dd->getParent());
      loc = cd;
      kind = Kind::Destroyer;
      naturalUncurryLevel = 0;
    }
    // Map ConstructorDecls to the Allocator SILConstant of the constructor.
    else if (ConstructorDecl *cd = dyn_cast<ConstructorDecl>(vd)) {
      loc = cd;
      kind = Kind::Allocator;
      naturalUncurryLevel = 1;
    }
    // Map OneOfElementDecls to the OneOfElement SILConstant of the element.
    else if (OneOfElementDecl *ed = dyn_cast<OneOfElementDecl>(vd)) {
      loc = ed;
      kind = Kind::OneOfElement;
      naturalUncurryLevel = ed->hasArgumentType() ? 1 : 0;
    }
    // VarDecl constants require an explicit kind.
    else if (isa<VarDecl>(vd)) {
      llvm_unreachable("must create SILConstant for VarDecl with explicit kind");
    }
    else {
      llvm_unreachable("invalid loc decl for SILConstant!");
    }
  } else {
    auto *expr = baseLoc.get<CapturingExpr*>();
    loc = expr;
    kind = Kind::Func;
    assert(expr->getParamPatterns().size() >= 1
           && "no param patterns for function?!");
    naturalUncurryLevel = getNaturalUncurryLevel(expr);
  }
  
  // Set the uncurry level.
  assert((atUncurryLevel == ConstructAtNaturalUncurryLevel
          || atUncurryLevel <= naturalUncurryLevel)
         && "can't emit SILConstant below natural uncurry level");
  uncurryLevel = atUncurryLevel == ConstructAtNaturalUncurryLevel
    ? naturalUncurryLevel
    : atUncurryLevel;
    
  isObjC = asObjC;
}

SILType SILType::getObjectPointerType(ASTContext &C) {
  return SILType(CanType(C.TheObjectPointerType),
                 /*isAddress=*/ false,
                 /*isLoadable=*/ true);
}

SILType SILType::getRawPointerType(ASTContext &C) {
  return SILType(CanType(C.TheRawPointerType),
                 /*isAddress=*/false,
                 /*isLoadable=*/true);
}

SILType SILType::getOpaquePointerType(ASTContext &C) {
  return SILType(CanType(C.TheOpaquePointerType),
                 /*isAddress=*/false,
                 /*isLoadable=*/true);
}

SILType SILType::getBuiltinIntegerType(unsigned bitWidth, ASTContext &C) {
  return SILType(CanType(BuiltinIntegerType::get(bitWidth, C)),
                 /*isAddress=*/false,
                 /*isLoadable=*/true);
}

SILType SILType::getBuiltinFloatType(BuiltinFloatType::FPKind Kind,
                                     ASTContext &C) {
  Type ty;
  switch (Kind) {
  case BuiltinFloatType::IEEE16:
    ty = C.TheIEEE16Type;
    break;
  case BuiltinFloatType::IEEE32:
    ty = C.TheIEEE32Type;
    break;
  case BuiltinFloatType::IEEE64:
    ty = C.TheIEEE64Type;
    break;
  case BuiltinFloatType::IEEE80:
    ty = C.TheIEEE80Type;
    break;
  case BuiltinFloatType::IEEE128:
    ty = C.TheIEEE128Type;
    break;
  case BuiltinFloatType::PPC128:
    ty = C.ThePPC128Type;
    break;
  }
  return SILType(CanType(ty),
                 /*isAddress=*/false,
                 /*isLoadable=*/true);
}
