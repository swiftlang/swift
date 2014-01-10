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

#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILUndef.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Pattern.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
using namespace swift;

void ValueBase::replaceAllUsesWith(ValueBase *RHS) {
  assert(this != RHS && "Cannot RAUW a value with itself");
  assert(getNumTypes() == RHS->getNumTypes() &&
         "An instruction and the value base that it is being replaced by "
         "must have the same number of types");

  while (!use_empty()) {
    Operand *Op = *use_begin();
    Op->set(SILValue(RHS, Op->get().getResultNumber()));
  }
}

void SILValue::replaceAllUsesWith(SILValue V) {
  assert(*this != V && "Cannot RAUW a value with itself");
  assert(getType() == V.getType() && "Invalid type");
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
           "can only create a Func SILDeclRef for a func decl");
    naturalUncurryLevel = getFuncNaturalUncurryLevel(func);
  } else if (isa<ConstructorDecl>(vd)) {
    assert((kind == Kind::Allocator || kind == Kind::Initializer)
           && "can only create Allocator or Initializer SILDeclRef for ctor");
    naturalUncurryLevel = 1;
  } else if (auto *ed = dyn_cast<EnumElementDecl>(vd)) {
    assert(kind == Kind::EnumElement
           && "can only create EnumElement SILDeclRef for enum element");
    naturalUncurryLevel = ed->hasArgumentType() ? 1 : 0;
  } else if (isa<DestructorDecl>(vd)) {
    assert((kind == Kind::Destroyer || kind == Kind::Deallocator)
           && "can only create destroyer/deallocator SILDeclRef for dtor");
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

SILDeclRef::SILDeclRef(SILDeclRef::Loc baseLoc, unsigned atUncurryLevel,
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
    // DestructorDecl constants require an explicit kind.
    else if (isa<DestructorDecl>(vd)) {
      llvm_unreachable("must create SILDeclRef for DestructorDecl with kind");
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

static void mangleConstant(SILDeclRef c, llvm::raw_ostream &buffer) {
  using namespace Mangle;
  Mangler mangler(buffer);

  // Almost everything below gets one of the common prefixes:
  //   mangled-name ::= '_T' global     // Native symbol
  //   mangled-name ::= '_TTo' global   // ObjC interop thunk
  //   mangled-name ::= '_TTO' global   // Foreign function thunk
  StringRef introducer = "_T";
  if (c.isForeign)
    introducer = "_TTo";
  else if (c.isForeignThunk())
    introducer = "_TTO";
  
  switch (c.kind) {
  //   entity ::= declaration                     // other declaration
  case SILDeclRef::Kind::Func:
    if (!c.hasDecl()) {
      buffer << introducer;
      mangler.mangleClosureEntity(c.getAbstractClosureExpr(),
                                  ExplosionKind::Minimal,
                                  c.uncurryLevel);
      return;
    }

    // As a special case, functions can have external asm names.
    // Use the asm name only for the original non-thunked, non-curried entry
    // point.
    if (!c.getDecl()->getAttrs().AsmName.empty()
        && !c.isForeignThunk() && !c.isCurried) {
      buffer << c.getDecl()->getAttrs().AsmName;
      return;
    }

    // Otherwise, fall through into the 'other decl' case.
    SWIFT_FALLTHROUGH;

  case SILDeclRef::Kind::EnumElement:
    // As a special case, Clang functions and globals don't get mangled at all.
    // FIXME: When we can import C++, use Clang's mangler.
    if (auto clangDecl = c.getDecl()->getClangDecl()) {
      if (!c.isForeignThunk() && !c.isCurried) {
        if (auto namedClangDecl = dyn_cast<clang::DeclaratorDecl>(clangDecl)) {
          if (auto asmLabel = namedClangDecl->getAttr<clang::AsmLabelAttr>()) {
            buffer << '\01' << asmLabel->getLabel();
          } else {
            buffer << namedClangDecl->getName();
          }
          return;
        }
      }
    }

    buffer << introducer;
    mangler.mangleEntity(c.getDecl(), ExplosionKind::Minimal, c.uncurryLevel);
    return;
      
  //   entity ::= context 'D'                     // deallocating destructor
  case SILDeclRef::Kind::Deallocator:
    buffer << introducer;
    mangler.mangleDestructorEntity(cast<ClassDecl>(
                                     c.getDecl()->getDeclContext()),
                                   /*isDeallocating*/ true);
    return;

  //   entity ::= context 'd'                     // destroying destructor
  case SILDeclRef::Kind::Destroyer:
    buffer << introducer;
    mangler.mangleDestructorEntity(cast<ClassDecl>(
                                     c.getDecl()->getDeclContext()),
                                   /*isDeallocating*/ false);
    return;

  //   entity ::= context 'C' type                // allocating constructor
  case SILDeclRef::Kind::Allocator:
    buffer << introducer;
    mangler.mangleConstructorEntity(cast<ConstructorDecl>(c.getDecl()),
                                    /*allocating*/ true,
                                    ExplosionKind::Minimal,
                                    c.uncurryLevel);
    return;

  //   entity ::= context 'c' type                // initializing constructor
  case SILDeclRef::Kind::Initializer:
    buffer << introducer;
    mangler.mangleConstructorEntity(cast<ConstructorDecl>(c.getDecl()),
                                    /*allocating*/ false,
                                    ExplosionKind::Minimal,
                                    c.uncurryLevel);
    return;

  //   entity ::= declaration 'g'                 // getter
  case SILDeclRef::Kind::Getter:
    buffer << introducer;
    mangler.mangleGetterEntity(c.getDecl(), ExplosionKind::Minimal);
    return;

  //   entity ::= declaration 's'                 // setter
  case SILDeclRef::Kind::Setter:
    buffer << introducer;
    mangler.mangleSetterEntity(c.getDecl(), ExplosionKind::Minimal);
    return;

  //   entity ::= declaration 'a'                 // addressor
  case SILDeclRef::Kind::GlobalAccessor:
    buffer << introducer;
    mangler.mangleAddressorEntity(c.getDecl());
    return;

  //   entity ::= context 'e' index           // default arg generator
  case SILDeclRef::Kind::DefaultArgGenerator:
    buffer << introducer;
    mangler.mangleDefaultArgumentEntity(cast<AbstractFunctionDecl>(c.getDecl()),
                                        c.defaultArgIndex);
    return;
  }

  llvm_unreachable("bad entity kind!");
}

StringRef SILDeclRef::mangle(SmallVectorImpl<char> &buffer) const {
  assert(buffer.empty());
  llvm::raw_svector_ostream stream(buffer);
  mangleConstant(*this, stream);
  return stream.str();
}

static FormalLinkage getGenericClauseLinkage(ArrayRef<GenericParam> params) {
  FormalLinkage result = FormalLinkage::Top;
  for (auto &param : params) {
    for (auto proto : param.getAsTypeParam()->getProtocols())
      result ^= getTypeLinkage(CanType(proto->getDeclaredType()));
    if (auto superclass = param.getAsTypeParam()->getSuperclass())
      result ^= getTypeLinkage(superclass->getCanonicalType());
  }
  return result;
}

FormalLinkage swift::getDeclLinkage(Decl *D) {
  DeclContext *DC = D->getDeclContext();
  while (!DC->isModuleScopeContext()) {
    if (DC->isLocalContext())
      return FormalLinkage::Private;
    DC = DC->getParent();
  }

  // Clang declarations are public and can't be assured of having a
  // unique defining location.
  if (isa<ClangModuleUnit>(DC))
    return FormalLinkage::PublicNonUnique;

  // TODO: access control
  return FormalLinkage::PublicUnique;
}

FormalLinkage swift::getTypeLinkage(CanType type) {
  FormalLinkage result = FormalLinkage::Top;

  // Merge all nominal types from the structural type.
  (void) type.findIf([&](Type _type) {
    CanType type = CanType(_type);

    // For any nominal type reference, look at the type declaration.
    if (auto nominal = type->getAnyNominal()) {
      result ^= getDeclLinkage(nominal);

    // For polymorphic function types, look at the generic parameters.
    // FIXME: findIf should do this, once polymorphic function types can be
    // canonicalized and re-formed properly.
    } else if (auto polyFn = dyn_cast<PolymorphicFunctionType>(type)) {
      result ^= getGenericClauseLinkage(polyFn->getGenericParameters());
    }

    return false; // continue searching
  });

  return result;
}

FormalLinkage swift::getConformanceLinkage(const ProtocolConformance *conf) {
  // FIXME
  return FormalLinkage::PublicUnique;
}

