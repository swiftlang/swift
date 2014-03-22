//===--- SILDeclRef.cpp - Implements SILDeclRef ---------------------------===//
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

#include "swift/SIL/SILDeclRef.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Mangle.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/SILLinkage.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
using namespace swift;

static unsigned getFuncNaturalUncurryLevel(AnyFunctionRef AFR) {
  assert(AFR.getBodyParamPatterns().size() >= 1 && "no arguments for func?!");
  unsigned Level = AFR.getBodyParamPatterns().size() - 1;
  // Functions with captures have an extra uncurry level for the capture
  // context.
  if (AFR.getCaptureInfo().hasLocalCaptures())
    Level += 1;
  return Level;
}

SILDeclRef::SILDeclRef(ValueDecl *vd, SILDeclRef::Kind kind,
                       ResilienceExpansion expansion,
                       unsigned atUncurryLevel, bool isForeign)
  : loc(vd), kind(kind), Expansion(unsigned(expansion)),
    isForeign(isForeign), defaultArgIndex(0)
{
  unsigned naturalUncurryLevel;

  // FIXME: restructure to use a "switch".

  if (auto *func = dyn_cast<FuncDecl>(vd)) {
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
  } else if (isa<ClassDecl>(vd)) {
    assert((kind == Kind::IVarInitializer || kind == Kind::IVarDestroyer) &&
           "can only create ivar initializer/destroyer SILDeclRef for class");
    naturalUncurryLevel = 1;
  } else if (auto *var = dyn_cast<VarDecl>(vd)) {
    assert(kind == Kind::GlobalAccessor &&
           "can only create GlobalAccessor SILDeclRef for var");

    naturalUncurryLevel = 0;
    assert(!var->getDeclContext()->isLocalContext() &&
           "can't reference local var as global var");
    assert(var->hasStorage() && "can't reference computed var as global var");
    (void)var;
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
                       ResilienceExpansion expansion,
                       unsigned atUncurryLevel, bool asForeign) 
 : defaultArgIndex(0)
{
  unsigned naturalUncurryLevel;
  if (ValueDecl *vd = baseLoc.dyn_cast<ValueDecl*>()) {
    if (FuncDecl *fd = dyn_cast<FuncDecl>(vd)) {
      // Map FuncDecls directly to Func SILDeclRefs.
      loc = fd;
      kind = Kind::Func;
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
    // Map DestructorDecls to the Deallocator of the destructor.
    else if (auto dtor = dyn_cast<DestructorDecl>(vd)) {
      loc = dtor;
      kind = Kind::Deallocator;
      naturalUncurryLevel = 0;
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
  Expansion = (unsigned) expansion;
  
  isCurried = uncurryLevel != naturalUncurryLevel;  
  isForeign = asForeign;
}

static SILLinkage getLinkageForLocalContext(DeclContext *dc) {
  while (!dc->isModuleScopeContext()) {
    // Local definitions in transparent contexts are forced public because
    // external references to them can be exposed by mandatory inlining.
    if (auto fn = dyn_cast<AbstractFunctionDecl>(dc)) {
      if (fn->isTransparent())
        return SILLinkage::Public;
    }
    // Check that this local context is not itself in a local transparent
    // context.
    dc = dc->getParent();
  }
  
  return SILLinkage::Private;
}

SILLinkage SILDeclRef::getLinkage(ForDefinition_t forDefinition) const {
  // Anonymous functions have local linkage.
  if (auto closure = getAbstractClosureExpr())
    return getLinkageForLocalContext(closure->getParent());
  
  // Function-local declarations always have internal linkage.
  ValueDecl *d = getDecl();
  DeclContext *dc = d->getDeclContext();
  while (!dc->isModuleScopeContext()) {
    if (dc->isLocalContext())
      return getLinkageForLocalContext(dc);
    dc = dc->getParent();
  }
  
  // Currying and calling convention thunks have shared linkage.
  if (isCurried || isForeignThunk())
    return SILLinkage::Shared;
  
  // Declarations imported from Clang modules have shared linkage.
  // FIXME: They shouldn't.
  const SILLinkage ClangLinkage = SILLinkage::Shared;
  
  if (isa<ClangModuleUnit>(dc)) {
    if (isa<ConstructorDecl>(d) || isa<EnumElementDecl>(d))
      return ClangLinkage;

    if (auto *FD = dyn_cast<FuncDecl>(d))
      if (FD->isGetterOrSetter() || isa<EnumDecl>(d->getDeclContext()) ||
          isa<StructDecl>(d->getDeclContext()))
        return ClangLinkage;
  }
  // Declarations that were derived on behalf of types in Clang modules get
  // shared linkage.
  if (auto *FD = dyn_cast<FuncDecl>(d)) {
    if (auto derivedFor = FD->getDerivedForTypeDecl())
      if (isa<ClangModuleUnit>(derivedFor->getModuleScopeContext()))
        return ClangLinkage;
  }
  
  // Otherwise, we have external linkage.
  // FIXME: access control
  return (forDefinition ? SILLinkage::Public : SILLinkage::PublicExternal);
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

  return hasDecl() ? getDecl()->isTransparent() : false;
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
                                  c.getResilienceExpansion(),
                                  c.uncurryLevel);
      return;
    }

    // As a special case, functions can have external asm names.
    // Use the asm name only for the original non-thunked, non-curried entry
    // point.
    if (auto AsmA = c.getDecl()->getAttrs().getAttribute<AsmnameAttr>())
      if (!c.isForeignThunk() && !c.isCurried) {
        buffer << AsmA->Name;
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
    mangler.mangleEntity(c.getDecl(), c.getResilienceExpansion(), c.uncurryLevel);
    return;
      
  //   entity ::= context 'D'                     // deallocating destructor
  case SILDeclRef::Kind::Deallocator:
    buffer << introducer;
    mangler.mangleDestructorEntity(cast<DestructorDecl>(c.getDecl()),
                                   /*isDeallocating*/ true);
    return;

  //   entity ::= context 'd'                     // destroying destructor
  case SILDeclRef::Kind::Destroyer:
    buffer << introducer;
    mangler.mangleDestructorEntity(cast<DestructorDecl>(c.getDecl()),
                                   /*isDeallocating*/ false);
    return;

  //   entity ::= context 'C' type                // allocating constructor
  case SILDeclRef::Kind::Allocator:
    buffer << introducer;
    mangler.mangleConstructorEntity(cast<ConstructorDecl>(c.getDecl()),
                                    /*allocating*/ true,
                                    c.getResilienceExpansion(),
                                    c.uncurryLevel);
    return;

  //   entity ::= context 'c' type                // initializing constructor
  case SILDeclRef::Kind::Initializer:
    buffer << introducer;
    mangler.mangleConstructorEntity(cast<ConstructorDecl>(c.getDecl()),
                                    /*allocating*/ false,
                                    c.getResilienceExpansion(),
                                    c.uncurryLevel);
    return;

  //   entity ::= declaration 'e'                 // ivar initializer
  //   entity ::= declaration 'E'                 // ivar destroyer
  case SILDeclRef::Kind::IVarInitializer:
  case SILDeclRef::Kind::IVarDestroyer:
    buffer << introducer;
    mangler.mangleIVarInitDestroyEntity(
      cast<ClassDecl>(c.getDecl()),
      c.kind == SILDeclRef::Kind::IVarDestroyer);
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
