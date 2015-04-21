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
#include "swift/SIL/SILLocation.h"
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
    isForeign(isForeign), isDirectReference(0), defaultArgIndex(0)
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
    assert((kind == Kind::GlobalAccessor || kind == Kind::GlobalGetter) &&
           "can only create GlobalAccessor or GlobalGetter SILDeclRef for var");

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
 : isDirectReference(0), defaultArgIndex(0)
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
    if (auto fn = dyn_cast<AbstractFunctionDecl>(dc))
      if (fn->isTransparent())
        return SILLinkage::Public;
    // Check that this local context is not itself in a local transparent
    // context.
    dc = dc->getParent();
  }

  // FIXME: Once we have access control at the AST level, we should not assume
  // shared always, but rather base it off of the local decl context.
  return SILLinkage::Shared;
}

SILLinkage SILDeclRef::getLinkage(ForDefinition_t forDefinition) const {
  // Anonymous functions have local linkage.
  if (auto closure = getAbstractClosureExpr())
    return getLinkageForLocalContext(closure->getParent());
  
  // Native function-local declarations have local linkage.
  // FIXME: @objc declarations should be too, but we currently have no way
  // of marking them "used" other than making them external. 
  ValueDecl *d = getDecl();
  DeclContext *moduleContext = d->getDeclContext();
  while (!moduleContext->isModuleScopeContext()) {
    if (!isForeign && moduleContext->isLocalContext())
      return getLinkageForLocalContext(moduleContext);
    moduleContext = moduleContext->getParent();
  }
  
  // Currying and calling convention thunks have shared linkage.
  if (isCurried || isForeignToNativeThunk() || isNativeToForeignThunk())
    return SILLinkage::Shared;
  
  // Declarations imported from Clang modules have shared linkage.
  // FIXME: They shouldn't.
  const SILLinkage ClangLinkage = SILLinkage::Shared;
  
  if (isa<ClangModuleUnit>(moduleContext)) {
    if (isa<ConstructorDecl>(d) || isa<EnumElementDecl>(d))
      return ClangLinkage;

    if (auto *FD = dyn_cast<FuncDecl>(d))
      if (FD->isAccessor() ||
          isa<NominalTypeDecl>(d->getDeclContext()))
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
  switch (d->getEffectiveAccess()) {
    case Accessibility::Private:
      return (forDefinition ? SILLinkage::Private : SILLinkage::PrivateExternal);

    case Accessibility::Internal:
      return (forDefinition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);

    default:
      return (forDefinition ? SILLinkage::Public : SILLinkage::PublicExternal);
  }
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

  if (hasAutoClosureExpr())
    return true;

  return hasDecl() ? getDecl()->isTransparent() : false;
}

/// \brief True if the function has noinline attribute.
bool SILDeclRef::isNoinline() const {
  if (!hasDecl())
    return false;
  if (auto InlineA = getDecl()->getAttrs().getAttribute<InlineAttr>())
    if (InlineA->getKind() == InlineKind::Never)
      return true;
   return false;
}

/// \brief True if the function has noinline attribute.
bool SILDeclRef::isAlwaysInline() const {
  if (!hasDecl())
    return false;
  if (auto InlineA = getDecl()->getAttrs().getAttribute<InlineAttr>())
    if (InlineA->getKind() == InlineKind::Always)
      return true;
  return false;
}

bool SILDeclRef::hasEffectsAttribute() const {
  if (!hasDecl())
    return false;
  return getDecl()->getAttrs().hasAttribute<EffectsAttr>();
}

EffectsKind SILDeclRef::getEffectsAttribute() const {
  assert(hasEffectsAttribute());
  EffectsAttr *MA = getDecl()->getAttrs().getAttribute<EffectsAttr>();
  return MA->getKind();
}

bool SILDeclRef::isForeignToNativeThunk() const {
  // Non-decl entry points are never natively foreign, so they would never
  // have a foreign-to-native thunk.
  if (!hasDecl())
    return false;
  // Otherwise, match whether we have a clang node with whether we're foreign.
  if (isa<FuncDecl>(getDecl()) && getDecl()->hasClangNode())
    return !isForeign;
  // ObjC initializing constructors and factories are foreign.
  // We emit a special native allocating constructor though.
  if (isa<ConstructorDecl>(getDecl())
      && (kind == Kind::Initializer
          || cast<ConstructorDecl>(getDecl())->isFactoryInit())
      && getDecl()->hasClangNode())
    return !isForeign;
  return false;
}

bool SILDeclRef::isNativeToForeignThunk() const {
  // We can have native-to-foreign thunks over closures.
  if (!hasDecl())
    return isForeign;
  // We can have native-to-foreign thunks over global or local native functions.
  // TODO: Static functions too.
  if (auto func = dyn_cast<FuncDecl>(getDecl())) {
    if (!func->getDeclContext()->isTypeContext()
        && !func->hasClangNode())
      return isForeign;
  }
  return false;
}

static void mangleConstant(SILDeclRef c, llvm::raw_ostream &buffer,
                           StringRef prefix) {
  using namespace Mangle;
  Mangler mangler(buffer);

  // Almost everything below gets one of the common prefixes:
  //   mangled-name ::= '_T' global     // Native symbol
  //   mangled-name ::= '_TTo' global   // ObjC interop thunk
  //   mangled-name ::= '_TTO' global   // Foreign function thunk
  //   mangled-name ::= '_TTd' global   // Direct
  StringRef introducer = "_T";
  if (!prefix.empty()) {
    introducer = prefix;
  } else if (c.isForeign) {
    assert(prefix.empty() && "can't have custom prefix on thunk");
    introducer = "_TTo";
  } else if (c.isDirectReference) {
    introducer = "_TTd";
  } else if (c.isForeignToNativeThunk()) {
    assert(prefix.empty() && "can't have custom prefix on thunk");
    introducer = "_TTO";
  }
  
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
      if (!c.isForeignToNativeThunk() && !c.isNativeToForeignThunk()
          && !c.isCurried) {
        buffer << AsmA->Name;
        return;
      }

    // Otherwise, fall through into the 'other decl' case.
    SWIFT_FALLTHROUGH;

  case SILDeclRef::Kind::EnumElement:
    // As a special case, Clang functions and globals don't get mangled at all.
    // FIXME: When we can import C++, use Clang's mangler.
    if (auto clangDecl = c.getDecl()->getClangDecl()) {
      if (!c.isForeignToNativeThunk() && !c.isNativeToForeignThunk()
          && !c.isCurried) {
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

  //   entity ::= declaration 'G'                 // getter
  case SILDeclRef::Kind::GlobalGetter:
    buffer << introducer;
    mangler.mangleGlobalGetterEntity(c.getDecl());
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

StringRef SILDeclRef::mangle(SmallVectorImpl<char> &buffer,
                             StringRef prefix) const {
  assert(buffer.empty());
  llvm::raw_svector_ostream stream(buffer);
  mangleConstant(*this, stream, prefix);
  return stream.str();
}

SILDeclRef SILDeclRef::getOverriddenVTableEntry() const {
  if (auto overridden = getOverridden()) {
    // If we overrode a foreign decl, a dynamic method, this is an
    // accessor for a property that overrides an ObjC decl, or if it is an
    // @NSManaged property, then it won't be in the vtable.
    if (overridden.getDecl()->hasClangNode())
      return SILDeclRef();
    if (overridden.getDecl()->getAttrs().hasAttribute<DynamicAttr>())
      return SILDeclRef();
    if (auto *ovFD = dyn_cast<FuncDecl>(overridden.getDecl()))
      if (auto *asd = ovFD->getAccessorStorageDecl()) {
        if (asd->hasClangNode())
          return SILDeclRef();
      }

    // If we overrode a decl from an extension, it won't be in a vtable
    // either. This can occur for extensions to ObjC classes.
    if (isa<ExtensionDecl>(overridden.getDecl()->getDeclContext()))
      return SILDeclRef();

    // If we overrode a non-required initializer, there won't be a vtable
    // slot for the allocator.
    if (overridden.kind == SILDeclRef::Kind::Allocator &&
        !cast<ConstructorDecl>(overridden.getDecl())->isRequired()) {
      return SILDeclRef();
    }

    return overridden;
  }
  return SILDeclRef();
}

SILLocation SILDeclRef::getAsRegularLocation() const {
  if (hasDecl())
    return RegularLocation(getDecl());
  return RegularLocation(getAbstractClosureExpr());
}
