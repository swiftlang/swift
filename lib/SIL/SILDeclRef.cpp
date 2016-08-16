//===--- SILDeclRef.cpp - Implements SILDeclRef ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILLocation.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Mangle.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/SILLinkage.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
using namespace swift;

/// Get the method dispatch mechanism for a method.
MethodDispatch
swift::getMethodDispatch(AbstractFunctionDecl *method) {
  // Final methods can be statically referenced.
  if (method->isFinal())
    return MethodDispatch::Static;
  // Some methods are forced to be statically dispatched.
  if (method->hasForcedStaticDispatch())
    return MethodDispatch::Static;

  // Import-as-member declarations are always statically referenced.
  if (method->isImportAsMember())
    return MethodDispatch::Static;

  // If this declaration is in a class but not marked final, then it is
  // always dynamically dispatched.
  auto dc = method->getDeclContext();
  if (isa<ClassDecl>(dc))
    return MethodDispatch::Class;

  // Class extension methods are only dynamically dispatched if they're
  // dispatched by objc_msgSend, which happens if they're foreign or dynamic.
  if (dc->getAsClassOrClassExtensionContext()) {
    if (method->hasClangNode())
      return MethodDispatch::Class;
    if (auto fd = dyn_cast<FuncDecl>(method)) {
      if (fd->isAccessor() && fd->getAccessorStorageDecl()->hasClangNode())
        return MethodDispatch::Class;
    }
    if (method->getAttrs().hasAttribute<DynamicAttr>())
      return MethodDispatch::Class;
  }

  // Otherwise, it can be referenced statically.
  return MethodDispatch::Static;
}

bool swift::requiresForeignToNativeThunk(ValueDecl *vd) {
  // Functions imported from C, Objective-C methods imported from Objective-C,
  // as well as methods in @objc protocols (even protocols defined in Swift)
  // require a foreign to native thunk.
  auto dc = vd->getDeclContext();
  if (auto proto = dyn_cast<ProtocolDecl>(dc))
    if (proto->isObjC())
      return true;

  if (auto fd = dyn_cast<FuncDecl>(vd))
    return fd->hasClangNode();

  return false;
}

/// FIXME: merge requiresForeignEntryPoint() into getMethodDispatch() and add
/// an ObjectiveC case to the MethodDispatch enum.
bool swift::requiresForeignEntryPoint(ValueDecl *vd) {
  if (vd->isImportAsMember())
    return true;

  // Final functions never require ObjC dispatch.
  if (vd->isFinal())
    return false;

  if (requiresForeignToNativeThunk(vd))
    return true;

  if (auto *fd = dyn_cast<FuncDecl>(vd)) {
  
    // Property accessors should be generated alongside the property.
    if (fd->isGetterOrSetter())
      return requiresForeignEntryPoint(fd->getAccessorStorageDecl());

    return fd->getAttrs().hasAttribute<DynamicAttr>();
  }

  if (auto *cd = dyn_cast<ConstructorDecl>(vd)) {
    if (cd->hasClangNode())
      return true;

    return cd->getAttrs().hasAttribute<DynamicAttr>();
  }

  if (auto *asd = dyn_cast<AbstractStorageDecl>(vd))
    return asd->requiresForeignGetterAndSetter();

  return vd->getAttrs().hasAttribute<DynamicAttr>();
}

static unsigned getFuncNaturalUncurryLevel(AnyFunctionRef AFR) {
  assert(AFR.getParameterLists().size() >= 1 && "no arguments for func?!");
  unsigned Level = AFR.getParameterLists().size() - 1;
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
    assert((kind == Kind::GlobalAccessor ||
            kind == Kind::GlobalGetter ||
            kind == Kind::StoredPropertyInitializer) &&
           "can only create GlobalAccessor, GlobalGetter or "
           "StoredPropertyInitializer SILDeclRef for var");

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
    assert(ACE->getParameterLists().size() >= 1 &&
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

Optional<AnyFunctionRef> SILDeclRef::getAnyFunctionRef() const {
  if (auto vd = loc.dyn_cast<ValueDecl*>()) {
    if (auto afd = dyn_cast<AbstractFunctionDecl>(vd)) {
      return AnyFunctionRef(afd);
    } else {
      return None;
    }
  }
  return AnyFunctionRef(loc.get<AbstractClosureExpr*>());
}

bool SILDeclRef::isThunk() const {
  return isCurried || isForeignToNativeThunk() || isNativeToForeignThunk();
}

bool SILDeclRef::isClangImported() const {
  if (!hasDecl())
    return false;

  ValueDecl *d = getDecl();
  DeclContext *moduleContext = d->getDeclContext()->getModuleScopeContext();

  if (isa<ClangModuleUnit>(moduleContext)) {
    if (isClangGenerated())
      return true;

    if (isa<ConstructorDecl>(d) || isa<EnumElementDecl>(d))
      return !isForeign;

    if (auto *FD = dyn_cast<FuncDecl>(d))
      if (FD->isAccessor() ||
          isa<NominalTypeDecl>(d->getDeclContext()))
        return !isForeign;
  }
  return false;
}

bool SILDeclRef::isClangGenerated() const {
  if (!hasDecl())
    return false;

  return isClangGenerated(getDecl()->getClangNode());
}

// FIXME: this is a weird predicate.
bool SILDeclRef::isClangGenerated(ClangNode node) {
  if (auto nd = dyn_cast_or_null<clang::NamedDecl>(node.getAsDecl())) {
    // ie, 'static inline' functions for which we must ask Clang to emit a body
    // for explicitly
    if (!nd->isExternallyVisible())
      return true;
  }

  return false;
}

SILLinkage SILDeclRef::getLinkage(ForDefinition_t forDefinition) const {
  // Anonymous functions have shared linkage.
  // FIXME: This should really be the linkage of the parent function.
  if (getAbstractClosureExpr())
    return SILLinkage::Shared;
  
  // Native function-local declarations have shared linkage.
  // FIXME: @objc declarations should be too, but we currently have no way
  // of marking them "used" other than making them external. 
  ValueDecl *d = getDecl();
  DeclContext *moduleContext = d->getDeclContext();
  while (!moduleContext->isModuleScopeContext()) {
    if (!isForeign && moduleContext->isLocalContext())
      return SILLinkage::Shared;
    moduleContext = moduleContext->getParent();
  }
  
  // Currying and calling convention thunks have shared linkage.
  if (isThunk())
    // If a function declares a @_cdecl name, its native-to-foreign thunk
    // is exported with the visibility of the function.
    if (!isNativeToForeignThunk() || !d->getAttrs().hasAttribute<CDeclAttr>())
      return SILLinkage::Shared;
  
  // Enum constructors are essentially the same as thunks, they are
  // emitted by need and have shared linkage.
  if (isEnumElement())
    return SILLinkage::Shared;

  // Stored property initializers have hidden linkage, since they are
  // not meant to be used from outside of their module.
  if (isStoredPropertyInitializer())
    return SILLinkage::Hidden;

  // Declarations imported from Clang modules have shared linkage.
  const SILLinkage ClangLinkage = SILLinkage::Shared;

  if (isClangImported())
    return ClangLinkage;

  // Otherwise, we have external linkage.
  switch (d->getEffectiveAccess()) {
    case Accessibility::Private:
    case Accessibility::FilePrivate:
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

  if (isStoredPropertyInitializer())
    return true;

  if (hasAutoClosureExpr())
    return true;

  return hasDecl() ? getDecl()->isTransparent() : false;
}

/// \brief True if the function should have its body serialized.
bool SILDeclRef::isFragile() const {
  DeclContext *dc;
  if (auto closure = getAbstractClosureExpr())
    dc = closure->getLocalContext();
  else
    dc = getDecl()->getInnermostDeclContext();

  // This is stupid
  return (dc->getResilienceExpansion() == ResilienceExpansion::Minimal);
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
  if (requiresForeignToNativeThunk(getDecl()))
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

/// Use the Clang importer to mangle a Clang declaration.
static void mangleClangDecl(raw_ostream &buffer,
                            const clang::NamedDecl *clangDecl,
                            ASTContext &ctx) {
  auto *importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  importer->getMangledName(buffer, clangDecl);
}

static std::string mangleConstant(SILDeclRef c, StringRef prefix) {
  using namespace Mangle;
  Mangler mangler;

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
  
  // As a special case, Clang functions and globals don't get mangled at all.
  if (c.hasDecl()) {
    if (auto clangDecl = c.getDecl()->getClangDecl()) {
      if (!c.isForeignToNativeThunk() && !c.isNativeToForeignThunk()
          && !c.isCurried) {
        if (auto namedClangDecl = dyn_cast<clang::DeclaratorDecl>(clangDecl)) {
          if (auto asmLabel = namedClangDecl->getAttr<clang::AsmLabelAttr>()) {
            mangler.append('\01');
            mangler.append(asmLabel->getLabel());
          } else if (namedClangDecl->hasAttr<clang::OverloadableAttr>()) {
            std::string storage;
            llvm::raw_string_ostream SS(storage);
            // FIXME: When we can import C++, use Clang's mangler all the time.
            mangleClangDecl(SS, namedClangDecl,
                            c.getDecl()->getASTContext());
            mangler.append(SS.str());
          } else {
            mangler.append(namedClangDecl->getName());
          }
          return mangler.finalize();
        }
      }
    }
  }
  
  switch (c.kind) {
  //   entity ::= declaration                     // other declaration
  case SILDeclRef::Kind::Func:
    if (!c.hasDecl()) {
      mangler.append(introducer);
      mangler.mangleClosureEntity(c.getAbstractClosureExpr(),
                                  c.uncurryLevel);
      return mangler.finalize();
    }

    // As a special case, functions can have manually mangled names.
    // Use the SILGen name only for the original non-thunked, non-curried entry
    // point.
    if (auto NameA = c.getDecl()->getAttrs().getAttribute<SILGenNameAttr>())
      if (!c.isForeignToNativeThunk() && !c.isNativeToForeignThunk()
          && !c.isCurried) {
        mangler.append(NameA->Name);
        return mangler.finalize();
      }
      
    // Use a given cdecl name for native-to-foreign thunks.
    if (auto CDeclA = c.getDecl()->getAttrs().getAttribute<CDeclAttr>())
      if (c.isNativeToForeignThunk()) {
        mangler.append(CDeclA->Name);
        return mangler.finalize();
      }

    // Otherwise, fall through into the 'other decl' case.
    SWIFT_FALLTHROUGH;

  case SILDeclRef::Kind::EnumElement:
    mangler.append(introducer);
    mangler.mangleEntity(c.getDecl(), c.uncurryLevel);
    return mangler.finalize();

  //   entity ::= context 'D'                     // deallocating destructor
  case SILDeclRef::Kind::Deallocator:
    mangler.append(introducer);
    mangler.mangleDestructorEntity(cast<DestructorDecl>(c.getDecl()),
                                   /*isDeallocating*/ true);
    return mangler.finalize();

  //   entity ::= context 'd'                     // destroying destructor
  case SILDeclRef::Kind::Destroyer:
    mangler.append(introducer);
    mangler.mangleDestructorEntity(cast<DestructorDecl>(c.getDecl()),
                                   /*isDeallocating*/ false);
    return mangler.finalize();

  //   entity ::= context 'C' type                // allocating constructor
  case SILDeclRef::Kind::Allocator:
    mangler.append(introducer);
    mangler.mangleConstructorEntity(cast<ConstructorDecl>(c.getDecl()),
                                    /*allocating*/ true,
                                    c.uncurryLevel);
    return mangler.finalize();

  //   entity ::= context 'c' type                // initializing constructor
  case SILDeclRef::Kind::Initializer:
    mangler.append(introducer);
    mangler.mangleConstructorEntity(cast<ConstructorDecl>(c.getDecl()),
                                    /*allocating*/ false,
                                    c.uncurryLevel);
    return mangler.finalize();

  //   entity ::= declaration 'e'                 // ivar initializer
  //   entity ::= declaration 'E'                 // ivar destroyer
  case SILDeclRef::Kind::IVarInitializer:
  case SILDeclRef::Kind::IVarDestroyer:
    mangler.append(introducer);
    mangler.mangleIVarInitDestroyEntity(
      cast<ClassDecl>(c.getDecl()),
      c.kind == SILDeclRef::Kind::IVarDestroyer);
    return mangler.finalize();

  //   entity ::= declaration 'a'                 // addressor
  case SILDeclRef::Kind::GlobalAccessor:
    mangler.append(introducer);
    mangler.mangleAddressorEntity(c.getDecl());
    return mangler.finalize();

  //   entity ::= declaration 'G'                 // getter
  case SILDeclRef::Kind::GlobalGetter:
    mangler.append(introducer);
    mangler.mangleGlobalGetterEntity(c.getDecl());
    return mangler.finalize();

  //   entity ::= context 'e' index               // default arg generator
  case SILDeclRef::Kind::DefaultArgGenerator:
    mangler.append(introducer);
    mangler.mangleDefaultArgumentEntity(cast<AbstractFunctionDecl>(c.getDecl()),
                                        c.defaultArgIndex);
    return mangler.finalize();

  //   entity ::= 'I' declaration 'i'             // stored property initializer
  case SILDeclRef::Kind::StoredPropertyInitializer:
    mangler.append(introducer);
    mangler.mangleInitializerEntity(cast<VarDecl>(c.getDecl()));
    return mangler.finalize();
  }

  llvm_unreachable("bad entity kind!");
}

std::string SILDeclRef::mangle(StringRef prefix) const {
  return mangleConstant(*this, prefix);
 }

SILDeclRef SILDeclRef::getNextOverriddenVTableEntry() const {
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

SILDeclRef SILDeclRef::getBaseOverriddenVTableEntry() const {
  // 'method' is the most final method in the hierarchy which we
  // haven't yet found a compatible override for.  'cur' is the method
  // we're currently looking at.  Compatibility is transitive,
  // so we can forget our original method and just keep going up.
  SILDeclRef method = *this;
  SILDeclRef cur = method;
  while ((cur = cur.getNextOverriddenVTableEntry())) {
    method = cur;
  }
  return method;
}

SILLocation SILDeclRef::getAsRegularLocation() const {
  if (hasDecl())
    return RegularLocation(getDecl());
  return RegularLocation(getAbstractClosureExpr());
}
