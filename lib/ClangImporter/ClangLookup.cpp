//===--- ClangLookup.cpp - Lookup in entities imported from Clang ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains facilities for name lookup in entites imported from Clang
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "SwiftDeclSynthesizer.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterRequests.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/ParseVersion.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Mangle.h"
#include "clang/AST/TemplateBase.h"
#include "clang/AST/Type.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Frontend/Utils.h"
#include "clang/Index/IndexingAction.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Rewrite/Frontend/Rewriters.h"
#include "clang/Sema/DelayedDiagnostic.h"
#include "clang/Sema/Sema.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/CAS/CASReference.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Memory.h"
#include <optional>
#include <string>
#include <utility>

using namespace swift;

namespace {
/// Collects name lookup results into the given tiny vector, for use in the
/// various ClangImporter lookup routines.
class CollectLookupResults {
  DeclName name;
  TinyPtrVector<ValueDecl *> &result;

public:
  CollectLookupResults(DeclName name, TinyPtrVector<ValueDecl *> &result)
      : name(name), result(result) {}

  void add(ValueDecl *imported) {
    result.push_back(imported);

    // Expand any macros introduced by the Clang importer.
    imported->visitAuxiliaryDecls([&](Decl *decl) {
      auto valueDecl = dyn_cast<ValueDecl>(decl);
      if (!valueDecl)
        return;

      // Bail out if the auxiliary decl was not produced by a macro.
      auto module = decl->getDeclContext()->getParentModule();
      auto *sf = module->getSourceFileContainingLocation(decl->getLoc());
      if (!sf || sf->Kind != SourceFileKind::MacroExpansion)
        return;

      // Only produce results that match the requested name.
      if (!valueDecl->getName().matchesRef(name))
        return;

      result.push_back(valueDecl);
    });
  }
};
} // anonymous namespace

static SmallVector<SwiftLookupTable::SingleEntry, 4>
lookupInClassTemplateSpecialization(
    ASTContext &ctx, const clang::ClassTemplateSpecializationDecl *clangDecl,
    DeclName name) {
  // TODO: we could make this faster if we can cache class templates in the
  // lookup table as well.
  // Import all the names to figure out which ones we're looking for.
  SmallVector<SwiftLookupTable::SingleEntry, 4> found;
  for (auto member : clangDecl->decls()) {
    auto *namedDecl = dyn_cast<clang::NamedDecl>(member);
    if (!namedDecl)
      continue;

    auto memberName = ctx.getClangModuleLoader()->importName(namedDecl);
    if (!memberName)
      continue;

    // Use the base names here because *sometimes* our input name won't have
    // any arguments.
    if (name.getBaseName().compare(memberName.getBaseName()) == 0)
      found.push_back(namedDecl);
  }

  return found;
}

static bool isDirectLookupMemberContext(const clang::Decl *foundClangDecl,
                                        const clang::Decl *memberContext,
                                        const clang::Decl *parent) {
  if (memberContext->getCanonicalDecl() == parent->getCanonicalDecl())
    return true;
  if (auto *namespaceDecl = dyn_cast<clang::NamespaceDecl>(memberContext)) {
    if (namespaceDecl->isInline()) {
      if (auto *memberCtxParent =
              dyn_cast<clang::Decl>(namespaceDecl->getParent()))
        return isDirectLookupMemberContext(foundClangDecl, memberCtxParent,
                                           parent);
    }
  }
  // Enum constant decl can be found in the parent context of the enum decl.
  if (auto *ED = dyn_cast<clang::EnumDecl>(memberContext)) {
    if (isa<clang::EnumConstantDecl>(foundClangDecl)) {
      if (auto *firstDecl = dyn_cast<clang::Decl>(ED->getDeclContext()))
        return firstDecl->getCanonicalDecl() == parent->getCanonicalDecl();
    }
  }
  return false;
}

static_assert(
    std::is_same_v<SwiftLookupTable::SingleEntry, ClangDirectLookupEntry>,
    "ClangDirectLookupRequest should return same type as entries in "
    "SwiftLookupTable");

SmallVector<ClangDirectLookupEntry, 4>
ClangDirectLookupRequest::evaluate(Evaluator &evaluator,
                                   ClangDirectLookupDescriptor desc) const {
  auto &ctx = desc.decl->getASTContext();
  auto *clangDecl = desc.clangDecl;
  // Class templates aren't in the lookup table.
  if (auto *spec = dyn_cast<clang::ClassTemplateSpecializationDecl>(clangDecl))
    return lookupInClassTemplateSpecialization(ctx, spec, desc.name);

  SwiftLookupTable *lookupTable;
  if (isa<clang::NamespaceDecl>(clangDecl)) {
    // DeclContext of a namespace imported into Swift is the __ObjC module.
    lookupTable = ctx.getClangModuleLoader()->findLookupTable(nullptr);
  } else {
    auto *clangModule =
        importer::getClangOwningModule(clangDecl, clangDecl->getASTContext());
    lookupTable = ctx.getClangModuleLoader()->findLookupTable(clangModule);
  }

  auto foundDecls = lookupTable->lookup(
      SerializedSwiftName(desc.name.getBaseName()), EffectiveClangContext());
  // Make sure that `clangDecl` is the parent of all the members we found.
  SmallVector<SwiftLookupTable::SingleEntry, 4> filteredDecls;
  llvm::copy_if(foundDecls, std::back_inserter(filteredDecls),
                [clangDecl](SwiftLookupTable::SingleEntry decl) {
                  auto *foundClangDecl = decl.dyn_cast<clang::NamedDecl *>();
                  if (!foundClangDecl)
                    return false;
                  auto *first = foundClangDecl->getDeclContext();
                  auto *second = cast<clang::DeclContext>(clangDecl);
                  if (auto *firstDecl = dyn_cast<clang::Decl>(first)) {
                    if (auto *secondDecl = dyn_cast<clang::Decl>(second)) {
                      return isDirectLookupMemberContext(foundClangDecl,
                                                         firstDecl, secondDecl);
                    }
                    return false;
                  }
                  return first == second;
                });
  return filteredDecls;
}

TinyPtrVector<ValueDecl *> CXXNamespaceMemberLookup::evaluate(
    Evaluator &evaluator, CXXNamespaceMemberLookupDescriptor desc) const {
  EnumDecl *namespaceDecl = desc.namespaceDecl;
  DeclName name = desc.name;
  auto *clangNamespaceDecl =
      cast<clang::NamespaceDecl>(namespaceDecl->getClangDecl());
  auto &ctx = namespaceDecl->getASTContext();

  TinyPtrVector<ValueDecl *> result;
  CollectLookupResults collector(name, result);

  llvm::SmallPtrSet<clang::NamedDecl *, 8> importedDecls;
  for (auto redecl : clangNamespaceDecl->redecls()) {
    auto allResults = evaluateOrDefault(
        ctx.evaluator, ClangDirectLookupRequest({namespaceDecl, redecl, name}),
        {});

    for (auto found : allResults) {
      auto clangMember = cast<clang::NamedDecl *>(found);
      auto it = importedDecls.insert(clangMember);
      // Skip over members already found during lookup in prior redeclarations.
      if (!it.second)
        continue;
      if (auto import =
              ctx.getClangModuleLoader()->importDeclDirectly(clangMember))
        collector.add(cast<ValueDecl>(import));
    }
  }

  return result;
}

// Just create a specialized function decl for "__swift_interopStaticCast"
// using the types base and derived.
static DeclRefExpr *
getInteropStaticCastDeclRefExpr(ASTContext &ctx,
                                const clang::Module *owningModule, Type base,
                                Type derived) {
  if (base->isForeignReferenceType() && derived->isForeignReferenceType()) {
    base = base->wrapInPointer(PTK_UnsafePointer);
    derived = derived->wrapInPointer(PTK_UnsafePointer);
  }

  // Lookup our static cast helper function in the C++ shim module.
  auto wrapperModule = ctx.getLoadedModule(ctx.getIdentifier(CXX_SHIM_NAME));
  assert(wrapperModule &&
         "CxxShim module is required when using members of a base class. "
         "Make sure you `import CxxShim`.");

  SmallVector<ValueDecl *, 1> results;
  ctx.lookupInModule(wrapperModule, "__swift_interopStaticCast", results);
  assert(
      results.size() == 1 &&
      "Did you forget to define a __swift_interopStaticCast helper function?");
  FuncDecl *staticCastFn = cast<FuncDecl>(results.back());

  // Now we have to force instantiate this. We can't let the type checker do
  // this yet because it can't infer the "To" type.
  auto subst =
      SubstitutionMap::get(staticCastFn->getGenericSignature(), {derived, base},
                           LookUpConformanceInModule());
  auto functionTemplate = const_cast<clang::FunctionTemplateDecl *>(
      cast<clang::FunctionTemplateDecl>(staticCastFn->getClangDecl()));
  auto spec = ctx.getClangModuleLoader()->instantiateCXXFunctionTemplate(
      ctx, functionTemplate, subst);
  auto specializedStaticCastFn =
      cast<FuncDecl>(ctx.getClangModuleLoader()->importDeclDirectly(spec));

  auto staticCastRefExpr = new (ctx)
      DeclRefExpr(ConcreteDeclRef(specializedStaticCastFn), DeclNameLoc(),
                  /*implicit*/ true);
  staticCastRefExpr->setType(specializedStaticCastFn->getInterfaceType());

  return staticCastRefExpr;
}

// Create the following expressions:
// %0 = Builtin.addressof(&self)
// %1 = Builtin.reinterpretCast<UnsafeMutablePointer<Derived>>(%0)
// %2 = __swift_interopStaticCast<UnsafeMutablePointer<Base>?>(%1)
// %3 = %2!
// return %3.pointee
static MemberRefExpr *getSelfInteropStaticCast(FuncDecl *funcDecl,
                                               NominalTypeDecl *baseStruct,
                                               NominalTypeDecl *derivedStruct) {
  auto &ctx = funcDecl->getASTContext();

  auto mutableSelf = [&ctx](FuncDecl *funcDecl) {
    auto selfDecl = funcDecl->getImplicitSelfDecl();

    auto selfRef =
        new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(), /*implicit*/ true);
    selfRef->setType(LValueType::get(selfDecl->getInterfaceType()));

    return selfRef;
  }(funcDecl);

  auto createCallToBuiltin = [&](Identifier name, ArrayRef<Type> substTypes,
                                 Argument arg) {
    auto builtinFn = cast<FuncDecl>(getBuiltinValueDecl(ctx, name));
    auto substMap =
        SubstitutionMap::get(builtinFn->getGenericSignature(), substTypes,
                             LookUpConformanceInModule());
    ConcreteDeclRef builtinFnRef(builtinFn, substMap);
    auto builtinFnRefExpr =
        new (ctx) DeclRefExpr(builtinFnRef, DeclNameLoc(), /*implicit*/ true);

    auto fnType = builtinFn->getInterfaceType();
    if (auto genericFnType = dyn_cast<GenericFunctionType>(fnType.getPointer()))
      fnType = genericFnType->substGenericArgs(substMap);
    builtinFnRefExpr->setType(fnType);
    auto *argList = ArgumentList::createImplicit(ctx, {arg});
    auto callExpr =
        CallExpr::create(ctx, builtinFnRefExpr, argList, /*implicit*/ true);
    callExpr->setThrows(nullptr);
    return callExpr;
  };

  auto rawSelfPointer = createCallToBuiltin(
      ctx.getIdentifier("addressof"), {derivedStruct->getSelfInterfaceType()},
      Argument::implicitInOut(ctx, mutableSelf));
  rawSelfPointer->setType(ctx.TheRawPointerType);

  auto derivedPtrType = derivedStruct->getSelfInterfaceType()->wrapInPointer(
      PTK_UnsafeMutablePointer);
  auto selfPointer =
      createCallToBuiltin(ctx.getIdentifier("reinterpretCast"),
                          {ctx.TheRawPointerType, derivedPtrType},
                          Argument::unlabeled(rawSelfPointer));
  selfPointer->setType(derivedPtrType);

  auto staticCastRefExpr = getInteropStaticCastDeclRefExpr(
      ctx, baseStruct->getClangDecl()->getOwningModule(),
      baseStruct->getSelfInterfaceType()->wrapInPointer(
          PTK_UnsafeMutablePointer),
      derivedStruct->getSelfInterfaceType()->wrapInPointer(
          PTK_UnsafeMutablePointer));
  auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {selfPointer});
  auto casted = CallExpr::createImplicit(ctx, staticCastRefExpr, argList);
  // This will be "Optional<UnsafeMutablePointer<Base>>"
  casted->setType(cast<FunctionType>(staticCastRefExpr->getType().getPointer())
                      ->getResult());
  casted->setThrows(nullptr);

  SubstitutionMap pointeeSubst = SubstitutionMap::get(
      ctx.getUnsafeMutablePointerDecl()->getGenericSignature(),
      {baseStruct->getSelfInterfaceType()}, LookUpConformanceInModule());
  VarDecl *pointeePropertyDecl =
      ctx.getPointerPointeePropertyDecl(PTK_UnsafeMutablePointer);
  auto pointeePropertyRefExpr = new (ctx) MemberRefExpr(
      casted, SourceLoc(), ConcreteDeclRef(pointeePropertyDecl, pointeeSubst),
      DeclNameLoc(),
      /*implicit=*/true);
  pointeePropertyRefExpr->setType(
      LValueType::get(baseStruct->getSelfInterfaceType()));

  return pointeePropertyRefExpr;
}

// Find the base C++ method called by the base function we want to synthesize
// the derived thunk for.
// The base C++ method is either the original C++ method that corresponds
// to the imported base member, or it's the synthesized C++ method thunk
// used in another synthesized derived thunk that acts as a base member here.
static const clang::CXXMethodDecl *
getCalledBaseCxxMethod(FuncDecl *baseMember) {
  if (baseMember->getClangDecl())
    return dyn_cast<clang::CXXMethodDecl>(baseMember->getClangDecl());
  // Another synthesized derived thunk is used as a base member here,
  // so extract its synthesized C++ method.
  auto body = baseMember->getBody();
  if (body->getElements().empty())
    return nullptr;
  ReturnStmt *returnStmt = dyn_cast_or_null<ReturnStmt>(
      body->getElements().front().dyn_cast<Stmt *>());
  if (!returnStmt)
    return nullptr;
  Expr *returnExpr = returnStmt->getResult();
  // Look through a potential 'reinterpretCast' that can be used
  // to cast UnsafeMutablePointer to UnsafePointer in the synthesized
  // Swift body for `.pointee`.
  if (auto *ce = dyn_cast<CallExpr>(returnExpr)) {
    if (auto *v = ce->getCalledValue()) {
      if (v->getModuleContext() ==
              baseMember->getASTContext().TheBuiltinModule &&
          v->getBaseName().userFacingName() == "reinterpretCast") {
        returnExpr = ce->getArgs()->get(0).getExpr();
      }
    }
  }
  // A member ref expr for `.pointee` access can be wrapping a call
  // when looking through the synthesized Swift body for `.pointee`
  // accessor.
  if (MemberRefExpr *mre = dyn_cast<MemberRefExpr>(returnExpr))
    returnExpr = mre->getBase();
  auto *callExpr = dyn_cast<CallExpr>(returnExpr);
  if (!callExpr)
    return nullptr;
  auto *cv = callExpr->getCalledValue();
  if (!cv)
    return nullptr;
  if (!cv->getClangDecl())
    return nullptr;
  return dyn_cast<clang::CXXMethodDecl>(cv->getClangDecl());
}

// Construct a Swift method that represents the synthesized C++ method
// that invokes the base C++ method.
static FuncDecl *synthesizeBaseFunctionDeclCall(ClangImporter &impl,
                                                ASTContext &ctx,
                                                NominalTypeDecl *derivedStruct,
                                                NominalTypeDecl *baseStruct,
                                                FuncDecl *baseMember) {
  auto *cxxMethod = getCalledBaseCxxMethod(baseMember);
  if (!cxxMethod)
    return nullptr;
  auto *newClangMethod =
      SwiftDeclSynthesizer(&impl).synthesizeCXXForwardingMethod(
          cast<clang::CXXRecordDecl>(derivedStruct->getClangDecl()),
          cast<clang::CXXRecordDecl>(baseStruct->getClangDecl()), cxxMethod,
          ForwardingMethodKind::Base);
  if (!newClangMethod)
    return nullptr;
  return cast_or_null<FuncDecl>(
      ctx.getClangModuleLoader()->importDeclDirectly(newClangMethod));
}

// Generates the body of a derived method, that invokes the base
// method.
// The method's body takes the following form:
//   return self.__synthesizedBaseCall_fn(args...)
static std::pair<BraceStmt *, bool>
synthesizeBaseClassMethodBody(AbstractFunctionDecl *afd, void *context) {

  ASTContext &ctx = afd->getASTContext();

  auto funcDecl = cast<FuncDecl>(afd);
  auto derivedStruct =
      cast<NominalTypeDecl>(funcDecl->getDeclContext()->getAsDecl());
  auto baseMember = static_cast<FuncDecl *>(context);
  auto baseStruct =
      cast<NominalTypeDecl>(baseMember->getDeclContext()->getAsDecl());

  auto forwardedFunc = synthesizeBaseFunctionDeclCall(
      *static_cast<ClangImporter *>(ctx.getClangModuleLoader()), ctx,
      derivedStruct, baseStruct, baseMember);
  if (!forwardedFunc) {
    ctx.Diags.diagnose(SourceLoc(), diag::failed_base_method_call_synthesis,
                       funcDecl, baseStruct);
    auto body = BraceStmt::create(ctx, SourceLoc(), {}, SourceLoc(),
                                  /*implicit=*/true);
    return {body, /*isTypeChecked=*/true};
  }

  SmallVector<Expr *, 8> forwardingParams;
  for (auto param : *funcDecl->getParameters()) {
    auto paramRefExpr = new (ctx) DeclRefExpr(param, DeclNameLoc(),
                                              /*Implicit=*/true);
    paramRefExpr->setType(param->getTypeInContext());
    forwardingParams.push_back(paramRefExpr);
  }

  Argument selfArg = [&]() {
    auto *selfDecl = funcDecl->getImplicitSelfDecl();
    auto selfExpr = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                          /*implicit*/ true);
    if (funcDecl->isMutating()) {
      selfExpr->setType(LValueType::get(selfDecl->getInterfaceType()));
      return Argument::implicitInOut(ctx, selfExpr);
    }
    selfExpr->setType(selfDecl->getTypeInContext());
    return Argument::unlabeled(selfExpr);
  }();

  auto *baseMemberExpr =
      new (ctx) DeclRefExpr(ConcreteDeclRef(forwardedFunc), DeclNameLoc(),
                            /*Implicit=*/true);
  baseMemberExpr->setType(forwardedFunc->getInterfaceType());

  auto baseMemberDotCallExpr =
      DotSyntaxCallExpr::create(ctx, baseMemberExpr, SourceLoc(), selfArg);
  baseMemberDotCallExpr->setType(baseMember->getMethodInterfaceType());
  baseMemberDotCallExpr->setThrows(nullptr);

  auto *argList = ArgumentList::forImplicitUnlabeled(ctx, forwardingParams);
  auto *baseMemberCallExpr =
      CallExpr::createImplicit(ctx, baseMemberDotCallExpr, argList);
  baseMemberCallExpr->setType(baseMember->getResultInterfaceType());
  baseMemberCallExpr->setThrows(nullptr);

  auto *returnStmt = ReturnStmt::createImplicit(ctx, baseMemberCallExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit=*/true);
  return {body, /*isTypeChecked=*/true};
}

// How should the synthesized C++ method that returns the field of interest
// from the base class should return the value - by value, or by reference.
enum ReferenceReturnTypeBehaviorForBaseAccessorSynthesis {
  ReturnByValue,
  ReturnByReference,
  ReturnByMutableReference
};

// Synthesize a C++ method that returns the field of interest from the base
// class. This lets Clang take care of the cast from the derived class
// to the base class while the field is accessed.
static clang::CXXMethodDecl *synthesizeCxxBaseGetterAccessorMethod(
    ClangImporter &impl, const clang::CXXRecordDecl *derivedClass,
    const clang::CXXRecordDecl *baseClass, const clang::FieldDecl *field,
    ValueDecl *retainOperationFn,
    ReferenceReturnTypeBehaviorForBaseAccessorSynthesis behavior) {
  auto &clangCtx = impl.getClangASTContext();
  auto &clangSema = impl.getClangSema();

  // Create a new method in the derived class that calls the base method.
  auto name = field->getDeclName();
  if (name.isIdentifier()) {
    std::string newName;
    llvm::raw_string_ostream os(newName);
    os << (behavior == ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                           ReturnByMutableReference
               ? "__synthesizedBaseSetterAccessor_"
               : "__synthesizedBaseGetterAccessor_")
       << name.getAsIdentifierInfo()->getName();
    name = clang::DeclarationName(
        &impl.getClangPreprocessor().getIdentifierTable().get(os.str()));
  }
  auto returnType = field->getType();
  if (returnType->isReferenceType())
    returnType = returnType->getPointeeType();
  auto valueReturnType = returnType;
  if (behavior !=
      ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::ReturnByValue) {
    returnType = clangCtx.getRValueReferenceType(
        behavior == ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                        ReturnByReference
            ? returnType.withConst()
            : returnType);
  }
  clang::FunctionProtoType::ExtProtoInfo info;
  if (behavior != ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                      ReturnByMutableReference)
    info.TypeQuals.addConst();
  info.ExceptionSpec.Type = clang::EST_NoThrow;
  auto ftype = clangCtx.getFunctionType(returnType, {}, info);
  auto newMethod = clang::CXXMethodDecl::Create(
      clangCtx, const_cast<clang::CXXRecordDecl *>(derivedClass),
      field->getSourceRange().getBegin(),
      clang::DeclarationNameInfo(name, clang::SourceLocation()), ftype,
      clangCtx.getTrivialTypeSourceInfo(ftype), clang::SC_None,
      /*UsesFPIntrin=*/false, /*isInline=*/true,
      clang::ConstexprSpecKind::Unspecified, field->getSourceRange().getEnd());
  newMethod->setImplicit();
  newMethod->setImplicitlyInline();
  newMethod->setAccess(clang::AccessSpecifier::AS_public);
  if (retainOperationFn) {
    // Return an FRT field at +1.
    newMethod->addAttr(clang::CFReturnsRetainedAttr::CreateImplicit(clangCtx));
  }

  // Create a new Clang diagnostic pool to capture any diagnostics
  // emitted during the construction of the method.
  clang::sema::DelayedDiagnosticPool diagPool{
      clangSema.DelayedDiagnostics.getCurrentPool()};
  auto diagState = clangSema.DelayedDiagnostics.push(diagPool);

  // Returns the expression that accesses the base field from derived type.
  auto createFieldAccess = [&]() -> clang::Expr * {
    auto *thisExpr = clang::CXXThisExpr::Create(
        clangCtx, clang::SourceLocation(), newMethod->getThisType(),
        /*IsImplicit=*/false);
    clang::QualType baseClassPtr = clangCtx.getRecordType(baseClass);
    baseClassPtr.addConst();
    baseClassPtr = clangCtx.getPointerType(baseClassPtr);

    clang::CastKind Kind;
    clang::CXXCastPath Path;
    clangSema.CheckPointerConversion(thisExpr, baseClassPtr, Kind, Path,
                                     /*IgnoreBaseAccess=*/false,
                                     /*Diagnose=*/true);
    auto conv = clangSema.ImpCastExprToType(thisExpr, baseClassPtr, Kind,
                                            clang::VK_PRValue, &Path);
    if (!conv.isUsable())
      return nullptr;
    auto memberExpr = clangSema.BuildMemberExpr(
        conv.get(), /*isArrow=*/true, clang::SourceLocation(),
        clang::NestedNameSpecifierLoc(), clang::SourceLocation(),
        const_cast<clang::FieldDecl *>(field),
        clang::DeclAccessPair::make(const_cast<clang::FieldDecl *>(field),
                                    clang::AS_public),
        /*HadMultipleCandidates=*/false,
        clang::DeclarationNameInfo(field->getDeclName(),
                                   clang::SourceLocation()),
        valueReturnType, clang::VK_LValue, clang::OK_Ordinary);
    auto returnCast = clangSema.ImpCastExprToType(memberExpr, valueReturnType,
                                                  clang::CK_LValueToRValue,
                                                  clang::VK_PRValue);
    if (!returnCast.isUsable())
      return nullptr;
    return returnCast.get();
  };

  llvm::SmallVector<clang::Stmt *, 2> body;
  if (retainOperationFn) {
    // Check if the returned value needs to be retained. This might occur if the
    // field getter is returning a shared reference type using, as it needs to
    // perform the retain to match the expected @owned convention.
    auto *retainClangFn =
        dyn_cast<clang::FunctionDecl>(retainOperationFn->getClangDecl());
    if (!retainClangFn) {
      return nullptr;
    }
    auto *fnRef = new (clangCtx) clang::DeclRefExpr(
        clangCtx, const_cast<clang::FunctionDecl *>(retainClangFn), false,
        retainClangFn->getType(), clang::ExprValueKind::VK_LValue,
        clang::SourceLocation());
    auto fieldExpr = createFieldAccess();
    if (!fieldExpr)
      return nullptr;
    auto retainCall = clangSema.BuildResolvedCallExpr(
        fnRef, const_cast<clang::FunctionDecl *>(retainClangFn),
        clang::SourceLocation(), {fieldExpr}, clang::SourceLocation());
    if (!retainCall.isUsable())
      return nullptr;
    body.push_back(retainCall.get());
  }

  // Construct the method's body.
  auto fieldExpr = createFieldAccess();
  if (!fieldExpr)
    return nullptr;
  auto returnStmt = clang::ReturnStmt::Create(clangCtx, clang::SourceLocation(),
                                              fieldExpr, nullptr);
  body.push_back(returnStmt);

  // Check if there were any Clang errors during the construction
  // of the method body.
  clangSema.DelayedDiagnostics.popWithoutEmitting(diagState);
  if (!diagPool.empty())
    return nullptr;
  newMethod->setBody(body.size() > 1
                         ? clang::CompoundStmt::Create(
                               clangCtx, body, clang::FPOptionsOverride(),
                               clang::SourceLocation(), clang::SourceLocation())
                         : body[0]);
  return newMethod;
}

// Generates the body of a derived method, that invokes the base
// field getter or the base subscript.
// The method's body takes the following form:
//   return self.__synthesizedBaseCall_fn(args...)
static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldGetterOrAddressGetterBody(AbstractFunctionDecl *afd,
                                                  void *context,
                                                  AccessorKind kind) {
  assert(kind == AccessorKind::Get || kind == AccessorKind::Address ||
         kind == AccessorKind::MutableAddress);
  ASTContext &ctx = afd->getASTContext();

  AccessorDecl *getterDecl = cast<AccessorDecl>(afd);
  AbstractStorageDecl *baseClassVar =
      static_cast<AbstractStorageDecl *>(context);
  NominalTypeDecl *baseStruct =
      cast<NominalTypeDecl>(baseClassVar->getDeclContext()->getAsDecl());
  NominalTypeDecl *derivedStruct =
      cast<NominalTypeDecl>(getterDecl->getDeclContext()->getAsDecl());

  const clang::Decl *baseClangDecl;
  if (baseClassVar->getClangDecl())
    baseClangDecl = baseClassVar->getClangDecl();
  else
    baseClangDecl = getCalledBaseCxxMethod(baseClassVar->getAccessor(kind));

  clang::CXXMethodDecl *baseGetterCxxMethod = nullptr;
  if (auto *md = dyn_cast_or_null<clang::CXXMethodDecl>(baseClangDecl)) {
    // Subscript operator, or `.pointee` wrapper is represented through a
    // generated C++ method call that calls the base operator.
    baseGetterCxxMethod =
        SwiftDeclSynthesizer(
            static_cast<ClangImporter *>(ctx.getClangModuleLoader()))
            .synthesizeCXXForwardingMethod(
                cast<clang::CXXRecordDecl>(derivedStruct->getClangDecl()),
                cast<clang::CXXRecordDecl>(baseStruct->getClangDecl()), md,
                ForwardingMethodKind::Base,
                getterDecl->getResultInterfaceType()->isForeignReferenceType()
                    ? ReferenceReturnTypeBehaviorForBaseMethodSynthesis::
                          RemoveReferenceIfPointer
                    : (kind != AccessorKind::Get
                           ? ReferenceReturnTypeBehaviorForBaseMethodSynthesis::
                                 KeepReference
                           : ReferenceReturnTypeBehaviorForBaseMethodSynthesis::
                                 RemoveReference),
                /*forceConstQualifier=*/kind != AccessorKind::MutableAddress);
  } else if (auto *fd = dyn_cast_or_null<clang::FieldDecl>(baseClangDecl)) {
    ValueDecl *retainOperationFn = nullptr;
    // Check if this field getter is returning a retainable FRT.
    if (getterDecl->getResultInterfaceType()->isForeignReferenceType()) {
      auto retainOperation = evaluateOrDefault(
          ctx.evaluator,
          CustomRefCountingOperation({getterDecl->getResultInterfaceType()
                                          ->lookThroughAllOptionalTypes()
                                          ->getClassOrBoundGenericClass(),
                                      CustomRefCountingOperationKind::retain}),
          {});
      if (retainOperation.kind ==
          CustomRefCountingOperationResult::foundOperation) {
        retainOperationFn = retainOperation.operation;
      }
    }
    // Field getter is represented through a generated
    // C++ method call that returns the value of the base field.
    baseGetterCxxMethod = synthesizeCxxBaseGetterAccessorMethod(
        *static_cast<ClangImporter *>(ctx.getClangModuleLoader()),
        cast<clang::CXXRecordDecl>(derivedStruct->getClangDecl()),
        cast<clang::CXXRecordDecl>(baseStruct->getClangDecl()), fd,
        retainOperationFn,
        kind == AccessorKind::Get
            ? ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::ReturnByValue
            : (kind == AccessorKind::Address
                   ? ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                         ReturnByReference
                   : ReferenceReturnTypeBehaviorForBaseAccessorSynthesis::
                         ReturnByMutableReference));
  }

  if (!baseGetterCxxMethod) {
    ctx.Diags.diagnose(SourceLoc(), diag::failed_base_method_call_synthesis,
                       getterDecl, baseStruct);
    auto body = BraceStmt::create(ctx, SourceLoc(), {}, SourceLoc(),
                                  /*implicit=*/true);
    return {body, true};
  }
  auto *baseGetterMethod = cast<FuncDecl>(
      ctx.getClangModuleLoader()->importDeclDirectly(baseGetterCxxMethod));

  Argument selfArg = [&]() {
    auto selfDecl = getterDecl->getImplicitSelfDecl();
    auto selfExpr = new (ctx) DeclRefExpr(selfDecl, DeclNameLoc(),
                                          /*implicit*/ true);
    if (kind == AccessorKind::MutableAddress) {
      selfExpr->setType(LValueType::get(selfDecl->getInterfaceType()));
      return Argument::implicitInOut(ctx, selfExpr);
    }
    selfExpr->setType(selfDecl->getTypeInContext());
    return Argument::unlabeled(selfExpr);
  }();

  auto *baseMemberExpr =
      new (ctx) DeclRefExpr(ConcreteDeclRef(baseGetterMethod), DeclNameLoc(),
                            /*Implicit=*/true);
  baseMemberExpr->setType(baseGetterMethod->getInterfaceType());

  auto baseMemberDotCallExpr =
      DotSyntaxCallExpr::create(ctx, baseMemberExpr, SourceLoc(), selfArg);
  baseMemberDotCallExpr->setType(baseGetterMethod->getMethodInterfaceType());
  baseMemberDotCallExpr->setThrows(nullptr);

  ArgumentList *argumentList;
  if (isa<SubscriptDecl>(baseClassVar)) {
    auto paramDecl = getterDecl->getParameters()->get(0);
    auto paramRefExpr = new (ctx) DeclRefExpr(paramDecl, DeclNameLoc(),
                                              /*Implicit=*/true);
    paramRefExpr->setType(paramDecl->getTypeInContext());
    argumentList = ArgumentList::forImplicitUnlabeled(ctx, {paramRefExpr});
  } else {
    argumentList = ArgumentList::forImplicitUnlabeled(ctx, {});
  }

  auto *baseMemberCallExpr =
      CallExpr::createImplicit(ctx, baseMemberDotCallExpr, argumentList);
  Type resultType = baseGetterMethod->getResultInterfaceType();
  baseMemberCallExpr->setType(resultType);
  baseMemberCallExpr->setThrows(nullptr);

  Expr *returnExpr = baseMemberCallExpr;
  // Cast an 'address' result from a mutable pointer if needed.
  if (kind == AccessorKind::Address &&
      baseGetterMethod->getResultInterfaceType()->isUnsafeMutablePointer()) {
    auto finalResultType = getterDecl->getResultInterfaceType();
    returnExpr = SwiftDeclSynthesizer::synthesizeReturnReinterpretCast(
        ctx, baseGetterMethod->getResultInterfaceType(), finalResultType,
        returnExpr);
  }

  auto *returnStmt = ReturnStmt::createImplicit(ctx, returnExpr);

  auto body = BraceStmt::create(ctx, SourceLoc(), {returnStmt}, SourceLoc(),
                                /*implicit=*/true);
  return {body, /*isTypeChecked=*/true};
}

static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldGetterBody(AbstractFunctionDecl *afd, void *context) {
  return synthesizeBaseClassFieldGetterOrAddressGetterBody(afd, context,
                                                           AccessorKind::Get);
}

static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldAddressGetterBody(AbstractFunctionDecl *afd,
                                          void *context) {
  return synthesizeBaseClassFieldGetterOrAddressGetterBody(
      afd, context, AccessorKind::Address);
}

// For setters we have to pass self as a pointer and then emit an assign:
//   %0 = Builtin.addressof(&self)
//   %1 = Builtin.reinterpretCast<UnsafeMutablePointer<Derived>>(%0)
//   %2 = __swift_interopStaticCast<UnsafeMutablePointer<Base>?>(%1)
//   %3 = %2!
//   %4 = %3.pointee
//   assign newValue to %4
static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldSetterBody(AbstractFunctionDecl *afd, void *context) {
  auto setterDecl = cast<AccessorDecl>(afd);
  AbstractStorageDecl *baseClassVar =
      static_cast<AbstractStorageDecl *>(context);
  ASTContext &ctx = setterDecl->getASTContext();

  NominalTypeDecl *baseStruct =
      cast<NominalTypeDecl>(baseClassVar->getDeclContext()->getAsDecl());
  NominalTypeDecl *derivedStruct =
      cast<NominalTypeDecl>(setterDecl->getDeclContext()->getAsDecl());

  auto *pointeePropertyRefExpr =
      getSelfInteropStaticCast(setterDecl, baseStruct, derivedStruct);

  Expr *storedRef = nullptr;
  if (auto subscript = dyn_cast<SubscriptDecl>(baseClassVar)) {
    auto paramDecl = setterDecl->getParameters()->get(1);
    auto paramRefExpr = new (ctx) DeclRefExpr(paramDecl, DeclNameLoc(),
                                              /*Implicit=*/true);
    paramRefExpr->setType(paramDecl->getTypeInContext());

    auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {paramRefExpr});
    storedRef =
        SubscriptExpr::create(ctx, pointeePropertyRefExpr, argList, subscript);
    storedRef->setType(LValueType::get(subscript->getElementInterfaceType()));
  } else {
    // If the base class var has a clang decl, that means it's an access into a
    // stored field. Otherwise, we're looking into another base class, so it's a
    // another synthesized accessor.
    AccessSemantics accessKind = baseClassVar->getClangDecl()
                                     ? AccessSemantics::DirectToStorage
                                     : AccessSemantics::DirectToImplementation;

    storedRef = new (ctx)
        MemberRefExpr(pointeePropertyRefExpr, SourceLoc(), baseClassVar,
                      DeclNameLoc(), /*Implicit=*/true, accessKind);
    storedRef->setType(
        LValueType::get(cast<VarDecl>(baseClassVar)->getTypeInContext()));
  }

  auto newValueParamRefExpr =
      new (ctx) DeclRefExpr(setterDecl->getParameters()->get(0), DeclNameLoc(),
                            /*Implicit=*/true);
  newValueParamRefExpr->setType(
      setterDecl->getParameters()->get(0)->getTypeInContext());

  auto assignExpr =
      new (ctx) AssignExpr(storedRef, SourceLoc(), newValueParamRefExpr,
                           /*implicit*/ true);
  assignExpr->setType(TupleType::getEmpty(ctx));

  auto body = BraceStmt::create(ctx, SourceLoc(), {assignExpr}, SourceLoc(),
                                /*implicit*/ true);
  return {body, /*isTypeChecked=*/true};
}

static std::pair<BraceStmt *, bool>
synthesizeBaseClassFieldAddressSetterBody(AbstractFunctionDecl *afd,
                                          void *context) {
  return synthesizeBaseClassFieldGetterOrAddressGetterBody(
      afd, context, AccessorKind::MutableAddress);
}

static SmallVector<AccessorDecl *, 2>
makeBaseClassMemberAccessors(DeclContext *declContext,
                             AbstractStorageDecl *computedVar,
                             AbstractStorageDecl *baseClassVar) {
  auto &ctx = declContext->getASTContext();
  auto computedType = computedVar->getInterfaceType();
  auto contextTy = declContext->mapTypeIntoEnvironment(computedType);

  // Use 'address' or 'mutableAddress' accessors for non-copyable
  // types, unless the base accessor returns it by value.
  bool useAddress = contextTy->isNoncopyable() &&
                    (baseClassVar->getReadImpl() == ReadImplKind::Stored ||
                     baseClassVar->getAccessor(AccessorKind::Address));

  ParameterList *bodyParams = nullptr;
  if (auto subscript = dyn_cast<SubscriptDecl>(baseClassVar)) {
    computedType = computedType->getAs<FunctionType>()->getResult();

    auto idxParam = subscript->getIndices()->get(0);
    bodyParams = ParameterList::create(ctx, {idxParam});
  } else {
    bodyParams = ParameterList::createEmpty(ctx);
  }

  auto getterDecl = AccessorDecl::create(
      ctx,
      /*FuncLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(),
      useAddress ? AccessorKind::Address : AccessorKind::Get, computedVar,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(), bodyParams,
      useAddress ? computedType->wrapInPointer(PTK_UnsafePointer)
                 : computedType,
      declContext);
  getterDecl->setIsTransparent(true);
  getterDecl->copyFormalAccessFrom(computedVar);
  getterDecl->setBodySynthesizer(useAddress
                                     ? synthesizeBaseClassFieldAddressGetterBody
                                     : synthesizeBaseClassFieldGetterBody,
                                 baseClassVar);
  if (baseClassVar->getWriteImpl() == WriteImplKind::Immutable)
    return {getterDecl};

  auto newValueParam =
      new (ctx) ParamDecl(SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                          ctx.getIdentifier("newValue"), declContext);
  newValueParam->setSpecifier(ParamSpecifier::Default);
  newValueParam->setInterfaceType(computedType);

  SmallVector<ParamDecl *, 2> setterParamDecls;
  if (!useAddress)
    setterParamDecls.push_back(newValueParam);
  if (auto subscript = dyn_cast<SubscriptDecl>(baseClassVar))
    setterParamDecls.push_back(subscript->getIndices()->get(0));
  ParameterList *setterBodyParams =
      ParameterList::create(ctx, setterParamDecls);

  auto setterDecl = AccessorDecl::create(
      ctx,
      /*FuncLoc=*/SourceLoc(),
      /*AccessorKeywordLoc=*/SourceLoc(),
      useAddress ? AccessorKind::MutableAddress : AccessorKind::Set,
      computedVar,
      /*Async=*/false, /*AsyncLoc=*/SourceLoc(),
      /*Throws=*/false,
      /*ThrowsLoc=*/SourceLoc(), /*ThrownType=*/TypeLoc(), setterBodyParams,
      useAddress ? computedType->wrapInPointer(PTK_UnsafeMutablePointer)
                 : TupleType::getEmpty(ctx),
      declContext);
  setterDecl->setIsTransparent(true);
  setterDecl->copyFormalAccessFrom(computedVar);
  setterDecl->setBodySynthesizer(useAddress
                                     ? synthesizeBaseClassFieldAddressSetterBody
                                     : synthesizeBaseClassFieldSetterBody,
                                 baseClassVar);
  setterDecl->setSelfAccessKind(SelfAccessKind::Mutating);

  return {getterDecl, setterDecl};
}

// Clone attributes that have been imported from Clang.
static void cloneImportedAttributes(ValueDecl *fromDecl, ValueDecl *toDecl) {
  ASTContext &context = fromDecl->getASTContext();
  for (auto attr : fromDecl->getAttrs()) {
    switch (attr->getKind()) {
    case DeclAttrKind::Available: {
      toDecl->addAttribute(cast<AvailableAttr>(attr)->clone(context, true));
      break;
    }
    case DeclAttrKind::Custom: {
      CustomAttr *cAttr = cast<CustomAttr>(attr);
      toDecl->addAttribute(
          CustomAttr::create(context, SourceLoc(), cAttr->getTypeExpr(),
                             /*owner*/ toDecl, cAttr->getInitContext(),
                             cAttr->getArgs(), /*implicit*/ true));
      break;
    }
    case DeclAttrKind::DiscardableResult: {
      toDecl->addAttribute(new (context) DiscardableResultAttr(true));
      break;
    }
    case DeclAttrKind::Effects: {
      toDecl->addAttribute(cast<EffectsAttr>(attr)->clone(context));
      break;
    }
    case DeclAttrKind::Final: {
      toDecl->addAttribute(new (context) FinalAttr(true));
      break;
    }
    case DeclAttrKind::Transparent: {
      toDecl->addAttribute(new (context) TransparentAttr(true));
      break;
    }
    case DeclAttrKind::WarnUnqualifiedAccess: {
      toDecl->addAttribute(new (context) WarnUnqualifiedAccessAttr(true));
      break;
    }
    default:
      break;
    }
  }
}

static ValueDecl *cloneBaseMemberDecl(ValueDecl *decl, DeclContext *newContext,
                                      ClangInheritanceInfo inheritance) {
  AccessLevel access = inheritance.accessForBaseDecl(decl);
  ASTContext &context = decl->getASTContext();

  if (auto fn = dyn_cast<FuncDecl>(decl)) {
    // TODO: function templates are specialized during type checking so to
    // support these we need to tell Swift to type check the synthesized bodies.
    // TODO: we also currently don't support static functions. That shouldn't be
    // too hard.
    if (fn->isStatic() ||
        isa_and_nonnull<clang::FunctionTemplateDecl>(fn->getClangDecl()))
      return nullptr;
    if (auto cxxMethod =
            dyn_cast_or_null<clang::CXXMethodDecl>(fn->getClangDecl())) {
      // FIXME: if this function has rvalue this, we won't be able to synthesize
      // the accessor correctly (https://github.com/apple/swift/issues/69745).
      if (cxxMethod->getRefQualifier() == clang::RefQualifierKind::RQ_RValue)
        return nullptr;
    }

    auto out = FuncDecl::createImplicit(
        context, fn->getStaticSpelling(), fn->getName(), fn->getNameLoc(),
        fn->hasAsync(), fn->hasThrows(), fn->getThrownInterfaceType(),
        fn->getGenericParams(), fn->getParameters(),
        fn->getResultInterfaceType(), newContext);
    cloneImportedAttributes(decl, out);
    out->setAccess(access);
    inheritance.setUnavailableIfNecessary(decl, out);
    out->setBodySynthesizer(synthesizeBaseClassMethodBody, fn);
    out->setSelfAccessKind(fn->getSelfAccessKind());
    return out;
  }

  if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
    auto contextTy = newContext->mapTypeIntoEnvironment(
        subscript->getElementInterfaceType());
    // Subscripts that return non-copyable types are not yet supported.
    // See: https://github.com/apple/swift/issues/70047.
    if (contextTy->isNoncopyable())
      return nullptr;
    auto out = SubscriptDecl::create(
        subscript->getASTContext(), subscript->getName(),
        subscript->getStaticLoc(), subscript->getStaticSpelling(),
        subscript->getSubscriptLoc(), subscript->getIndices(),
        subscript->getNameLoc(), subscript->getElementInterfaceType(),
        newContext, subscript->getGenericParams());
    out->setAccess(access);
    inheritance.setUnavailableIfNecessary(decl, out);
    out->setAccessors(SourceLoc(),
                      makeBaseClassMemberAccessors(newContext, out, subscript),
                      SourceLoc());
    out->setImplInfo(subscript->getImplInfo());
    return out;
  }

  if (auto var = dyn_cast<VarDecl>(decl)) {
    auto oldContext = var->getDeclContext();
    auto oldTypeDecl = oldContext->getSelfNominalTypeDecl();
    // FIXME: this is a workaround for rdar://128013193
    if (oldTypeDecl->getAttrs().hasAttribute<MoveOnlyAttr>() &&
        context.LangOpts.CxxInteropUseOpaquePointerForMoveOnly)
      return nullptr;

    auto rawMemory = allocateMemoryForDecl<VarDecl>(var->getASTContext(),
                                                    sizeof(VarDecl), false);
    auto out =
        new (rawMemory) VarDecl(var->isStatic(), var->getIntroducer(),
                                var->getLoc(), var->getName(), newContext);
    out->setInterfaceType(var->getInterfaceType());
    out->setIsObjC(var->isObjC());
    out->setIsDynamic(var->isDynamic());
    out->setAccess(access);
    inheritance.setUnavailableIfNecessary(decl, out);
    out->getASTContext().evaluator.cacheOutput(HasStorageRequest{out}, false);
    auto accessors = makeBaseClassMemberAccessors(newContext, out, var);
    out->setAccessors(SourceLoc(), accessors, SourceLoc());
    auto isMutable = var->getWriteImpl() == WriteImplKind::Immutable
                         ? StorageIsNotMutable
                         : StorageIsMutable;
    out->setImplInfo(
        accessors[0]->getAccessorKind() == AccessorKind::Address
            ? (accessors.size() > 1
                   ? StorageImplInfo(ReadImplKind::Address,
                                     WriteImplKind::MutableAddress,
                                     ReadWriteImplKind::MutableAddress)
                   : StorageImplInfo(ReadImplKind::Address))
            : StorageImplInfo::getComputed(isMutable));
    out->setIsSetterMutating(true);
    return out;
  }

  if (auto typeAlias = dyn_cast<TypeAliasDecl>(decl)) {
    auto rawMemory = allocateMemoryForDecl<TypeAliasDecl>(
        typeAlias->getASTContext(), sizeof(TypeAliasDecl), false);
    auto out = new (rawMemory)
        TypeAliasDecl(typeAlias->getStartLoc(), typeAlias->getEqualLoc(),
                      typeAlias->getName(), typeAlias->getNameLoc(),
                      typeAlias->getGenericParams(), newContext);
    out->setUnderlyingType(typeAlias->getUnderlyingType());
    out->setAccess(access);
    inheritance.setUnavailableIfNecessary(decl, out);
    return out;
  }

  if (auto typeDecl = dyn_cast<TypeDecl>(decl)) {
    auto rawMemory = allocateMemoryForDecl<TypeAliasDecl>(
        typeDecl->getASTContext(), sizeof(TypeAliasDecl), false);
    auto out = new (rawMemory) TypeAliasDecl(
        typeDecl->getLoc(), typeDecl->getLoc(), typeDecl->getName(),
        typeDecl->getLoc(), nullptr, newContext);
    out->setUnderlyingType(typeDecl->getDeclaredInterfaceType());
    out->setAccess(access);
    inheritance.setUnavailableIfNecessary(decl, out);
    return out;
  }

  return nullptr;
}

ValueDecl *ClangImporter::Implementation::importBaseMemberDecl(
    ValueDecl *decl, DeclContext *newContext,
    ClangInheritanceInfo inheritance) {

  // Make sure we don't clone the decl again for this class, as that would
  // result in multiple definitions of the same symbol.
  std::pair<ValueDecl *, DeclContext *> key = {decl, newContext};
  auto known = clonedBaseMembers.find(key);
  if (known == clonedBaseMembers.end()) {
    ValueDecl *cloned = cloneBaseMemberDecl(decl, newContext, inheritance);
    handleAmbiguousSwiftName(cloned);
    known = clonedBaseMembers.insert({key, cloned}).first;
    clonedMembers.insert(std::make_pair(cloned, decl));
  }

  return known->second;
}

ValueDecl *ClangImporter::Implementation::getOriginalForClonedMember(
    const ValueDecl *decl) {
  // If this is a cloned decl, we don't want to reclone it
  // Otherwise, we may end up with multiple copies of the same method
  if (!decl->hasClangNode()) {
    // Skip decls with a clang node as those will never be a clone
    auto result = clonedMembers.find(decl);
    if (result != clonedMembers.end())
      return result->getSecond();
  }

  return nullptr;
}

size_t ClangImporter::Implementation::getImportedBaseMemberDeclArity(
    const ValueDecl *valueDecl) {
  if (auto *func = dyn_cast<FuncDecl>(valueDecl)) {
    if (auto *params = func->getParameters()) {
      return params->size();
    }
  }
  return 0;
}

TinyPtrVector<ValueDecl *> ClangRecordMemberLookup::evaluate(
    Evaluator &evaluator, ClangRecordMemberLookupDescriptor desc) const {
  NominalTypeDecl *recordDecl = desc.recordDecl;
  NominalTypeDecl *inheritingDecl = desc.inheritingDecl;
  DeclName name = desc.name;
  ClangInheritanceInfo inheritance = desc.inheritance;

  auto &ctx = recordDecl->getASTContext();

  // Whether to skip non-public members. Feature::ImportNonPublicCxxMembers says
  // to import all non-public members by default; if that is disabled, we only
  // import non-public members annotated with SWIFT_PRIVATE_FILEID (since those
  // are the only classes that need non-public members.)
  auto *cxxRecordDecl =
      dyn_cast<clang::CXXRecordDecl>(inheritingDecl->getClangDecl());
  auto skipIfNonPublic =
      !ctx.LangOpts.hasFeature(Feature::ImportNonPublicCxxMembers) &&
      cxxRecordDecl && importer::getPrivateFileIDAttrs(cxxRecordDecl).empty();

  auto directResults = evaluateOrDefault(
      ctx.evaluator,
      ClangDirectLookupRequest({recordDecl, recordDecl->getClangDecl(), name}),
      {});

  // The set of declarations we found.
  TinyPtrVector<ValueDecl *> result;
  CollectLookupResults collector(name, result);

  // Find the results that are actually a member of "recordDecl".
  ClangModuleLoader *clangModuleLoader = ctx.getClangModuleLoader();
  for (auto foundEntry : directResults) {
    auto found = cast<clang::NamedDecl *>(foundEntry);
    if (dyn_cast<clang::Decl>(found->getDeclContext()) !=
        recordDecl->getClangDecl())
      continue;

    // We should not import 'found' if the following are all true:
    //
    // -  Feature::ImportNonPublicCxxMembers is not enabled
    // -  'found' is not a member of a SWIFT_PRIVATE_FILEID-annotated class
    // -  'found' is a non-public member.
    // -  'found' is not a non-inherited FieldDecl; we must import private
    //    fields because they may affect implicit conformances that iterate
    //    through all of a struct's fields, e.g., Sendable (#76892).
    //
    // Note that we can skip inherited FieldDecls because implicit conformances
    // handle those separately.
    //
    // The first two conditions are captured by skipIfNonPublic. The next two
    // are conveyed by the following:
    auto nonPublic = found->getAccess() == clang::AS_private ||
                     found->getAccess() == clang::AS_protected;
    auto noninheritedField = !inheritance && isa<clang::FieldDecl>(found);
    if (skipIfNonPublic && nonPublic && !noninheritedField)
      continue;

    // Don't import constructors on foreign reference types.
    if (isa<clang::CXXConstructorDecl>(found) && isa<ClassDecl>(recordDecl))
      continue;

    auto imported = clangModuleLoader->importDeclDirectly(found);
    if (!imported)
      continue;

    // If this member is found due to inheritance, clone it from the base class
    // by synthesizing getters and setters.
    if (inheritance) {
      imported = clangModuleLoader->importBaseMemberDecl(
          cast<ValueDecl>(imported), inheritingDecl, inheritance);
      if (!imported)
        continue;
    }

    collector.add(cast<ValueDecl>(imported));
  }

  if (inheritance) {
    // For inherited members, add members that are synthesized eagerly, such as
    // subscripts. This is not necessary for non-inherited members because those
    // should already be in the lookup table.
    for (auto member :
         cast<NominalTypeDecl>(recordDecl)->getCurrentMembersWithoutLoading()) {
      auto namedMember = dyn_cast<ValueDecl>(member);
      if (!namedMember || !namedMember->hasName() ||
          namedMember->getName().getBaseName() != name ||
          clangModuleLoader->getOriginalForClonedMember(namedMember))
        continue;

      auto *imported = clangModuleLoader->importBaseMemberDecl(
          namedMember, inheritingDecl, inheritance);
      if (!imported)
        continue;

      collector.add(imported);
    }
  }

  // If this is a C++ record, look through any base classes.
  const clang::CXXRecordDecl *cxxRecord;
  if ((cxxRecord =
           dyn_cast<clang::CXXRecordDecl>(recordDecl->getClangDecl())) &&
      cxxRecord->isCompleteDefinition()) {
    // Capture the arity of already found members in the
    // current record, to avoid adding ambiguous members
    // from base classes.
    llvm::SmallSet<DeclName, 4> foundMethodNames;
    for (const auto *valueDecl : result)
      foundMethodNames.insert(valueDecl->getName());

    for (auto base : cxxRecord->bases()) {
      if (skipIfNonPublic && base.getAccessSpecifier() != clang::AS_public)
        continue;

      clang::QualType baseType = base.getType();
      if (auto spectType =
              dyn_cast<clang::TemplateSpecializationType>(baseType))
        baseType = spectType->desugar();
      if (!isa<clang::RecordType>(baseType.getCanonicalType()))
        continue;

      auto *baseRecord = baseType->getAs<clang::RecordType>()->getDecl();

      if (importer::isSymbolicCircularBase(cxxRecord, baseRecord))
        // Skip circular bases to avoid unbounded recursion
        continue;

      if (auto import = clangModuleLoader->importDeclDirectly(baseRecord)) {
        // If we are looking up the base class, go no further. We will have
        // already found it during the other lookup.
        if (cast<ValueDecl>(import)->getName() == name)
          continue;

        auto baseInheritance = ClangInheritanceInfo(inheritance, base);

        // Add Clang members that are imported lazily.
        auto baseResults = evaluateOrDefault(
            ctx.evaluator,
            ClangRecordMemberLookup({cast<NominalTypeDecl>(import), name,
                                     inheritingDecl, baseInheritance}),
            {});

        for (auto foundInBase : baseResults) {
          // Do not add duplicate entry with the same DeclName,
          // as that would cause an ambiguous lookup.
          if (foundMethodNames.count(foundInBase->getName()))
            continue;

          collector.add(foundInBase);
        }
      }
    }
  }

  return result;
}
