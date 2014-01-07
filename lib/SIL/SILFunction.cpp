//===--- SILFunction.cpp - Defines the SILFunction data structure ---------===//
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

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
// FIXME: For mapTypeInContext
#include "swift/AST/ArchetypeBuilder.h"

using namespace swift;

SILFunction *SILFunction::create(SILModule &M, SILLinkage linkage,
                                 StringRef name,
                                 CanSILFunctionType loweredType,
                                 Optional<SILLocation> loc,
                                 IsBare_t isBareSILFunction,
                                 IsTransparent_t isTrans,
                                 SILFunction *insertBefore,
                                 SILDebugScope *debugScope,
                                 DeclContext *DC) {
  // Get a StringMapEntry for the function.  As a sop to error cases,
  // allow the name to have an empty string.
  llvm::StringMapEntry<SILFunction*> *entry = nullptr;
  if (!name.empty()) {
    entry = &M.FunctionTable.GetOrCreateValue(name);
    assert(!entry->getValue() && "function already exists");
    name = entry->getKey();
  }

  auto fn = new (M) SILFunction(M, linkage, name, loweredType, loc,
                                isBareSILFunction, isTrans, insertBefore,
                                debugScope, DC);

  if (entry) entry->setValue(fn);
  return fn;
}

SILFunction::SILFunction(SILModule &Module, SILLinkage Linkage,
                         StringRef Name, CanSILFunctionType LoweredType,
                         Optional<SILLocation> Loc,
                         IsBare_t isBareSILFunction,
                         IsTransparent_t isTrans,
                         SILFunction *InsertBefore,
                         SILDebugScope *DebugScope,
                         DeclContext *DC)
  : ModuleAndLinkage(&Module, Linkage),
    Name(Name),
    LoweredType(LoweredType),
    // FIXME: Context params should be independent of the function type.
    ContextGenericParams(LoweredType->getGenericParams()),
    Location(Loc),
    DeclCtx(DC),
    DebugScope(DebugScope),
    Bare(isBareSILFunction),
    Transparent(isTrans) {
  if (InsertBefore)
    Module.functions.insert(SILModule::iterator(InsertBefore), this);
  else
    Module.functions.push_back(this);
}

SILFunction::~SILFunction() {
  assert(RefCount == 0 &&
         "Function cannot be deleted while function_ref's still exist");
  getModule().FunctionTable.erase(Name);
}

void SILFunction::setDeclContext(Decl *D) {
  if (!D)
    return;
  switch (D->getKind()) {
  // These four dual-inherit from DeclContext.
  case DeclKind::Func:        DeclCtx = cast<FuncDecl>(D); break;
  case DeclKind::Constructor: DeclCtx = cast<ConstructorDecl>(D); break;
  case DeclKind::Extension:   DeclCtx = cast<ExtensionDecl>(D);   break;
  case DeclKind::Destructor:  DeclCtx = cast<DestructorDecl>(D);  break;
  default:
    DeclCtx = D->getDeclContext();
  }
  assert(DeclCtx);
}

void SILFunction::setDeclContext(Expr *E) {
  DeclCtx = dyn_cast_or_null<AbstractClosureExpr>(E);
}

ASTContext &SILFunction::getASTContext() const {
  return getModule().getASTContext();
}

Type SILFunction::mapTypeIntoContext(Type type) const {
  return ArchetypeBuilder::mapTypeIntoContext(getContextGenericParams(),
                                              type);
}
