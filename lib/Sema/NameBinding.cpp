//===--- NameBinding.cpp - Name Binding -----------------------------------===//
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
//  This file implements name binding for Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/AST.h"
#include "swift/AST/Component.h"
#include "swift/AST/Diagnostics.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/Path.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// NameBinder
//===----------------------------------------------------------------------===//

typedef TranslationUnit::ImportedModule ImportedModule;
typedef llvm::PointerUnion<const ImportedModule*, OneOfType*> BoundScope;

namespace {  
  class NameBinder {
    llvm::error_code findModule(StringRef Module, 
                                SourceLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer);
    
  public:
    TranslationUnit *TU;
    ASTContext &Context;
    
    NameBinder(TranslationUnit *TU) : TU(TU), Context(TU->Ctx) {
    }
    ~NameBinder() {
    }
    
    template<typename ...ArgTypes>
    InFlightDiagnostic diagnose(ArgTypes... Args) {
      return Context.Diags.diagnose(Args...);
    }
    
    void addImport(ImportDecl *ID, SmallVectorImpl<ImportedModule> &Result);

    BoundScope bindScopeName(TypeAliasDecl *TypeFromScope,
                             Identifier Name, SourceLoc NameLoc);

  private:
    /// getModule - Load a module referenced by an import statement,
    /// emitting an error at the specified location and returning null on
    /// failure.
    Module *getModule(std::pair<Identifier,SourceLoc> ModuleID);
  };
}

llvm::error_code NameBinder::findModule(StringRef Module, 
                                        SourceLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer) {
  std::string ModuleFilename = Module.str() + std::string(".swift");
  
  llvm::SmallString<128> InputFilename;
  
  // First, search in the directory corresponding to the import location.
  // FIXME: This screams for a proper FileManager abstraction.
  llvm::SourceMgr &SourceMgr = Context.SourceMgr;
  int CurrentBufferID = SourceMgr.FindBufferContainingLoc(ImportLoc.Value);
  if (CurrentBufferID >= 0) {
    const llvm::MemoryBuffer *ImportingBuffer 
      = SourceMgr.getBufferInfo(CurrentBufferID).Buffer;
    StringRef CurrentDirectory 
      = llvm::sys::path::parent_path(ImportingBuffer->getBufferIdentifier());
    if (!CurrentDirectory.empty()) {
      InputFilename = CurrentDirectory;
      llvm::sys::path::append(InputFilename, ModuleFilename);
      llvm::error_code Err = llvm::MemoryBuffer::getFile(InputFilename, Buffer);
      if (!Err)
        return Err;
    }
  }
  
  // Second, search in the current directory.
  llvm::error_code Err = llvm::MemoryBuffer::getFile(ModuleFilename, Buffer);
  if (!Err)
    return Err;

  // If we fail, search each import search path.
  for (auto Path : Context.ImportSearchPaths) {
    InputFilename = Path;
    llvm::sys::path::append(InputFilename, ModuleFilename);
    Err = llvm::MemoryBuffer::getFile(InputFilename, Buffer);
    if (!Err)
      return Err;
  }

  return Err;
}

Module *NameBinder::getModule(std::pair<Identifier, SourceLoc> ModuleID) {
  // TODO: We currently just recursively parse referenced modules.  This works
  // fine for now since they are each a single file.  Ultimately we'll want a
  // compiled form of AST's like clang's that support lazy deserialization.
  
  // Open the input file.
  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
  if (llvm::error_code Err = findModule(ModuleID.first.str(), ModuleID.second,
                                        InputFile)) {
    diagnose(ModuleID.second, diag::sema_opening_import,
             ModuleID.first.str(), Err.message());
    return 0;
  }

  unsigned BufferID =
    Context.SourceMgr.AddNewSourceBuffer(InputFile.take(),
                                         ModuleID.second.Value);

  // For now, treat all separate modules as unique components.
  Component *Comp = new (Context.Allocate<Component>(1)) Component();

  // Parse the translation unit, but don't do name binding or type checking.
  // This can produce new errors etc if the input is erroneous.
  TranslationUnit *TU = parseTranslationUnit(BufferID, Comp, Context);
  if (TU == 0)
    return 0;
  
  // We have to do name binding on it to ensure that types are fully resolved.
  // This should eventually be eliminated by having actual fully resolved binary
  // dumps of the code instead of reparsing though.
  performNameBinding(TU);
  
  return TU;
}


void NameBinder::addImport(ImportDecl *ID, 
                           SmallVectorImpl<ImportedModule> &Result) {
  ArrayRef<ImportDecl::AccessPathElement> Path = ID->getAccessPath();
  Module *M = getModule(Path[0]);
  if (M == 0) return;
  
  // FIXME: Validate the access path against the module.  Reject things like
  // import swift.aslkdfja
  if (Path.size() > 2) {
    diagnose(Path[2].second, diag::invalid_declaration_imported);
    return;
  }
  
  Result.push_back(std::make_pair(Path.slice(1), M));
}

/// Try to bind an unqualified name into something usable as a scope.
BoundScope NameBinder::bindScopeName(TypeAliasDecl *TypeFromScope,
                                     Identifier Name, SourceLoc NameLoc) {
  // Check whether the "optimistic" type from scope is still
  // undefined.  If not, use that as the actual type; otherwise we'll
  // need to do a lookup from the imports.
  TypeAliasDecl *Type;
  if (TypeFromScope->hasUnderlyingType()) {
    Type = TypeFromScope;
  } else {
    Type = TU->lookupGlobalType(Name, NLKind::UnqualifiedLookup);
  }

  // If that failed, look for a module name.
  if (!Type) {
    for (const ImportedModule &ImpEntry : TU->ImportedModules)
      if (ImpEntry.second->Name == Name)
        return &ImpEntry;
    
    diagnose(NameLoc, diag::no_module_or_type);
    return BoundScope();
  }

  // Otherwise, at least cache the type we found.
  assert(Type->hasUnderlyingType());
  if (!TypeFromScope->hasUnderlyingType()) {
    TypeFromScope->setUnderlyingType(Type->getUnderlyingType());
  }

  // Try to convert that to a type scope.
  TypeBase *Ty = Type->getUnderlyingType()->getCanonicalType();

  // Silently fail if we have an error type.
  if (isa<ErrorType>(Ty)) return BoundScope();
    
  // Reject things like int::x.
  OneOfType *DT = dyn_cast<OneOfType>(Ty);
  if (DT == 0) {
    diagnose(NameLoc, diag::invalid_type_scoped_access, Name);
    return BoundScope();
  }
    
  if (DT->Elements.empty()) {
    diagnose(NameLoc, diag::incomplete_or_empty_oneof, Name);
    return BoundScope();
  }

  return DT;
}

//===----------------------------------------------------------------------===//
// performNameBinding
//===----------------------------------------------------------------------===//

static Expr *BindNames(Expr *E, WalkOrder Order, NameBinder &Binder) {
  
  // Ignore the preorder walk.
  if (Order == WalkOrder::PreOrder)
    return E;

  UnresolvedDeclRefExpr *UDRE = dyn_cast<UnresolvedDeclRefExpr>(E);
  if (UDRE == 0) return E;
  
  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  Identifier Name = UDRE->getName();
  SourceLoc Loc = UDRE->getLoc();
  SmallVector<ValueDecl*, 4> Decls;
  // Perform standard value name lookup.
  Binder.TU->lookupGlobalValue(Name, NLKind::UnqualifiedLookup, Decls);

  // If that fails, this may be the name of a module, try looking that up.
  if (Decls.empty()) {
    for (const ImportedModule &ImpEntry : Binder.TU->ImportedModules)
      if (ImpEntry.second->Name == Name) {
        ModuleType *MT = ModuleType::get(ImpEntry.second);
        return new (Binder.Context) ModuleExpr(Loc, 
                                      TypeJudgement(MT, ValueKind::RValue));
      }      
  }

  if (Decls.empty()) {
    Binder.diagnose(Loc, diag::use_unresolved_identifier, Name);
    return 0;
  }

  return OverloadSetRefExpr::createWithCopy(Decls, Loc);
}

static void bindNamesInDecl(Decl *D, WalkExprType ^BinderBlock) {
  if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
    if (VD->getInit())
      VD->setInit(VD->getInit()->walk(BinderBlock));
  } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
    for (Decl *Member : ED->getMembers()) {
      bindNamesInDecl(Member, BinderBlock);
    }
  }
}

/// performNameBinding - Once parsing is complete, this walks the AST to
/// resolve names and do other top-level validation.
///
/// At this parsing has been performed, but we still have UnresolvedDeclRefExpr
/// nodes for unresolved value names, and we may have unresolved type names as
/// well.  This handles import directives and forward references.
void swift::performNameBinding(TranslationUnit *TU) {
  NameBinder Binder(TU);

  SmallVector<ImportedModule, 8> ImportedModules;
  
  // Import the builtin library as an implicit import.
  // FIXME: This should only happen for translation units in the standard
  // library.
  ImportedModules.push_back(std::make_pair(Module::AccessPathTy(),
                                           TU->Ctx.TheBuiltinModule));
  
  // FIXME: For translation units not in the standard library, we should import
  // swift.swift implicitly.  We need a way for swift.swift itself to not
  // recursively import itself though.

  // Do a prepass over the declarations to find and load the imported modules.
  for (auto Elt : TU->Body->getElements())
    if (Decl *D = Elt.dyn_cast<Decl*>()) {
      if (ImportDecl *ID = dyn_cast<ImportDecl>(D))
        Binder.addImport(ID, ImportedModules);
    }
  
  TU->setImportedModules(TU->Ctx.AllocateCopy(ImportedModules));
  
  // Type binding.  Loop over all of the unresolved types in the translation
  // unit, resolving them with imports.
  for (TypeAliasDecl *TA : TU->getUnresolvedTypes()) {
    if (TypeAliasDecl *Result =
          Binder.TU->lookupGlobalType(TA->getName(),
                                      NLKind::UnqualifiedLookup)) {
      assert(!TA->hasUnderlyingType() && "Not an unresolved type");
      // Update the decl we already have to be the correct type.
      TA->setTypeAliasLoc(Result->getTypeAliasLoc());
      TA->setUnderlyingType(Result->getUnderlyingType());
      continue;
    }
    
    Binder.diagnose(TA->getLocStart(), diag::use_undeclared_type,
                    TA->getName());
    
    TA->setUnderlyingType(ErrorType::get(TU->Ctx));
  }

  // Loop over all the unresolved scoped types in the translation
  // unit, resolving them if possible.
  for (auto BaseAndType : TU->getUnresolvedScopedTypes()) {
    BoundScope Scope = Binder.bindScopeName(BaseAndType.first,
                                            BaseAndType.first->getName(),
                                        BaseAndType.first->getTypeAliasLoc());
    if (!Scope) continue;

    Identifier Name = BaseAndType.second->getName();
    SourceLoc NameLoc = BaseAndType.second->getTypeAliasLoc();

    TypeAliasDecl *Alias = nullptr;
    if (auto Module = Scope.dyn_cast<const ImportedModule*>())
      Alias = Module->second->lookupType(Module->first, Name,
                                         NLKind::QualifiedLookup);

    if (Alias) {
      BaseAndType.second->setUnderlyingType(Alias->getAliasType());
    } else {
      Binder.diagnose(NameLoc, diag::invalid_member_type,
                      Name, BaseAndType.first->getName());
      BaseAndType.second->setUnderlyingType(Binder.Context.TheErrorType);
    }
  }

  NameBinder *NBPtr = &Binder;
  auto BinderBlock = ^(Expr *E, WalkOrder Order, WalkContext const&) {
    return BindNames(E, Order, *NBPtr);
  };
  
  // Now that we know the top-level value names, go through and resolve any
  // UnresolvedDeclRefExprs that exist.
  for (unsigned i = 0, e = TU->Body->getNumElements(); i != e; ++i) {
    BraceStmt::ExprStmtOrDecl Elt = TU->Body->getElement(i);
    if (Decl *D = Elt.dyn_cast<Decl*>()) {
      bindNamesInDecl(D, BinderBlock);
    } else if (Stmt *S = Elt.dyn_cast<Stmt*>()) {
      Elt = S->walk(BinderBlock);
    } else {
      Elt = Elt.get<Expr*>()->walk(BinderBlock);
    }
    
    // Fill in null results with a dummy expression.
    if (Elt.isNull())
      Elt = new (TU->Ctx) TupleExpr(SourceLoc(), 0, 0, 0, SourceLoc(),
                                    TypeJudgement(TupleType::getEmpty(TU->Ctx),
                                                  ValueKind::RValue));
    TU->Body->setElement(i, Elt);
  }

  TU->ASTStage = TranslationUnit::NameBound;
  verify(TU);
}

