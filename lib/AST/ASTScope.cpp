//===--- ASTScope.cpp - Swift AST Scope -----------------------------------===//
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
//
// This file implements the ASTScope class and related functionality, which
// describes the scopes that exist within a Swift AST.
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTScope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/STLExtras.h"
using namespace swift;

void ASTScope::expand() const {
  assert(!isExpanded() && "Already expanded the children of this node");
  ASTContext &ctx = getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

#ifndef NDEBUG
  auto verificationError = [&]() -> llvm::raw_ostream& {
    return llvm::errs() << "ASTScope verification error in source file '"
      << getSourceFile().getFilename()
      << "':";
  };
#endif

  // Local function to add a child to the list of children.
  bool outOfOrder = false;
  bool previouslyEmpty = storedChildren.empty();
  auto addChild = [&](ASTScope *child) {
    assert(child->getParent() == this && "Wrong parent");

    // Check for containment of the child within the parent.
#ifndef NDEBUG
    if (!sourceMgr.rangeContains(getSourceRange(), child->getSourceRange())) {
      auto &out = verificationError() << "child not contained in its parent\n";
      out << "***Child node***\n";
      child->print(out);
      out << "***Parent node***\n";
      getParent()->print(out);
      abort();
    }
#endif

    // If there was a previous child, check it's source
    if (!storedChildren.empty()) {
      auto prevChild = storedChildren.back();
      SourceRange prevChildRange = prevChild->getSourceRange();
      SourceRange childRange = child->getSourceRange();

#ifndef NDEBUG
      // FIXME: Make sure that the we don't overlap the previous child.
#endif

      // If the previous child does not precede this one, we have an
      // out-of-order list of children.
      // FIXME: Decide whether fixing this up at end is a good interim step.
      if (sourceMgr.isBeforeInBuffer(childRange.Start, prevChildRange.End))
        outOfOrder = true;
    }

    // Add the child.
    storedChildren.push_back(child);
  };

  // Expand the children in the current scope.
  switch (kind) {
  case ASTScopeKind::SourceFile: {
    /// Add all of the new declarations to the list of children.
    for (Decl *decl : llvm::makeArrayRef(sourceFile.file->Decls)
                        .slice(sourceFile.nextElement)) {
      // Create a child node for this declaration.
      if (ASTScope *child = createIfNeeded(this, decl))
        addChild(child);
    }

    // Make sure we don't visit these children again.
    sourceFile.nextElement = sourceFile.file->Decls.size();
    break;
  }

  case ASTScopeKind::TypeOrExtensionBody:
    for (auto member : iterableDeclContext->getMembers()) {
      // Create a child node for this declaration.
      if (ASTScope *child = createIfNeeded(this, member))
        addChild(child);
    }
    break;

  case ASTScopeKind::GenericParams:
    // Create a child of the generic parameters, if needed.
    if (auto child = createIfNeeded(this, genericParams.decl))
      addChild(child);
    break;

  case ASTScopeKind::AbstractFunctionParams:
    // Create a child of the function parameters, which may eventually be
    // the function body.
    if (auto child = createIfNeeded(this, abstractFunctionParams.decl))
      addChild(child);
    break;

  case ASTScopeKind::AfterPatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];

    /// Create a child for the initializer expression, if present.
    if (auto child = createIfNeeded(this, patternEntry.getInit()))
      addChild(child);

    /// Create a child for the next pattern binding.
    if (auto child = createIfNeeded(this, patternBinding.decl))
      addChild(child);
    break;
  }

  case ASTScopeKind::BraceStmt: {
    if (braceStmt->getNumElements() > 0) {
      // Intrduce a scope for the first element.
      addChild(new (ctx) ASTScope(this, braceStmt, 0));
    }
    break;
  }

  case ASTScopeKind::BraceStmtElement: {
    // Find the first element that requires a scope. This updates the current
    // brace statement element in place, rather than creating additional nodes,
    // because clients cannot see and don't care about the unexpanded scope.
    auto elements = braceStmtElement.braceStmt->getElements();
    for (unsigned i : range(braceStmtElement.element, elements.size())) {
      braceStmtElement.element = i;

      // Try to create the child. If it succeeds, we're done.
      if (auto child = createIfNeeded(this, elements[i])) {
        addChild(child);
        break;
      }
    }
    break;
  }
  }

  // If this is the first time we've added children, notify the ASTContext
  // that there's a SmallVector that needs to be cleaned up.
  // FIXME: If we had access to SmallVector::isSmall(), we could do better.
  if (previouslyEmpty && !storedChildren.empty())
    getASTContext().addDestructorCleanup(storedChildren);

  // Anything but a SourceFile is considered "expanded" at this point; source
  // files can grow due to the REPL.
  if (kind != ASTScopeKind::SourceFile)
    parentAndExpanded.setInt(true);
}

bool ASTScope::isExpanded() const {
  // If the 'expanded' bit is set, we've expanded already.
  if (parentAndExpanded.getInt()) return true;

  // Source files are expanded when there are no new declarations to process.
  if (kind == ASTScopeKind::SourceFile &&
      sourceFile.nextElement == sourceFile.file->Decls.size())
    return true;

  return false;
}

/// Create the AST scope for a source file, which is the root of the scope
/// tree.
ASTScope *ASTScope::createRoot(SourceFile *sourceFile) {
  ASTContext &ctx = sourceFile->getASTContext();

  // Create the scope.
  ASTScope *scope = new (ctx) ASTScope(sourceFile, 0);
  scope->sourceFile.file = sourceFile;
  scope->sourceFile.nextElement = 0;

  return scope;
}

/// Find the parameter list and parameter index (into that list) corresponding
/// to the next parameter.
static Optional<std::pair<unsigned, unsigned>>
findNextParameter(AbstractFunctionDecl *func, unsigned listIndex,
                  unsigned paramIndex) {
  auto paramLists = func->getParameterLists();
  unsigned paramOffset = 1;
  while (listIndex < paramLists.size()) {
    auto currentList = paramLists[listIndex];

    // If there is a parameter in this list, return it.
    if (paramIndex + paramOffset < currentList->size()) {
      return std::make_pair(listIndex, paramIndex + paramOffset);
    }

    // Move on to the next list.
    ++listIndex;
    paramIndex = 0;
    paramOffset = 0;
  }

  return None;
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, Decl *decl) {
  if (!decl) return nullptr;

  // Implicit declarations don't have source information for name lookup.
  if (decl->isImplicit()) return nullptr;

  // Local function to handle generic parameters.
  ASTContext &ctx = decl->getASTContext();
  auto nextGenericParam =
      [&](GenericParamList *genericParams, Decl *decl) -> ASTScope * {
    if (!genericParams) return nullptr;

    unsigned index = parent->getKind() == ASTScopeKind::GenericParams
                       ? parent->genericParams.index + 1
                       : 0;
    if (index < genericParams->size())
      return new (ctx) ASTScope(parent, genericParams, decl, index);

    return nullptr;
  };

  // Create the inner scope.
  bool inLocalContext = decl->getDeclContext()->isLocalContext();
  switch (decl->getKind()) {
  case DeclKind::Import:
  case DeclKind::EnumCase:
  case DeclKind::PrecedenceGroup:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Module:
  case DeclKind::Param:
  case DeclKind::EnumElement:
  case DeclKind::IfConfig:
    // These declarations do not introduce scopes.
    return nullptr;

  case DeclKind::Extension:
    return new (ctx) ASTScope(parent, cast<ExtensionDecl>(decl));

  case DeclKind::Class:
  case DeclKind::Enum:
  case DeclKind::Struct: {
    auto nominal = cast<NominalTypeDecl>(decl);

    // If we have a generic type and our parent isn't describing our generic
    // parameters, build the generic parameter scope.
    if (auto scope = nextGenericParam(nominal->getGenericParams(), nominal))
      return scope;

    return new (ctx) ASTScope(parent, nominal);
  }

  case DeclKind::Protocol:
    return new (ctx) ASTScope(parent, cast<ProtocolDecl>(decl));

  case DeclKind::TypeAlias: {
    // If we have a generic typealias and our parent isn't describing our
    // generic parameters, build the generic parameter scope.
    auto typeAlias = cast<TypeAliasDecl>(decl);
    if (auto scope = nextGenericParam(typeAlias->getGenericParams(), typeAlias))
      return scope;

    // Typealiases don't introduce any other scopes.
    return nullptr;
  }

  case DeclKind::Func:
  case DeclKind::Constructor:
  case DeclKind::Destructor: {
    auto abstractFunction = cast<AbstractFunctionDecl>(decl);

    // If we have a generic function and our parent isn't describing our generic
    // parameters or function parameters, build the generic parameter scope.
    if (parent->getKind() != ASTScopeKind::AbstractFunctionParams) {
      if (auto scope = nextGenericParam(abstractFunction->getGenericParams(),
                                        abstractFunction))
        return scope;
    }

    // Figure out which parameter is next is the next one down.
    Optional<std::pair<unsigned, unsigned>> nextParameter;
    if (parent->getKind() == ASTScopeKind::AbstractFunctionParams) {
      assert(parent->abstractFunctionParams.decl == decl);
      nextParameter =
        findNextParameter(parent->abstractFunctionParams.decl,
                          parent->abstractFunctionParams.listIndex,
                          parent->abstractFunctionParams.paramIndex);
    } else if (abstractFunction->getParameterList(0)->size() > 0) {
      nextParameter = std::make_pair(0, 0);
    } else {
      nextParameter = findNextParameter(abstractFunction, 0, 0);
    }

    // If there is another parameter to visit, do so now.
    if (nextParameter)
      return new (ctx) ASTScope(parent, abstractFunction, nextParameter->first,
                                nextParameter->second);


    // Function body, if present.
    return createIfNeeded(parent, abstractFunction->getBody());
  }

  case DeclKind::PatternBinding: {
    // Only pattern bindings in local scope introduce scopes.
    if (!inLocalContext) return nullptr;

    // If there are no bindings, we're done.
    auto patternBinding = cast<PatternBindingDecl>(decl);

    // Find the next pattern binding.
    unsigned entry = parent->getKind() == ASTScopeKind::AfterPatternBinding
                       ? parent->patternBinding.entry + 1
                       : 0;
    if (entry < patternBinding->getPatternList().size())
      return new (ctx) ASTScope(parent, patternBinding, entry);

    return nullptr;
  }
  }

  // FIXME: Handle remaining cases.
  return nullptr;
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, Stmt *stmt) {
  if (!stmt) return nullptr;

  ASTContext &ctx = parent->getASTContext();
  switch (stmt->getKind()) {
  case StmtKind::Brace:
    return new (ctx) ASTScope(parent, cast<BraceStmt>(stmt));
  }

  // FIXME: Handle remaining cases.
  return nullptr;
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, Expr *expr) {
  // FIXME: Handle expressions.
  return nullptr;
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, ASTNode node) {
  if (auto decl = node.dyn_cast<Decl *>())
    return createIfNeeded(parent, decl);
  if (auto stmt = node.dyn_cast<Stmt *>())
    return createIfNeeded(parent, stmt);
  return createIfNeeded(parent, node.get<Expr *>());
}

ASTContext &ASTScope::getASTContext() const {
  switch (kind) {
  case ASTScopeKind::SourceFile:
    return sourceFile.file->getASTContext();

  case ASTScopeKind::TypeOrExtensionBody:
    return getParent()->getASTContext();

  case ASTScopeKind::GenericParams:
    return genericParams.decl->getASTContext();

  case ASTScopeKind::AbstractFunctionParams:
    return abstractFunctionParams.decl->getASTContext();

  case ASTScopeKind::AfterPatternBinding:
    return patternBinding.decl->getASTContext();

  case ASTScopeKind::BraceStmt:
  case ASTScopeKind::BraceStmtElement:
    return getParent()->getASTContext();
  }
}

SourceFile &ASTScope::getSourceFile() const {
  if (kind == ASTScopeKind::SourceFile)
    return *sourceFile.file;

  return getParent()->getSourceFile();
}

SourceRange ASTScope::getSourceRange() const {
  switch (kind) {
  case ASTScopeKind::SourceFile:
    if (auto bufferID = sourceFile.file->getBufferID()) {
      auto charRange = getASTContext().SourceMgr.getRangeForBuffer(*bufferID);
      return SourceRange(charRange.getStart(), charRange.getEnd());
    }

    return SourceRange();

  case ASTScopeKind::TypeOrExtensionBody:
    if (auto ext = dyn_cast<ExtensionDecl>(iterableDeclContext))
      return ext->getBraces();

    return cast<NominalTypeDecl>(iterableDeclContext)->getBraces();

  case ASTScopeKind::GenericParams:
    return SourceRange(genericParams.params->getParams()[genericParams.index]
                         ->getEndLoc(),
                       genericParams.decl->getEndLoc());

  case ASTScopeKind::AbstractFunctionParams: {
    SourceLoc endLoc = abstractFunctionParams.decl->getEndLoc();

    // For the 'self' parameter of a member function, use the start of the
    // first parameter list... or the 'deinit' keyword for deinitializers.
    // FIXME: Why oh why don't deinitializers have a parameter list?
    if (abstractFunctionParams.listIndex == 0 &&
        abstractFunctionParams.decl->getDeclContext()->isTypeContext()) {
      SourceLoc startLoc;
      if (isa<DestructorDecl>(abstractFunctionParams.decl)) {
        startLoc = abstractFunctionParams.decl->getNameLoc();
      } else {
        startLoc = abstractFunctionParams.decl->getParameterList(1)
                     ->getLParenLoc();
      }
      return SourceRange(startLoc, endLoc);
    }

    // Otherwise, find the end of this parameter.
    auto param = abstractFunctionParams.decl->getParameterList(
                   abstractFunctionParams.listIndex)
                     ->get(abstractFunctionParams.paramIndex);
    // FIXME: getLocForEndOfToken... but I can't use it here.
    return SourceRange(param->getEndLoc(), endLoc);
  }

  case ASTScopeKind::AfterPatternBinding: {
    const auto &patternEntry =
      patternBinding.decl->getPatternList()[patternBinding.entry];
    // The scope of the binding begins after the initializer (if there is one);
    // other, after the pattern itself.
    SourceLoc startLoc = patternEntry.getOrigInitRange().End;
    if (startLoc.isInvalid())
      startLoc = patternEntry.getPattern()->getSourceRange().End;

    // And extends to the end of the parent range.
    return SourceRange(startLoc, getParent()->getSourceRange().End);
  }

  case ASTScopeKind::BraceStmt:
    return braceStmt->getSourceRange();

  case ASTScopeKind::BraceStmtElement: {
    auto element =
      braceStmtElement.braceStmt->getElement(braceStmtElement.element);
    return SourceRange(element.getStartLoc(),
                       braceStmtElement.braceStmt->getEndLoc());
  }
  }
}

void ASTScope::expandAll() const {
  if (isExpanded()) return;
  expand();

  for (auto child : children())
    child->expandAll();
}

void ASTScope::print(llvm::raw_ostream &out, unsigned level,
                     bool lastChild) const {
  SourceManager &sourceMgr = getASTContext().SourceMgr;

  // Indent for levels 2+.
  if (level > 1) out.indent((level-1) * 2);

  // Print child marker and leading '-' for levels 1+.
  if (level > 0) {
    out << (lastChild ? '`' : '|') << '-';
  }

  // Local function to print the scope kind
  auto printScopeKind = [&](StringRef name) {
    out << name;
  };

  // Print the address of the node.
  auto printAddress = [&](const void *address) {
    out << " " << address;
  };

  // Print the source location of the node.
  auto printRange = [&]() {
    auto range = getSourceRange();
    auto startLineAndCol = sourceMgr.getLineAndColumn(range.Start);
    auto endLineAndCol = sourceMgr.getLineAndColumn(range.End);

    out << " [" << startLineAndCol.first << ":" << startLineAndCol.second
        << " - " << endLineAndCol.first << ":" << endLineAndCol.second << "]";
  };

  // Print the scope kind and any salient information.
  switch (kind) {
  case ASTScopeKind::SourceFile:
    printScopeKind("SourceFile");
    printAddress(sourceFile.file);
    out << " '" << sourceFile.file->getFilename() << "'";
    printRange();
    break;

  case ASTScopeKind::TypeOrExtensionBody: {
    printScopeKind("TypeOrExtensionBody");
    if (auto ext = dyn_cast<ExtensionDecl>(iterableDeclContext)) {
      printAddress(ext);
      out << " extension of '";
      if (auto typeRepr = ext->getExtendedTypeLoc().getTypeRepr())
        typeRepr->print(out);
      else
        ext->getExtendedType()->print(out);
      out << "'";
      printRange();
    } else {
      auto nominal = cast<NominalTypeDecl>(iterableDeclContext);
      printAddress(nominal);
      out << " '" << nominal->getName() << "'";
      printRange();
    }
    break;
  }

  case ASTScopeKind::GenericParams:
    printScopeKind("GenericParams");
    printAddress(genericParams.params);
    out << " param " << genericParams.index;
    printRange();
    break;

  case ASTScopeKind::AbstractFunctionParams:
    printScopeKind("AbstractFunctionParams");
    printAddress(abstractFunctionParams.decl);
    out << " " << abstractFunctionParams.decl->getFullName()
        << " param " << abstractFunctionParams.listIndex << ":"
        << abstractFunctionParams.paramIndex;
    printRange();
    break;

  case ASTScopeKind::AfterPatternBinding:
    printScopeKind("AfterPatternBinding");
    printAddress(patternBinding.decl);
    out << " entry " << patternBinding.entry;
    printRange();
    break;

  case ASTScopeKind::BraceStmt:
    printScopeKind("BraceStmt");
    printAddress(braceStmt);
    printRange();
    break;

  case ASTScopeKind::BraceStmtElement:
    printScopeKind("BraceStmtElement");
    printAddress(braceStmtElement.braceStmt);
    out << " element " << braceStmtElement.element;
    printRange();
    break;
  }

  // Was this scope expanded?
  out << (isExpanded() ? " expanded" : " unexpanded");

  out << "\n";

  // Print the children. In some cases, we can be "unexpanded" but still have
  // children.
  for (unsigned i : indices(storedChildren)) {
    storedChildren[i]->print(out, level + 1,
                             /*lastChild=*/i == storedChildren.size()-1);
  }
}

void ASTScope::dump() const {
  print(llvm::errs(), 0, false);
}

void *ASTScope::operator new(size_t bytes, const ASTContext &ctx,
                             unsigned alignment) {
  return ctx.Allocate(bytes, alignment);
}

