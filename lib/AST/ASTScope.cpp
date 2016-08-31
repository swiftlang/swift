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
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/STLExtras.h"
using namespace swift;

/// Determine whether we should completely skip the given element in a
/// \c BraceStmt.
static bool shouldSkipBraceStmtElement(ASTNode element) {
  if (auto decl = element.dyn_cast<Decl *>())
    return isa<VarDecl>(decl);

  return false;
}

/// Determines whether the given AST node introduces a continuation.
static bool introducesContinuation(ASTNode element) {
  if (auto decl = element.dyn_cast<Decl *>()) {
    // Declarations in local contexts introduce continuations.
    return decl->getDeclContext()->isLocalContext();
  }

  if (auto stmt = element.dyn_cast<Stmt *>()) {
    // Guard statements introduce continuations.
    if (isa<GuardStmt>(stmt)) return true;

    return false;
  }

  return false;
}

/// Determine whether the given abstract storage declaration has accessors.
static bool hasAccessors(AbstractStorageDecl *asd) {
  switch (asd->getStorageKind()) {
  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithObservers:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::Computed:
  case AbstractStorageDecl::ComputedWithMutableAddress:
  case AbstractStorageDecl::InheritedWithObservers:
  case AbstractStorageDecl::StoredWithObservers:
    return true;

  case AbstractStorageDecl::Stored:
  case AbstractStorageDecl::StoredWithTrivialAccessors:
    return false;
  }
}

void ASTScope::expand() const {
  assert(!isExpanded() && "Already expanded the children of this node");
  ASTContext &ctx = getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

#ifndef NDEBUG
  auto verificationError = [&]() -> llvm::raw_ostream& {
    return llvm::errs() << "ASTScope verification error in source file '"
      << getSourceFile().getFilename()
      << "': ";
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
      this->print(out);
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

    // Create a child for the initializer expression, if present.
    if (auto child = createIfNeeded(this, patternEntry.getInit()))
      addChild(child);

    // Create children for the variables in the pattern, if needed.
    patternEntry.getPattern()->forEachVariable([&](VarDecl *var) {
      if (hasAccessors(var)) {
        if (auto varChild = createIfNeeded(this, var))
          addChild(varChild);
      }
    });

    // Create a child for the next pattern binding.
    if (auto child = createIfNeeded(this, patternBinding.decl))
      addChild(child);
    break;
  }

  case ASTScopeKind::BraceStmt: {
    // Find the first element that requires a scope.
    auto elements = braceStmt.stmt->getElements();
    for (unsigned i : range(braceStmt.nextElement, elements.size())) {
      braceStmt.nextElement = i + 1;

      // Skip this brace element if it's unnecessary.
      if (shouldSkipBraceStmtElement(elements[i])) continue;

      // Try to create the child. If it succeeds, we're done.
      if (auto child = createIfNeeded(this, elements[i])) {
        addChild(child);

        // If this element introduces a continuation, it's expansion will
        // handle the remainder of the brace statement.
        if (introducesContinuation(elements[i])) break;
      }
    }
    break;
  }

  case ASTScopeKind::LocalDeclaration:
    // Add the contents of the declaration itself.
    if (auto child = createIfNeeded(this, localDeclaration))
      addChild(child);
    break;

  case ASTScopeKind::IfStmt:
    // The first conditional clause or, failing that, the 'then' clause.
    if (!ifStmt->getCond().empty()) {
      addChild(new (ctx) ASTScope(this, ifStmt, 0));
    } else {
      if (auto thenChild = createIfNeeded(this, ifStmt->getThenStmt()))
        addChild(thenChild);
    }

    // Add the 'else' branch, if needed.
    if (auto elseChild = createIfNeeded(this, ifStmt->getElseStmt()))
      addChild(elseChild);

    break;

  case ASTScopeKind::ConditionalClause: {
    // If there's another conditional clause, add it as the child.
    unsigned nextIndex = conditionalClause.index + 1;
    if (nextIndex < conditionalClause.stmt->getCond().size()) {
      addChild(new (ctx) ASTScope(this, conditionalClause.stmt, nextIndex));
      break;
    }

    // There aren't any additional conditional clauses. Add the appropriate
    // nested scope based on the kind of statement.
    if (auto ifStmt = dyn_cast<IfStmt>(conditionalClause.stmt)) {
      if (auto child = createIfNeeded(this, ifStmt->getThenStmt()))
        addChild(child);
    } else if (auto whileStmt = dyn_cast<WhileStmt>(conditionalClause.stmt)) {
      if (auto child = createIfNeeded(this, whileStmt->getBody()))
        addChild(child);
    } else {
      // Note: guard statements have the continuation nested under the last
      // condition.
      assert(isa<GuardStmt>(conditionalClause.stmt) &&
             "unknown labeled conditional statement");
    }
    break;
  }

  case ASTScopeKind::GuardStmt:
    // Add a child to describe the guard condition.
    addChild(new (ctx) ASTScope(this, guard, 0));

    // Add a child for the 'guard' body, which always exits.
    if (auto bodyChild = createIfNeeded(this, guard->getBody()))
      addChild(bodyChild);

    break;

  case ASTScopeKind::RepeatWhileStmt:
    // Add a child for the loop body.
    if (auto bodyChild = createIfNeeded(this, repeatWhile->getBody()))
      addChild(bodyChild);

    // Add a child for the loop condition.
    if (auto conditionChild = createIfNeeded(this, repeatWhile->getCond()))
      addChild(conditionChild);

    break;

  case ASTScopeKind::ForEachStmt:
    // Add a child for the sequence.
    if (auto seqChild = createIfNeeded(this, forEach->getSequence()))
      addChild(seqChild);

    // Add a child describing the scope of the pattern.
    addChild(new (ctx) ASTScope(ASTScopeKind::ForEachPattern, this, forEach));
    break;

  case ASTScopeKind::ForEachPattern:
    // Add a child for the 'where' clause.
    if (auto whereChild = createIfNeeded(this, forEach->getWhere()))
      addChild(whereChild);

    // Add a child for the body.
    if (auto bodyChild = createIfNeeded(this, forEach->getBody()))
      addChild(bodyChild);

    break;

  case ASTScopeKind::DoCatchStmt:
    // Add a child for the body.
    if (auto bodyChild = createIfNeeded(this, doCatch->getBody()))
      addChild(bodyChild);

    // Add children for each of the 'catch' clauses.
    for (auto catchClause : doCatch->getCatches()) {
      if (auto catchChild = createIfNeeded(this, catchClause))
        addChild(catchChild);
    }
    break;

  case ASTScopeKind::CatchStmt:
    // Add a child for the guard expression, if there is one.
    if (auto guardChild = createIfNeeded(this, catchStmt->getGuardExpr()))
      addChild(guardChild);

    // Add a child for the catch body.
    if (auto bodyChild = createIfNeeded(this, catchStmt->getBody()))
      addChild(bodyChild);

    break;

  case ASTScopeKind::SwitchStmt:
    // Add a child for the subject expression.
    if (auto subjectChild = createIfNeeded(this, switchStmt->getSubjectExpr()))
      addChild(subjectChild);

    // Add children for each of the cases.
    for (auto caseStmt : switchStmt->getCases()) {
      if (auto caseChild = createIfNeeded(this, caseStmt))
        addChild(caseChild);
    }
    break;

  case ASTScopeKind::CaseStmt:
    // Add children for the items.
    for (auto &caseItem : caseStmt->getMutableCaseLabelItems()) {
      if (auto guardChild = createIfNeeded(this, caseItem.getGuardExpr()))
        addChild(guardChild);
    }

    // Add a child for the case body.
    if (auto bodyChild = createIfNeeded(this, caseStmt->getBody()))
      addChild(bodyChild);
    break;

  case ASTScopeKind::ForStmt:
    // The for statement encloses the scope introduced by its initializers.
    addChild(new (ctx) ASTScope(ASTScopeKind::ForStmtInitializer,
                                this, forStmt));
    break;

  case ASTScopeKind::ForStmtInitializer:
    // Add a child for the condition, if present.
    if (auto cond = forStmt->getCond()) {
      if (auto condChild = createIfNeeded(this, cond.get()))
        addChild(condChild);
    }

    // Add a child for the increment, if present.
    if (auto incr = forStmt->getIncrement()) {
      if (auto incrChild = createIfNeeded(this, incr.get()))
        addChild(incrChild);
    }

    // Add a child for the body.
    if (auto bodyChild = createIfNeeded(this, forStmt->getBody()))
      addChild(bodyChild);
    break;

  case ASTScopeKind::Accessors: {
    // Collect all of the explicitly-written accessors.
    SmallVector<FuncDecl *, 2> accessors;
    auto addAccessor = [&](FuncDecl *accessor) {
      if (!accessor) return;
      if (accessor->isImplicit()) return;
      if (accessor->getStartLoc().isInvalid()) return;

      accessors.push_back(accessor);
    };

    addAccessor(abstractStorageDecl->getGetter());
    addAccessor(abstractStorageDecl->getSetter());
    addAccessor(abstractStorageDecl->getMaterializeForSetFunc());
    if (abstractStorageDecl->hasAddressors()) {
      addAccessor(abstractStorageDecl->getAddressor());
      addAccessor(abstractStorageDecl->getMutableAddressor());
    }
    if (abstractStorageDecl->hasObservers()) {
      addAccessor(abstractStorageDecl->getDidSetFunc());
      addAccessor(abstractStorageDecl->getWillSetFunc());
    }

    // Sort the accessors by beginning source range.
    std::sort(accessors.begin(), accessors.end(),
      [&](FuncDecl *a1, FuncDecl *a2) {
        return ctx.SourceMgr.isBeforeInBuffer(a1->getStartLoc(),
                                              a2->getStartLoc());
      });

    // Add the accessor children.
    for (auto accessor : accessors) {
      if (auto accessorChild = createIfNeeded(this, accessor))
        addChild(accessorChild);
    }
    break;
  }
  }

  // Enumerate any continuation scopes associated with this parent.
  enumerateContinuationScopes(addChild);

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

/// Determine whether the given parent is a local declaration or is
/// directly descended from one.
static bool parentDirectDescendedFromLocalDeclaration(const ASTScope *parent,
                                                      const Decl *decl) {
  while (true) {
    switch (parent->getKind()) {
    case ASTScopeKind::AbstractFunctionParams:
    case ASTScopeKind::GenericParams:
    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::Accessors:
      // Keep looking.
      parent = parent->getParent();
      continue;

    case ASTScopeKind::LocalDeclaration:
      return (parent->getLocalDeclaration() == decl);

    case ASTScopeKind::SourceFile:
    case ASTScopeKind::AfterPatternBinding:
    case ASTScopeKind::BraceStmt:
    case ASTScopeKind::ConditionalClause:
    case ASTScopeKind::IfStmt:
    case ASTScopeKind::GuardStmt:
    case ASTScopeKind::RepeatWhileStmt:
    case ASTScopeKind::ForEachStmt:
    case ASTScopeKind::ForEachPattern:
    case ASTScopeKind::DoCatchStmt:
    case ASTScopeKind::CatchStmt:
    case ASTScopeKind::SwitchStmt:
    case ASTScopeKind::CaseStmt:
    case ASTScopeKind::ForStmt:
    case ASTScopeKind::ForStmtInitializer:
      // Not a direct descendant.
      return false;
    }
  }
}

/// Determine whether the given parent is a local declaration or is
/// directly descended from one.
static bool parentDirectDescendedFromAbstractStorageDecl(
              const ASTScope *parent,
              const AbstractStorageDecl *decl) {
  while (true) {
  switch (parent->getKind()) {
    case ASTScopeKind::AbstractFunctionParams:
    case ASTScopeKind::GenericParams:
      // Keep looking.
      parent = parent->getParent();
      continue;

    case ASTScopeKind::Accessors:
      return (parent->getAbstractStorageDecl() == decl);

    case ASTScopeKind::SourceFile:
    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::LocalDeclaration:
    case ASTScopeKind::AfterPatternBinding:
    case ASTScopeKind::BraceStmt:
    case ASTScopeKind::ConditionalClause:
    case ASTScopeKind::IfStmt:
    case ASTScopeKind::GuardStmt:
    case ASTScopeKind::RepeatWhileStmt:
    case ASTScopeKind::ForEachStmt:
    case ASTScopeKind::ForEachPattern:
    case ASTScopeKind::DoCatchStmt:
    case ASTScopeKind::CatchStmt:
    case ASTScopeKind::SwitchStmt:
    case ASTScopeKind::CaseStmt:
    case ASTScopeKind::ForStmt:
    case ASTScopeKind::ForStmtInitializer:
      // Not a direct descendant.
      return false;
  }
  }
}

ASTScope *ASTScope::createIfNeeded(const ASTScope *parent, Decl *decl) {
  if (!decl) return nullptr;

  // Implicit declarations don't have source information for name lookup.
  if (decl->isImplicit()) return nullptr;

  // Accessors are always nested within their abstract storage declaration.
  bool isAccessor = false;
  if (auto func = dyn_cast<FuncDecl>(decl)) {
    if (func->isAccessor()) {
      isAccessor = true;
      if (!parentDirectDescendedFromAbstractStorageDecl(
             parent, func->getAccessorStorageDecl()))
        return nullptr;
    }
  }

  // If this is a local declaration for which we have not yet produced a
  // local declaration scope, introduce that local declaration scope now.
  //
  // Note that pattern bindings are handled as a special case, because they
  // potentially introduce several levels of bindings.
  ASTContext &ctx = decl->getASTContext();
  bool inLocalContext = decl->getDeclContext()->isLocalContext();
  if (!isa<PatternBindingDecl>(decl) && !isa<AbstractStorageDecl>(decl) &&
      inLocalContext && !isAccessor &&
      !parentDirectDescendedFromLocalDeclaration(parent, decl)) {
    auto scope = new (ctx) ASTScope(ASTScopeKind::LocalDeclaration,
                                parent);
    scope->localDeclaration = decl;
    return scope;
  }

  // Local function to handle generic parameters.
  auto nextGenericParam =
      [&](GenericParamList *genericParams, Decl *decl) -> ASTScope * {
    if (!genericParams) return nullptr;

    unsigned index = (parent->getKind() == ASTScopeKind::GenericParams &&
                      parent->genericParams.decl == decl)
                        ? parent->genericParams.index + 1
                        : 0;
    if (index < genericParams->size())
      return new (ctx) ASTScope(parent, genericParams, decl, index);

    return nullptr;
  };

  // Create the inner scope.
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
    if (parent->getKind() != ASTScopeKind::AbstractFunctionParams ||
        parent->abstractFunctionParams.decl != decl) {
      if (auto scope = nextGenericParam(abstractFunction->getGenericParams(),
                                        abstractFunction))
        return scope;
    }

    // Figure out which parameter is next is the next one down.
    Optional<std::pair<unsigned, unsigned>> nextParameter;
    if (parent->getKind() == ASTScopeKind::AbstractFunctionParams &&
        parent->abstractFunctionParams.decl == decl) {
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
    unsigned entry = (parent->getKind() == ASTScopeKind::AfterPatternBinding &&
                      parent->patternBinding.decl == decl)
                       ? parent->patternBinding.entry + 1
                       : 0;
    if (entry < patternBinding->getPatternList().size())
      return new (ctx) ASTScope(parent, patternBinding, entry);

    return nullptr;
  }

  case DeclKind::Var:
  case DeclKind::Subscript: {
    auto asd = cast<AbstractStorageDecl>(decl);
    if (hasAccessors(asd))
      return new (ctx) ASTScope(parent, asd);
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

  case StmtKind::Return: {
    auto returnStmt = cast<ReturnStmt>(stmt);
    if (!returnStmt->hasResult()) return nullptr;

    return createIfNeeded(parent, returnStmt->getResult());
  }

  case StmtKind::Defer:
    return createIfNeeded(parent, cast<DeferStmt>(stmt)->getTempDecl());

  case StmtKind::If:
    return new (ctx) ASTScope(parent, cast<IfStmt>(stmt));

  case StmtKind::Guard:
    return new (ctx) ASTScope(parent, cast<GuardStmt>(stmt));

  case StmtKind::While: {
    // If there are no conditions, just create the body.
    auto whileStmt = cast<WhileStmt>(stmt);
    if (whileStmt->getCond().empty())
      return createIfNeeded(parent, whileStmt->getBody());

    // Create a node for the first conditional clause.
    return new (ctx) ASTScope(parent, whileStmt, 0);
  }

  case StmtKind::RepeatWhile:
    return new (ctx) ASTScope(parent, cast<RepeatWhileStmt>(stmt));

  case StmtKind::ForEach:
    return new (ctx) ASTScope(ASTScopeKind::ForEachStmt, parent,
                              cast<ForEachStmt>(stmt));

  case StmtKind::For:
    return new (ctx) ASTScope(ASTScopeKind::ForStmt, parent,
                              cast<ForStmt>(stmt));

  case StmtKind::Do:
    return createIfNeeded(parent, cast<DoStmt>(stmt)->getBody());

  case StmtKind::DoCatch:
    return new (ctx) ASTScope(parent, cast<DoCatchStmt>(stmt));

  case StmtKind::Catch:
    return new (ctx) ASTScope(parent, cast<CatchStmt>(stmt));

  case StmtKind::Switch:
    return new (ctx) ASTScope(parent, cast<SwitchStmt>(stmt));

  case StmtKind::Case:
    return new (ctx) ASTScope(parent, cast<CaseStmt>(stmt));

  case StmtKind::Break:
  case StmtKind::Continue:
  case StmtKind::Fallthrough:
  case StmtKind::IfConfig:
  case StmtKind::Fail:
  case StmtKind::Throw:
    // Nothing to do for these statements.
    return nullptr;
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

bool ASTScope::isContinuationScope() const {
  switch (getKind()) {
  case ASTScopeKind::SourceFile:
  case ASTScopeKind::TypeOrExtensionBody:
  case ASTScopeKind::GenericParams:
  case ASTScopeKind::AbstractFunctionParams:
  case ASTScopeKind::BraceStmt:
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::GuardStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::ForEachPattern:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::CatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::CaseStmt:
  case ASTScopeKind::ForStmt:
  case ASTScopeKind::ForStmtInitializer:
    // These node kinds never have a viable continuation.
    return false;

  case ASTScopeKind::AfterPatternBinding:
    // The last parameter binding in a local scope can have a continuation.
    return patternBinding.decl->getDeclContext()->isLocalContext() &&
      patternBinding.entry == patternBinding.decl->getNumPatternEntries() - 1;

  case ASTScopeKind::LocalDeclaration:
    // Local declarations can always have a continuation.
    return true;

  case ASTScopeKind::Accessors:
    // Local declarations can have a continuation.
    return abstractStorageDecl->getDeclContext()->isLocalContext();

  case ASTScopeKind::ConditionalClause:
    // The last conditional clause of a 'guard' statement can have a
    // continuation.
    return isa<GuardStmt>(conditionalClause.stmt) &&
      conditionalClause.index + 1 == conditionalClause.stmt->getCond().size();
  }
}

void ASTScope::enumerateContinuationScopes(
       llvm::function_ref<void(ASTScope *)> fn) const {
  // Only consider scopes that can be continuation scopes.
  if (!isContinuationScope()) return;

  // Look for the nearest enclosing BraceStmt; that's where we continue from.
  const ASTScope *continuation = getParent();
  while (true) {
    switch (continuation->getKind()) {
    case ASTScopeKind::SourceFile:
    case ASTScopeKind::IfStmt:
    case ASTScopeKind::RepeatWhileStmt:
    case ASTScopeKind::ForEachStmt:
    case ASTScopeKind::ForEachPattern:
    case ASTScopeKind::DoCatchStmt:
    case ASTScopeKind::CatchStmt:
    case ASTScopeKind::SwitchStmt:
    case ASTScopeKind::CaseStmt:
    case ASTScopeKind::ForStmt:
    case ASTScopeKind::ForStmtInitializer:
      // These scopes are hard barriers; if we hit one, there is nothing to
      // continue to.
      return;

    case ASTScopeKind::Accessors:
      // Non-local variables/subscripts are hard barriers; there's nothing
      // to continue to.
      if (!abstractStorageDecl->getDeclContext()->isLocalContext())
        return;

      SWIFT_FALLTHROUGH;

    case ASTScopeKind::TypeOrExtensionBody:
    case ASTScopeKind::AbstractFunctionParams:
    case ASTScopeKind::GenericParams:
    case ASTScopeKind::AfterPatternBinding:
    case ASTScopeKind::LocalDeclaration:
    case ASTScopeKind::ConditionalClause:
    case ASTScopeKind::GuardStmt:
      // Continue looking for the continuation parent.
      continuation = continuation->getParent();
      continue;

    case ASTScopeKind::BraceStmt:
      // Found it. We're done.
      break;
    }

    break;
  }
  assert(continuation->getKind() == ASTScopeKind::BraceStmt);

  // Find the next suitable child in the brace statement.
  auto continuationElements = continuation->braceStmt.stmt->getElements();
  for (unsigned i : range(continuation->braceStmt.nextElement,
                          continuationElements.size())) {
    continuation->braceStmt.nextElement = i + 1;

    // Skip this element if it's useless.
    if (shouldSkipBraceStmtElement(continuationElements[i])) continue;

    // Try to create this child.
    if (auto child = createIfNeeded(this, continuationElements[i])) {
      // List this child.
      fn(child);

      // If this child introduces a continuation itself, we're done.
      if (introducesContinuation(continuationElements[i])) return;
    }
  }
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
  case ASTScopeKind::IfStmt:
  case ASTScopeKind::ConditionalClause:
  case ASTScopeKind::GuardStmt:
  case ASTScopeKind::RepeatWhileStmt:
  case ASTScopeKind::ForEachStmt:
  case ASTScopeKind::ForEachPattern:
  case ASTScopeKind::DoCatchStmt:
  case ASTScopeKind::CatchStmt:
  case ASTScopeKind::SwitchStmt:
  case ASTScopeKind::CaseStmt:
  case ASTScopeKind::ForStmt:
  case ASTScopeKind::ForStmtInitializer:
    return getParent()->getASTContext();

  case ASTScopeKind::LocalDeclaration:
    return localDeclaration->getASTContext();

  case ASTScopeKind::Accessors:
    return abstractStorageDecl->getASTContext();
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

    // For an accessor, all of the parameters are implicit, so start them at
    // the start location of the accessor.
    if (isa<FuncDecl>(abstractFunctionParams.decl) &&
        cast<FuncDecl>(abstractFunctionParams.decl)->isAccessor())
      return SourceRange(abstractFunctionParams.decl->getLoc(), endLoc);

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
    return braceStmt.stmt->getSourceRange();

  case ASTScopeKind::LocalDeclaration:
    return SourceRange(localDeclaration->getStartLoc(),
                       getParent()->getSourceRange().End);

  case ASTScopeKind::IfStmt:
    return ifStmt->getSourceRange();

  case ASTScopeKind::ConditionalClause: {
    // Determine the start location, which is either the beginning of the next
    // conditional or something statement-specific.
    auto conditionals = conditionalClause.stmt->getCond();
    unsigned nextIndex = conditionalClause.index + 1;
    SourceLoc startLoc;
    if (nextIndex < conditionals.size())
      startLoc = conditionals[nextIndex].getStartLoc();

    SourceLoc endLoc;

    // For 'guard' statements, the conditional clause covers the continuation.
    if (auto guard = dyn_cast<GuardStmt>(conditionalClause.stmt)) {
      // If we didn't have a condition clause to start the new scope, use the
      // end of the guard statement itself.
      if (startLoc.isInvalid())
        startLoc = guard->getEndLoc();

      return SourceRange(startLoc, getParent()->getSourceRange().End);
    }

    // For 'if' statements, the conditional clause covers the 'then' branch.
    if (auto ifStmt = dyn_cast<IfStmt>(conditionalClause.stmt)) {
      // If we didn't have a conditional clause to start the new scope, use
      // the beginning of the 'then' clause.
      if (startLoc.isInvalid())
        startLoc = ifStmt->getThenStmt()->getStartLoc();

      return SourceRange(startLoc, ifStmt->getThenStmt()->getEndLoc());
    }

    // For 'while' statements, the conditional clause covers the body.
    auto whileStmt = cast<WhileStmt>(conditionalClause.stmt);
    // If we didn't have a conditional clause to start the new scope, use
    // the beginning of the body.
    if (startLoc.isInvalid())
      startLoc = whileStmt->getBody()->getStartLoc();

    return SourceRange(startLoc, whileStmt->getBody()->getEndLoc());
  }

  case ASTScopeKind::GuardStmt:
    return SourceRange(guard->getStartLoc(), getParent()->getSourceRange().End);

  case ASTScopeKind::RepeatWhileStmt:
    return repeatWhile->getSourceRange();

  case ASTScopeKind::ForEachStmt:
    return forEach->getSourceRange();

  case ASTScopeKind::ForEachPattern:
    // The scope of the pattern extends from the 'where' expression (if present)
    // until the end of the body.
    if (forEach->getWhere())
      return SourceRange(forEach->getWhere()->getStartLoc(),
                         forEach->getBody()->getEndLoc());

    // Otherwise, scope of the pattern covers the body.
    return forEach->getBody()->getSourceRange();

  case ASTScopeKind::DoCatchStmt:
    return doCatch->getSourceRange();

  case ASTScopeKind::CatchStmt:
    // The scope of the pattern extends from the 'where' (if present)
    // to the end of the body.
    if (catchStmt->getGuardExpr())
      return SourceRange(catchStmt->getWhereLoc(),
                         catchStmt->getBody()->getEndLoc());

    // Otherwise, the scope of the pattern encompasses the body.
    return catchStmt->getBody()->getSourceRange();

  case ASTScopeKind::SwitchStmt:
    return switchStmt->getSourceRange();

  case ASTScopeKind::CaseStmt:
    // The scope of the case statement begins at the first guard expression,
    // if there is one, and extends to the end of the body.
    // FIXME: Figure out what to do about multiple pattern bindings. We might
    // want a more restrictive rule in those cases.
    for (const auto &caseItem : caseStmt->getCaseLabelItems()) {
      if (auto guardExpr = caseItem.getGuardExpr())
        return SourceRange(guardExpr->getStartLoc(),
                           caseStmt->getBody()->getEndLoc());
    }

    // Otherwise, it covers the body.
    return caseStmt->getBody()->getSourceRange();

  case ASTScopeKind::ForStmt:
    return forStmt->getSourceRange();

  case ASTScopeKind::ForStmtInitializer:
    return SourceRange(forStmt->getFirstSemicolonLoc(), forStmt->getEndLoc());

  case ASTScopeKind::Accessors: {
    return abstractStorageDecl->getBracesRange();
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
    printAddress(braceStmt.stmt);
    printRange();
    break;

  case ASTScopeKind::LocalDeclaration:
    printScopeKind("LocalDeclaration");
    printAddress(localDeclaration);
    printRange();
    break;

  case ASTScopeKind::IfStmt:
    printScopeKind("IfStmt");
    printAddress(ifStmt);
    printRange();
    break;

  case ASTScopeKind::ConditionalClause:
    printScopeKind("ConditionalClause");
    printAddress(conditionalClause.stmt);
    out << " index " << conditionalClause.index;
    printRange();
    break;

  case ASTScopeKind::GuardStmt:
    printScopeKind("GuardStmt");
    printAddress(guard);
    printRange();
    break;

  case ASTScopeKind::RepeatWhileStmt:
    printScopeKind("RepeatWhileStmt");
    printAddress(repeatWhile);
    printRange();
    break;

  case ASTScopeKind::ForEachStmt:
    printScopeKind("ForEachStmt");
    printAddress(forEach);
    printRange();
    break;

  case ASTScopeKind::ForEachPattern:
    printScopeKind("ForEachPattern");
    printAddress(forEach);
    printRange();
    break;

  case ASTScopeKind::DoCatchStmt:
    printScopeKind("DoCatchStmt");
    printAddress(doCatch);
    printRange();
    break;

  case ASTScopeKind::CatchStmt:
    printScopeKind("CatchStmt");
    printAddress(catchStmt);
    printRange();
    break;

  case ASTScopeKind::SwitchStmt:
    printScopeKind("SwitchStmt");
    printAddress(switchStmt);
    printRange();
    break;

  case ASTScopeKind::CaseStmt:
    printScopeKind("CaseStmt");
    printAddress(caseStmt);
    printRange();
    break;

  case ASTScopeKind::ForStmt:
    printScopeKind("ForStmt");
    printAddress(forStmt);
    printRange();
    break;

  case ASTScopeKind::ForStmtInitializer:
    printScopeKind("ForStmtInitializer");
    printAddress(forStmt);
    printRange();
    break;

  case ASTScopeKind::Accessors:
    printScopeKind("Accessors");
    printAddress(abstractStorageDecl);
    out << " ";
    abstractStorageDecl->dumpRef(out);
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

