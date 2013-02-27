//===-- Completion.cpp - Completion engine for swift immediate mode -------===//
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
// This module provides completions to the immediate mode environment.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-completion"

#include "Completion.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Sema/Lookup.h"

using namespace swift;

static bool isIdentifier(char c) {
  return isalnum(c) || c == '_';
}

struct CompletionContext {
  enum class Kind {
    Invalid,
    Unqualified,
    Qualified
  } Kind;
  
  union {
    struct {
      DeclContext *dc;
      SourceLoc loc;
    } Unqualified;
    struct {
      TypeBase *baseTy;
    } Qualified;
  };
  
  CompletionContext(const CompletionContext &) = default;
  CompletionContext(CompletionContext &&) = default;
  CompletionContext &operator=(const CompletionContext &) = default;
  CompletionContext &operator=(CompletionContext &&) = default;
  
  CompletionContext() : Kind(Kind::Invalid) {}
  
  CompletionContext(DeclContext *dc, SourceLoc loc)
    : Kind(Kind::Unqualified), Unqualified{dc, loc} {
  }
  
  CompletionContext(Type baseTy)
    : Kind(Kind::Qualified), Qualified{baseTy.getPointer()} {
  }
  
  explicit operator bool() { return Kind != Kind::Invalid; }
  
  DeclContext *getDeclContext() const {
    assert(Kind == Kind::Unqualified);
    return Unqualified.dc;
  }
  SourceLoc getLoc() const {
    assert(Kind == Kind::Unqualified);
    return Unqualified.loc;
  }
  
  Type getBaseType() const {
    assert(Kind == Kind::Qualified);
    return Qualified.baseTy;
  }
};

static SourceLoc getTUEndLoc(TranslationUnit *TU) {
  return TU->Decls.empty()
    ? SourceLoc()
    : TU->Decls.back()->getSourceRange().End;
}

static void findBalancedBracket(char const *begin,
                                char const *&p,
                                char open,
                                char close) {
  unsigned depth = 1;
  if (p == begin) {
    --p;
    return;
  }
  do {
    --p;
    if (*p == close)
      ++depth;
    else if (*p == open)
      --depth;
  } while (depth != 0 && p > begin);
  
  --p;
}

static CompletionContext getCompletionContextFromDotExpression(
                                                           TranslationUnit *TU,
                                                           DeclContext *dc,
                                                           StringRef expr) {
  char const *begin = expr.begin(), *p = expr.end()-1, *end = expr.end();
  
  // Walk backward through identifier '.' identifier '.' ...
  while (true) {
    while (p >= begin && isspace(*p))
      --p;
    if (p < begin)
      break;

    bool requireIdentifier = true;
    if (*p == ')') {
      findBalancedBracket(begin, p, '(', ')');
      requireIdentifier = false;
    } else if (*p == ']') {
      findBalancedBracket(begin, p, '[', ']');
      requireIdentifier = false;
    } else if (*p == '>') {
      findBalancedBracket(begin, p, '<', '>');
      requireIdentifier = true;
    }
    
    while (p >= begin && isspace(*p))
      --p;
    if (p < begin)
      break;
    if (requireIdentifier && !isIdentifier(*p))
      break;
    
    while (p >= begin && isIdentifier(*p)) {
      --p;
    }
    
    
    while (p >= begin && isspace(*p))
      --p;
    if (p < begin || *p != '.')
      break;
    --p;
  }
  ++p;
  
  // Try to parse and typecheck the thing we found as an expression.
  StringRef exprPart = StringRef(p, end - p);
  DEBUG(llvm::dbgs() << "\ncompletion context string: " << exprPart << "\n");

  Expr *parsedExpr = parseCompletionContextExpr(TU, exprPart);
  
  // If we couldn't parse, give up.
  if (!parsedExpr)
    return CompletionContext();

  DEBUG(llvm::dbgs() << "\nparsed:\n";
        parsedExpr->print(llvm::dbgs());
        llvm::dbgs() << "\n");
  
  // Try to typecheck the expression.
  if (!typeCheckCompletionContextExpr(TU, parsedExpr))
    return CompletionContext();
  
  DEBUG(llvm::dbgs() << "\ntypechecked:\n";
        parsedExpr->print(llvm::dbgs());
        llvm::dbgs() << "\n");
  
  // Use the type as the context for qualified lookup.
  return CompletionContext(parsedExpr->getType());
}

/// Determine the DeclContext, lookup kind, and starting prefix in which to
/// perform a completion given an initial DeclContext and user-entered string.
static CompletionContext getCompletionContext(DeclContext *dc,
                                              StringRef &prefix) {
  // Find the TranslationUnit.
  DeclContext *tuDC = dc;
  while (!isa<TranslationUnit>(tuDC))
    tuDC = dc->getParent();
  TranslationUnit *TU = cast<TranslationUnit>(tuDC);
  
  SourceLoc tuEndLoc = getTUEndLoc(TU);
  
  if (prefix.empty())
    return CompletionContext(dc, tuEndLoc);
  
  if (isIdentifier(prefix.back())) {
    // If the character preceding us looks like a named identifier character,
    // walk backward to find as much of a name as we can.
    char const *p = prefix.end(), *end = p, *begin = prefix.begin();
    
    for (; p != begin; --p) {
      if (!isIdentifier(*(p-1))) {
        prefix = StringRef(p, end - p);
        break;
      }
    }
    
    // See if we were preceded by a dot.
    while (p >= begin && isspace(*--p));

    if (p >= begin && *p == '.') {
      // Try to figure out a qualified lookup context from the expression
      // preceding the dot.
      return getCompletionContextFromDotExpression(TU, dc,
                                                   StringRef(begin, p - begin));
    }
    
    // If there was no dot, do unqualified completion on the name.
    return CompletionContext(dc, tuEndLoc);
  } else if (prefix.back() == '.') {
    // Try to figure out a qualified lookup context from the expression
    // preceding the dot.
    StringRef beforeDot = prefix.slice(0, prefix.size() - 1);
    prefix = StringRef();
    return getCompletionContextFromDotExpression(TU, dc, beforeDot);
  } else if (Identifier::isOperatorChar(prefix.back())) {
    // If the character preceding us looks like an operator character,
    // walk backward to find as much of an operator name as we can.
    for (char const *p = prefix.end(), *end = p, *begin = prefix.begin();
         p != begin;
         --p) {
      if (!Identifier::isOperatorChar(*(p-1))) {
        prefix = StringRef(p, end - p);
        return CompletionContext(dc, tuEndLoc);
      }
    }
    
    // Operators only appear unqualified.
    return CompletionContext(dc, tuEndLoc);
  }

  // Complete everything at the top level.
  prefix = StringRef();
  return CompletionContext(dc, tuEndLoc);
}

/// Build completions by doing visible decl lookup from a context.
class CompletionLookup : swift::VisibleDeclConsumer,
                         clang::VisibleDeclConsumer
{
public:
  CompletionContext Context;
  llvm::StringRef Prefix;
  llvm::StringSet<> Results;
  Optional<StringRef> Root;
  
  void updateRoot(StringRef S) {
    if (!Root) {
      Root = S;
      return;
    }
    
    size_t len = 0;
    for (char const *r = Root->begin(), *s = S.begin();
         r != Root->end() && s != S.end();
         ++r, ++s) {
      if (*r == *s)
        ++len;
      else
        break;
    }
    Root = StringRef(Root->data(), len);
  }
  
  bool shouldCompleteDecl(ValueDecl *vd) {
    // Don't complete nameless values.
    if (vd->getName().empty())
      return false;
    
    // Don't complete constructors, destructors, or subscripts, since references
    // to them can't be spelled.
    // FIXME: except for super.constructor!
    if (isa<ConstructorDecl>(vd))
      return false;
    if (isa<DestructorDecl>(vd))
      return false;
    if (isa<SubscriptDecl>(vd))
      return false;
    
    // Don't complete class methods of instances.
    if (Context.Kind == CompletionContext::Kind::Qualified)
      if (auto *fd = dyn_cast<FuncDecl>(vd))
        if (fd->isStatic() && !Context.getBaseType()->is<MetaTypeType>())
          return false;
    
    return true;
  }
  
  void addCompletionString(llvm::StringRef name) {
    if (!name.startswith(Prefix))
      return;

    if (Results.insert(name))
      updateRoot(name);
  }
  
  // Implement swift::VisibleDeclConsumer
  void foundDecl(ValueDecl *vd) override {
    if (!shouldCompleteDecl(vd))
      return;
    StringRef name = vd->getName().get();

    addCompletionString(name);
  }
  
  // Implement clang::VisibleDeclConsumer
  void FoundDecl(clang::NamedDecl *ND, clang::NamedDecl *Hiding,
                 clang::DeclContext *Ctx,
                 bool InBaseClass) override {
    StringRef name = ND->getName();
    
    addCompletionString(name);
  }
  
  CompletionLookup(CompletionContext context, StringRef prefix)
    : Context(context), Prefix(prefix)
  {
    assert(context && "invalid completion lookup context!");
    
    if (context.Kind == CompletionContext::Kind::Unqualified) {
      lookupVisibleDecls(*this, context.getDeclContext(), context.getLoc());

      // TODO: Integrate Clang LookupVisibleDecls with Swift LookupVisibleDecls.
      // Doing so now makes REPL interaction too slow.
      if (!context.getDeclContext()->getASTContext().hasModuleLoader())
        return;
      
      ClangImporter &clangImporter
        = static_cast<ClangImporter&>(
                  context.getDeclContext()->getASTContext().getModuleLoader());
      clangImporter.lookupVisibleDecls(*this);
    } else {
      lookupVisibleDecls(*this, context.getBaseType());
    
      // Add the special qualified keyword 'metatype' so that, for example,
      // 'Int.metatype' can be completed.
      addCompletionString("metatype");
    }
  }
};
  
StringRef Completions::allocateCopy(StringRef s) {
  size_t size = s.size();
  char *copy = static_cast<char*>(strings->Allocate(size + 1, 1));
  memcpy(copy, s.data(), size);
  copy[size] = '\0';
  return StringRef(copy, size);
}

Completions::Completions(DeclContext *dc, StringRef prefix)
  : strings(new llvm::BumpPtrAllocator()),
    enteredLength(0),
    rootLength(0),
    currentStem((size_t)-1)
{
  Type lookupType;
  CompletionContext context = getCompletionContext(dc, prefix);
  enteredLength = prefix.size();
  
  if (!context) {
    rootLength = 0;
    state = CompletionState::Empty;
    return;
  }

  CompletionLookup lookup(context, prefix);

  if (lookup.Results.empty()) {
    rootLength = 0;
    state = CompletionState::Empty;
    return;
  }
  
  rootLength = lookup.Root->size();

  assert(rootLength >= enteredLength && "completions don't match prefix?!");
  for (auto &c : lookup.Results) {
    completions.push_back(allocateCopy(c.getKey()));
  }
  std::sort(completions.begin(), completions.end(),
            [](StringRef a, StringRef b) { return a < b; });
  
  if (lookup.Results.size() == 1) {
    state = CompletionState::Unique;
    return;
  }
  state = CompletionState::CompletedRoot;
  return;
}
  
StringRef Completions::getRoot() const {
  if (completions.empty())
    return StringRef();
  return StringRef(completions[0].data() + enteredLength,
                   rootLength - enteredLength);
}
  
StringRef Completions::getPreviousStem() const {
  if (currentStem == (size_t)-1 || completions.empty())
    return StringRef();
  StringRef s = completions[currentStem];
  return StringRef(s.data() + rootLength, s.size() - rootLength);
}
  
StringRef Completions::getNextStem() {
  if (completions.empty())
    return StringRef();
  currentStem += 1;
  if (currentStem >= completions.size())
    currentStem = 0;
  StringRef s = completions[currentStem];
  return StringRef(s.data() + rootLength, s.size() - rootLength);
}

void Completions::reset() {
  if (isValid()) {
    state = CompletionState::Invalid;
    strings.reset();
    completions.clear();
  }
}