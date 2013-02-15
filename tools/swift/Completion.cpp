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

#include "Completion.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookup.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

static bool isIdentifier(char c) {
  return isalnum(c) || c == '_';
}
  
static StringRef getCompletionContext(DeclContext* &dc,
                                      StringRef prefix) {
  // For now, we just walk backward from the end of the prefix until we run
  // out of identifier chars. In the future we should walk back through dotted
  // paths and maybe even matching brackets and type-check the subexpression
  // to find our completion context.
  for (char const *p = prefix.end(), *end = p; p != prefix.begin(); --p) {
    if (!isIdentifier(*(p-1)))
      return StringRef(p, end - p);
  }
  
  return prefix;
}

/// Build completions by doing visible decl lookup from a context.
class CompletionLookup : VisibleDeclConsumer {
public:
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
  
  void foundDecl(ValueDecl *vd) override {
    StringRef name = vd->getName().get();
    if (!name.startswith(Prefix))
      return;

    if (Results.insert(name)) {
      updateRoot(name);
    }
  }
  
  CompletionLookup(DeclContext *dc, SourceLoc loc, StringRef prefix)
    : Prefix(prefix)
  {
    lookupVisibleDecls(*this, dc, loc);
  }
};
  
StringRef Completions::allocateCopy(StringRef s) {
  char *copy = static_cast<char*>(strings->Allocate(s.size() + 1, 1));
  memcpy(copy, s.data(), s.size());
  copy[s.size()] = '\0';
  return StringRef(copy, s.size());
}

Completions::Completions(DeclContext *dc, SourceLoc loc, StringRef prefix)
  : strings(new llvm::BumpPtrAllocator()),
    enteredLength(0),
    rootLength(0),
    currentStem((size_t)-1)
{
  StringRef completionPrefix = getCompletionContext(dc, prefix);
  enteredLength = completionPrefix.size();

  CompletionLookup lookup(dc, loc, completionPrefix);

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