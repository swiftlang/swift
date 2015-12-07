//===--- SwiftLookupTable.cpp - Swift Lookup Table ------------------------===//
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
// This file implements support for Swift name lookup tables stored in Clang
// modules.
//
//===----------------------------------------------------------------------===//
#include "SwiftLookupTable.h"
#include "swift/Basic/STLExtras.h"
#include "clang/AST/DeclObjC.h"
using namespace swift;

/// Determine whether the new declarations matches an existing declaration.
static bool matchesExistingDecl(clang::Decl *decl, clang::Decl *existingDecl) {
  // If the canonical declarations are equivalent, we have a match.
  if (decl->getCanonicalDecl() == existingDecl->getCanonicalDecl()) {
    return true;
  }

  return false;
}

Optional<std::pair<SwiftLookupTable::ContextKind, StringRef>>
SwiftLookupTable::translateContext(clang::DeclContext *context) {
  // Translation unit context.
  if (context->isTranslationUnit())
    return std::make_pair(ContextKind::TranslationUnit, StringRef());

  // Tag declaration context.
  if (auto tag = dyn_cast<clang::TagDecl>(context)) {
    if (tag->getIdentifier())
      return std::make_pair(ContextKind::Tag, tag->getName());
    if (auto typedefDecl = tag->getTypedefNameForAnonDecl())
      return std::make_pair(ContextKind::Tag, typedefDecl->getName());
    return None;
  }

  // Objective-C class context.
  if (auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(context))
    return std::make_pair(ContextKind::ObjCClass, objcClass->getName());

  // Objective-C protocol context.
  if (auto objcProtocol = dyn_cast<clang::ObjCProtocolDecl>(context))
    return std::make_pair(ContextKind::ObjCProtocol, objcProtocol->getName());

  return None;
}

void SwiftLookupTable::addEntry(DeclName name, clang::NamedDecl *decl,
                                clang::DeclContext *effectiveContext) {
  // Translate the context.
  auto contextOpt = translateContext(effectiveContext);
  if (!contextOpt) return;
  auto context = *contextOpt;

  // Find the list of entries for this base name.
  auto &entries = LookupTable[name.getBaseName().str()];
  for (auto &entry : entries) {
    if (entry.Context == context) {
      // We have entries for this context.

      // Check whether this entry matches any existing entry.
      for (auto existingDecl : entry.Decls) {
        if (matchesExistingDecl(decl, existingDecl)) return;
      }

      // Add an entry to this context.
      entry.Decls.push_back(decl);
      return;
    }
  }

  // This is a new context for this name. Add it.
  FullTableEntry newEntry;
  newEntry.Context = context;;
  newEntry.Decls.push_back(decl);
  entries.push_back(newEntry);
}

static void printName(clang::NamedDecl *named, llvm::raw_ostream &out) {
  // If there is a name, print it.
  if (!named->getDeclName().isEmpty()) {
    // If we have an Objective-C method, print the class name along
    // with '+'/'-'.
    if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(named)) {
      out << (objcMethod->isInstanceMethod() ? '-' : '+') << '[';
      if (auto classDecl = objcMethod->getClassInterface()) {
        classDecl->printName(out);
        out << ' ';
      } else if (auto proto = dyn_cast<clang::ObjCProtocolDecl>(
                                objcMethod->getDeclContext())) {
        proto->printName(out);
        out << ' ';
      }
      named->printName(out);
      out << ']';
      return;
    }

    // If we have an Objective-C property, print the class name along
    // with the property name.
    if (auto objcProperty = dyn_cast<clang::ObjCPropertyDecl>(named)) {
      auto dc = objcProperty->getDeclContext();
      if (auto classDecl = dyn_cast<clang::ObjCInterfaceDecl>(dc)) {
        classDecl->printName(out);
        out << '.';
      } else if (auto categoryDecl = dyn_cast<clang::ObjCCategoryDecl>(dc)) {
        categoryDecl->getClassInterface()->printName(out);
        out << '.';
      } else if (auto proto = dyn_cast<clang::ObjCProtocolDecl>(dc)) {
        proto->printName(out);
        out << '.';
      }
      named->printName(out);
      return;
    }

    named->printName(out);
    return;
  }

  // If this is an anonymous tag declaration with a typedef name, use that.
  if (auto tag = dyn_cast<clang::TagDecl>(named)) {
    if (auto typedefName = tag->getTypedefNameForAnonDecl()) {
      printName(typedefName, out);
      return;
    }
  }
}

void SwiftLookupTable::dump() const {
  // Dump the base name -> full table entry mappings.
  SmallVector<StringRef, 4> baseNames;
  for (const auto &entry : LookupTable) {
    baseNames.push_back(entry.first);
  }
  llvm::array_pod_sort(baseNames.begin(), baseNames.end());
  llvm::errs() << "Base name -> entry mappings:\n";
  for (auto baseName : baseNames) {
    llvm::errs() << "  " << baseName << ":\n";
    const auto &entries = LookupTable.find(baseName)->second;
    for (const auto &entry : entries) {
      llvm::errs() << "    ";
      switch (entry.Context.first) {
      case ContextKind::TranslationUnit:
        llvm::errs() << "TU";
        break;

      case ContextKind::Tag:
      case ContextKind::ObjCClass:
      case ContextKind::ObjCProtocol:
        llvm::errs() << entry.Context.second;
      }
      llvm::errs() << ": ";

      interleave(entry.Decls.begin(), entry.Decls.end(),
                 [](clang::NamedDecl *decl) {
                   if (auto named = dyn_cast<clang::NamedDecl>(decl))
                     printName(named, llvm::errs());
                   else
                     decl->printName(llvm::errs());
                 },
                 [] {
                   llvm::errs() << ", ";
                 });
      llvm::errs() << "\n";
    }
  }
}
