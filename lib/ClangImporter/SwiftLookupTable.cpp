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

bool SwiftLookupTable::matchesContext(clang::DeclContext *foundContext,
                                      clang::DeclContext *requestedContext) {
  /// If the requested context was null, we match.
  if (!requestedContext)
    return true;

  // If the contexts match, we match.
  if (foundContext == requestedContext)
    return true;

  // If we found something in an Objective-C protocol to which a class
  // conforms, we match.
  if (auto objcProto = dyn_cast<clang::ObjCProtocolDecl>(foundContext)) {
    if (auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(requestedContext)) {
      return objcClass->ClassImplementsProtocol(objcProto,
                                                /*lookupCategory=*/true);
    }
  }

  return false;
}

/// Determine whether the new declarations matches an existing declaration.
static bool matchesExistingDecl(clang::Decl *decl, clang::Decl *existingDecl) {
  // If the canonical declarations are equivalent, we have a match.
  if (decl->getCanonicalDecl() == existingDecl->getCanonicalDecl()) {
    return true;
  }

  return false;
}

void SwiftLookupTable::addEntry(DeclName name, clang::NamedDecl *decl,
                                clang::DeclContext *effectiveContext) {
  clang::DeclContext *context = effectiveContext->getPrimaryContext();

  // First, check whether there is already a full name entry.
  auto knownFull = FullNameTable.find(name);
  if (knownFull == FullNameTable.end()) {
    // We didn't have a full name entry, so record that in the base
    // name table.
    BaseNameTable[name.getBaseName()].push_back(name);

    // Insert the entry into the full name table. We're done.
    FullTableEntry newEntry;
    newEntry.Context = context;
    newEntry.Decls.push_back(decl);
    (void)FullNameTable.insert({name, { newEntry }});
    return;
  }

  // Check whether there is already an entry with the same context.
  auto &fullEntries = knownFull->second;
  for (auto &fullEntry : fullEntries) {
    if (fullEntry.Context == context) {
      // Check whether this entry matches any existing entry.
      for (auto existingDecl : fullEntry.Decls) {
        if (matchesExistingDecl(decl, existingDecl)) return;
      }

      fullEntry.Decls.push_back(decl);
      return;
    }
  }

  // This is a new context for this name. Add it.
  FullTableEntry newEntry;
  newEntry.Context = context;
  newEntry.Decls.push_back(decl);
  fullEntries.push_back(newEntry);
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
  // Dump the base name -> full name mappings.
  SmallVector<Identifier, 4> baseNames;
  for (const auto &entry : BaseNameTable) {
    baseNames.push_back(entry.first);
  }
  std::sort(baseNames.begin(), baseNames.end(),
            [&](Identifier x, Identifier y) {
              return x.compare(y) < 0;
            });
  llvm::errs() << "Base -> full name mappings:\n";
  for (auto baseName : baseNames) {
    llvm::errs() << "  " << baseName.str() << " --> ";
    const auto &fullNames = BaseNameTable.find(baseName)->second;
    interleave(fullNames.begin(), fullNames.end(),
               [](DeclName fullName) {
                 llvm::errs() << fullName;
               },
               [] {
                 llvm::errs() << ", ";
               });
    llvm::errs() << "\n";
  }
  llvm::errs() << "\n";

  // Dump the full name -> full table entry mappings.
  SmallVector<DeclName, 4> fullNames;
  for (const auto &entry : FullNameTable) {
    fullNames.push_back(entry.first);
  }
  std::sort(fullNames.begin(), fullNames.end(),
            [](DeclName x, DeclName y) {
              return x.compare(y) < 0;
            });
  llvm::errs() << "Full name -> entry mappings:\n";
  for (auto fullName : fullNames) {
    llvm::errs() << "  " << fullName << ":\n";
    const auto &fullEntries = FullNameTable.find(fullName)->second;
    for (const auto &fullEntry : fullEntries) {
      llvm::errs() << "    ";
      if (fullEntry.Context->isTranslationUnit()) {
        llvm::errs() << "TU";
      } else if (auto named = dyn_cast<clang::NamedDecl>(fullEntry.Context)) {
        printName(named, llvm::errs());
      } else {
        llvm::errs() << "<unknown>";
      }
      llvm::errs() << ": ";

      interleave(fullEntry.Decls.begin(), fullEntry.Decls.end(),
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
