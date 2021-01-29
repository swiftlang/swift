//===--- AccessNotes.cpp - Access Notes -------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Implements access notes, which allow certain modifiers or attributes to be
//  added to the declarations in a module.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AccessNotes.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"     // DeclContext::isModuleScopeContext()
#include "swift/Parse/Parser.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/YAMLTraits.h"

// FIXME: Copied from MiscDiagnostics--don't do that.
static llvm::Optional<swift::ObjCSelector>
parseObjCSelector(swift::ASTContext &ctx, llvm::StringRef string) {
  using namespace swift;

  // Find the first colon.
  auto colonPos = string.find(':');

  // If there is no colon, we have a nullary selector.
  if (colonPos == StringRef::npos) {
    if (string.empty() || !Lexer::isIdentifier(string)) return None;
    return ObjCSelector(ctx, 0, { ctx.getIdentifier(string) });
  }

  SmallVector<Identifier, 2> pieces;
  do {
    // Check whether we have a valid selector piece.
    auto piece = string.substr(0, colonPos);
    if (piece.empty()) {
      pieces.push_back(Identifier());
    } else {
      if (!Lexer::isIdentifier(piece)) return None;
      pieces.push_back(ctx.getIdentifier(piece));
    }

    // Move to the next piece.
    string = string.substr(colonPos+1);
    colonPos = string.find(':');
  } while (colonPos != StringRef::npos);

  // If anything remains of the string, it's not a selector.
  if (!string.empty()) return None;

  return ObjCSelector(ctx, pieces.size(), pieces);
}

namespace swift {

AccessNoteDeclName::AccessNoteDeclName()
  : parentNames(), name(), accessorKind(None) { }

AccessNoteDeclName::AccessNoteDeclName(ASTContext &ctx, StringRef str) {
  auto parsedName = parseDeclName(str);

  StringRef first, rest = parsedName.ContextName;
  while (!rest.empty()) {
    std::tie(first, rest) = rest.split('.');
    parentNames.push_back(ctx.getIdentifier(first));
  }

  if (parsedName.IsGetter)
    accessorKind = AccessorKind::Get;
  else if (parsedName.IsSetter)
    accessorKind = AccessorKind::Set;
  else
    accessorKind = None;

  name = parsedName.formDeclName(ctx);

  // FIXME: parseDeclName() doesn't handle the special `subscript` name.
  // Fixing this without affecting existing uses in import-as-member will need
  // a bit of work. Hack around the problem for this specific caller instead.
  if (name.getBaseName() == ctx.getIdentifier("subscript"))
    name = DeclName(ctx, DeclBaseName::createSubscript(),
                    name.getArgumentNames());
}

bool AccessNoteDeclName::matches(ValueDecl *VD) const {
  // These are normally just `VD` and `name`, but not for accessors.
  auto lookupVD = VD;

  // First, we check if the accessor-ness of `VD` matches the accessor-ness of
  // the name, and update `lookupVD` if necessary.
  if (auto accessor = dyn_cast<AccessorDecl>(VD)) {
    if (!accessorKind || *accessorKind != accessor->getAccessorKind())
      return false;
    lookupVD = accessor->getStorage();
  }
  else if (accessorKind.hasValue())
    return false;

  // Check that `name` matches `lookupVD`.
  if (!lookupVD->getName().matchesRef(name))
    return false;

  // The rest of this checks `parentNames` against the parents of `lookupVD`.

  ArrayRef<Identifier> remainingContextNames = parentNames;
  DeclContext *nextContext = lookupVD->getDeclContext();

  while (!nextContext->isModuleScopeContext()) {
    // If we've run out of names without reaching module scope, we've failed.
    if (remainingContextNames.empty())
      return false;

    Identifier contextName = remainingContextNames.back();

    // If the context is not a type (or extension), we can't name VD in an
    // access note and the match fails; if the name doesn't match, the match
    // fails too.
    auto contextType = nextContext->getSelfNominalTypeDecl();
    if (!contextType || contextType->getName() != contextName)
      return false;

    // Still checking. Move to the parent.
    remainingContextNames = remainingContextNames.drop_back();
    nextContext = contextType->getParent();
  }

  // If the context is module-scoped, we've succeeded if we're out of names, or
  // failed if we still have some names to go.
  return remainingContextNames.empty();
}

bool AccessNoteDeclName::empty() const {
  return !name;
}

void AccessNoteDeclName::print(llvm::raw_ostream &os) const {
  if (accessorKind)
    os << getAccessorLabel(*accessorKind) << "ter:";

  for (auto parentName : parentNames)
    os << parentName << '.';
  name.print(os, /*skipEmptyArgumentNames=*/false);
}

void AccessNoteDeclName::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

NullablePtr<const AccessNote> AccessNotes::lookup(ValueDecl *VD) const {
  assert(VD != nullptr);

  auto iter = llvm::find_if(notes, [&](const AccessNote &note) -> bool {
    return note.name.matches(VD);
  });

  return NullablePtr<const AccessNote>(iter == notes.end() ? nullptr : &*iter);
}

void AccessNotes::dump() const {
  dump(llvm::dbgs());
  llvm::dbgs() << "\n";
}
void AccessNote::dump() const {
  dump(llvm::dbgs());
  llvm::dbgs() << "\n";
}

void AccessNotes::dump(llvm::raw_ostream &os) const {
  os << "(access_notes reason='" << reason << "'";
  for (const auto &note : notes) {
    os << "\n";
    note.dump(os, /*indent=*/2);
  }
}

void AccessNote::dump(llvm::raw_ostream &os, int indent) const {
  os.indent(indent) << "(note name='";
  name.print(os);
  os << "'";

  if (name.name.getBaseName().isSpecial())
    os << " is_special_name";

  if (ObjC)
    os << " objc=" << *ObjC;
  if (ObjCName)
    os << " objc_name='" << *ObjCName << "'";
  if (Dynamic)
    os << " dynamic=" << *Dynamic;

  os << ")";
}

}

LLVM_YAML_DECLARE_SCALAR_TRAITS(swift::AccessNoteDeclName, QuotingType::Single);
LLVM_YAML_DECLARE_SCALAR_TRAITS(swift::ObjCSelector, QuotingType::Single);
LLVM_YAML_IS_SEQUENCE_VECTOR(swift::AccessNote)
LLVM_YAML_DECLARE_MAPPING_TRAITS(swift::AccessNotes)

// Not using macro to avoid validation issues.
template <> struct llvm::yaml::MappingTraits<swift::AccessNote> {
  static void mapping(IO &IO, swift::AccessNote &Obj);
  static StringRef validate(IO &IO, swift::AccessNote &Obj);
};

namespace swift {
llvm::Expected<AccessNotes>
AccessNotes::load(ASTContext &ctx, llvm::MemoryBuffer *buffer) {
  llvm::yaml::Input yamlIn(buffer->getBuffer(), (void *)&ctx);

  AccessNotes notes;
  yamlIn >> notes;

  if (yamlIn.error())
    return llvm::errorCodeToError(yamlIn.error());

  return notes;
}
}


namespace llvm {
namespace yaml {

using AccessNote = swift::AccessNote;
using AccessNotes = swift::AccessNotes;
using ASTContext = swift::ASTContext;
using AccessNoteDeclName = swift::AccessNoteDeclName;
using ObjCSelector = swift::ObjCSelector;

void ScalarTraits<AccessNoteDeclName>::
output(const AccessNoteDeclName &name, void *ctxPtr, raw_ostream &os) {
  name.print(os);
}

StringRef ScalarTraits<AccessNoteDeclName>::
input(StringRef str, void *ctxPtr, AccessNoteDeclName &name) {
  ASTContext &ctx = *static_cast<ASTContext *>(ctxPtr);
  name = AccessNoteDeclName(ctx, str);
  return name.empty() ? "invalid declaration name" : "";
}

void ScalarTraits<ObjCSelector>::output(const ObjCSelector &selector,
                                        void *ctxPtr, raw_ostream &os) {
  os << selector;
}

StringRef ScalarTraits<ObjCSelector>::input(StringRef str, void *ctxPtr,
                                            ObjCSelector &selector) {
  ASTContext &ctx = *static_cast<ASTContext *>(ctxPtr);

  if (auto sel = parseObjCSelector(ctx, str)) {
    selector = *sel;
    return "";
  }

  return "invalid selector";
}

void MappingTraits<AccessNote>::mapping(IO &io, AccessNote &note) {
  io.mapRequired("Name", note.name);
  io.mapOptional("ObjC", note.ObjC);
  io.mapOptional("Dynamic", note.Dynamic);
  io.mapOptional("ObjCName", note.ObjCName);
}

StringRef MappingTraits<AccessNote>::validate(IO &io, AccessNote &note) {
  if (note.ObjCName.hasValue()) {
    if (!note.ObjC)
      note.ObjC = true;
    else if (!*note.ObjC)
      return "cannot have an 'ObjCName' if 'ObjC' is false";
  }

  return "";
}

void MappingTraits<AccessNotes>::mapping(IO &io, AccessNotes &notes) {
  io.mapRequired("Reason", notes.reason);
  io.mapRequired("Notes", notes.notes);
}

}
}
