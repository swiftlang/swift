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

NullablePtr<const AccessNote> AccessNotes::lookup(ValueDecl *VD) const {
  assert(VD != nullptr);

  auto lookupVD = VD;
  if (auto accessor = dyn_cast<AccessorDecl>(VD))
    VD = accessor->getStorage();

  auto lookupName = lookupVD->getName();

  // FIXME: parseDeclName() doesn't handle the special `subscript` name.
  // Fixing this without affecting existing uses in import-as-member will need
  // a bit of work. Hack around this by changing to a normal identifier.
  if (isa<SubscriptDecl>(lookupVD) &&
      lookupName.getBaseName() == DeclBaseName::createSubscript()) {
    ASTContext &ctx = lookupVD->getASTContext();
    lookupName = DeclName(ctx, ctx.getIdentifier("subscript"),
                          lookupName.getArgumentNames());
  }

  const std::vector<AccessNote> *notesToSearch = &notes;

  // If nested, look at the parent context's notes.
  if (auto parent = lookupVD->getDeclContext()->getSelfNominalTypeDecl()) {
    if (auto parentNote = lookup(parent))
      notesToSearch = &(parentNote.get()->members);
    else
      return nullptr;
  }

  auto iter = llvm::find_if(*notesToSearch, [&](const AccessNote &note) -> bool {
    return lookupName.matchesRef(note.name);
  });
  return NullablePtr<const AccessNote>(iter == notesToSearch->end() ? nullptr : &*iter);
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
  os.indent(indent) << "(note name='" << name << "'";
  if (name.getBaseName().isSpecial())
    os << " is_special_name";

  if (ObjC)
    os << " objc=" << *ObjC;
  if (ObjCName)
    os << " objc_name='" << *ObjCName << "'";
  if (Dynamic)
    os << " dynamic=" << *Dynamic;

  if (!members.empty()) {
    os << "\n";
    os.indent(indent + 2) << "(members";
    for (const auto &member : members) {
      os << "\n";
      member.dump(os, indent + 4);
    }
    os << ")";
  }

  os << ")";
}

}

LLVM_YAML_DECLARE_SCALAR_TRAITS(swift::DeclName, QuotingType::Single)
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
using DeclName = swift::DeclName;
using ObjCSelector = swift::ObjCSelector;

void ScalarTraits<DeclName>::output(const DeclName &name, void *ctxPtr,
                                    raw_ostream &os) {
  name.print(os, /*skipEmptyArgumentNames=*/false);
}

StringRef ScalarTraits<DeclName>::input(StringRef str, void *ctxPtr,
                                        DeclName &name) {
  ASTContext &ctx = *static_cast<ASTContext *>(ctxPtr);
  name = parseDeclName(ctx, str);
  return name ? "" : "invalid declaration name";
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

  io.mapOptional("Members", note.members);
}

StringRef MappingTraits<AccessNote>::validate(IO &io, AccessNote &note) {
  if (!note.ObjC.getValueOr(true) && note.ObjCName.hasValue())
    return "cannot have an 'ObjCName' if 'ObjC' is false";

  return "";
}

void MappingTraits<AccessNotes>::mapping(IO &io, AccessNotes &notes) {
  io.mapRequired("Reason", notes.reason);
  io.mapRequired("Notes", notes.notes);
}

}
}
