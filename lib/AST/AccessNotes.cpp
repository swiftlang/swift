//===--- AccessNotes.cpp - Access Notes -------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
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

static void
convertToErrorAndJoin(const llvm::SMDiagnostic &diag, void *Context) {
  auto newError = llvm::createStringError(std::errc::invalid_argument,
                                          "%s at line %d, column %d",
                                          diag.getMessage().bytes_begin(),
                                          diag.getLineNo(), diag.getColumnNo());

  auto &errors = *(llvm::Error*)Context;
  errors = llvm::joinErrors(std::move(errors), std::move(newError));
}

struct AccessNoteYAMLContext {
  ASTContext &ctx;
  std::set<std::string> unknownKeys;
};

llvm::Expected<AccessNotes>
AccessNotes::load(ASTContext &ctx, llvm::MemoryBuffer *buffer) {
  llvm::Error errors = llvm::Error::success();
  AccessNoteYAMLContext yamlCtx = { ctx, {} };

  llvm::yaml::Input yamlIn(buffer->getBuffer(), (void *)&yamlCtx,
                           convertToErrorAndJoin, &errors);

  AccessNotes notes;
  yamlIn >> notes;

  if (errors)
    return llvm::Expected<AccessNotes>(std::move(errors));

  notes.unknownKeys = std::move(yamlCtx.unknownKeys);

  return notes;
}
}


namespace llvm {
namespace yaml {

using AccessNote = swift::AccessNote;
using AccessNotes = swift::AccessNotes;
using AccessNoteYAMLContext = swift::AccessNoteYAMLContext;
using AccessNoteDeclName = swift::AccessNoteDeclName;
using ObjCSelector = swift::ObjCSelector;

void ScalarTraits<AccessNoteDeclName>::
output(const AccessNoteDeclName &name, void *ctxPtr, raw_ostream &os) {
  name.print(os);
}

StringRef ScalarTraits<AccessNoteDeclName>::
input(StringRef str, void *ctxPtr, AccessNoteDeclName &name) {
  auto &yamlCtx = *static_cast<swift::AccessNoteYAMLContext *>(ctxPtr);

  name = AccessNoteDeclName(yamlCtx.ctx, str);
  return name.empty() ? "invalid declaration name" : "";
}

void ScalarTraits<ObjCSelector>::output(const ObjCSelector &selector,
                                        void *ctxPtr, raw_ostream &os) {
  os << selector;
}

StringRef ScalarTraits<ObjCSelector>::input(StringRef str, void *ctxPtr,
                                            ObjCSelector &selector) {
  auto &yamlCtx = *static_cast<swift::AccessNoteYAMLContext *>(ctxPtr);

  if (auto sel = ObjCSelector::parse(yamlCtx.ctx, str)) {
    selector = *sel;
    return "";
  }

  return "invalid selector";
}

/// If \p io is outputting, mark all keys in the current mapping as used.
static void diagnoseUnknownKeys(IO &io, ArrayRef<StringRef> expectedKeys) {
  if (io.outputting())
    return;

  auto &yamlCtx = *static_cast<swift::AccessNoteYAMLContext *>(io.getContext());

  for (auto key : io.keys()) {
    if (is_contained(expectedKeys, key))
      continue;

    // "Use" this key without actually doing anything with it to suppress the
    // error llvm::yaml::Input will otherwise generate.
    bool useDefault;
    void *saveInfo;
    if (io.preflightKey((const char *)key.bytes_begin(), /*required=*/false,
                        /*sameAsDefault=*/false, useDefault, saveInfo)) {
      // FIXME: We should diagnose these with locations, but llvm::yaml::Input
      // encapsulates all of the necessary details. Instead, we are currently
      // just building a list that the frontend will display.
      yamlCtx.unknownKeys.insert(key.str());

      io.postflightKey(saveInfo);
    }
  }
}

void MappingTraits<AccessNote>::mapping(IO &io, AccessNote &note) {
  diagnoseUnknownKeys(io, { "Name", "ObjC", "Dynamic", "ObjCName" });

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
  diagnoseUnknownKeys(io, { "Reason", "Notes" });

  io.mapRequired("Reason", notes.reason);
  io.mapRequired("Notes", notes.notes);
}

}
}
