//===--- TypeCheckAccessNotes.cpp - Type Checking for Access Notes --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the implicit attribute transform caused by access notes.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckDecl.h"
#include "TypeCheckObjC.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/LLVM.h"

using namespace swift;

static StringRef prettyPrintAttrs(const ValueDecl *VD,
                                  ArrayRef<const DeclAttribute *> attrs,
                                  SmallVectorImpl<char> &out) {
  llvm::raw_svector_ostream os(out);
  StreamPrinter printer(os);

  PrintOptions opts = PrintOptions::printDeclarations();
  VD->getAttrs().print(printer, opts, attrs, VD);
  return StringRef(out.begin(), out.size()).drop_back();
}

static void diagnoseChangesByAccessNote(
    ValueDecl *VD, ArrayRef<const DeclAttribute *> attrs,
    Diag<StringRef, StringRef, const ValueDecl *> diagID,
    Diag<StringRef> fixItID,
    llvm::function_ref<void(InFlightDiagnostic, StringRef)> addFixIts) {
  if (!VD->getASTContext().LangOpts.shouldRemarkOnAccessNoteSuccess() ||
      attrs.empty())
    return;

  // Generate string containing all attributes.
  SmallString<64> attrString;
  auto attrText = prettyPrintAttrs(VD, attrs, attrString);

  SourceLoc fixItLoc;

  auto reason = VD->getModuleContext()->getAccessNotes()->Reason;
  auto diag = VD->diagnose(diagID, reason, attrText, VD);
  for (auto attr : attrs) {
    diag.highlight(attr->getRangeWithAt());
    if (fixItLoc.isInvalid())
      fixItLoc = attr->getRangeWithAt().Start;
  }

  if (!fixItLoc)
    fixItLoc = VD->getAttributeInsertionLoc(true);

  addFixIts(VD->getASTContext().Diags.diagnose(fixItLoc, fixItID, attrText),
            attrString);
}

template <typename Attr>
static void addOrRemoveAttr(ValueDecl *VD, const AccessNotesFile &notes,
                            std::optional<bool> expected,
                            SmallVectorImpl<DeclAttribute *> &removedAttrs,
                            llvm::function_ref<Attr *()> willCreate) {
  if (!expected)
    return;

  auto attr = VD->getAttrs().getAttribute<Attr>();
  if (*expected == (attr != nullptr))
    return;

  if (*expected) {
    attr = willCreate();
    attr->setAddedByAccessNote();
    VD->getAttrs().add(attr);

    // Arrange for us to emit a remark about this attribute after type checking
    // has ensured it's valid.
    if (auto SF = VD->getDeclContext()->getParentSourceFile())
      SF->AttrsAddedByAccessNotes[VD].push_back(attr);
  } else {
    removedAttrs.push_back(attr);
    VD->getAttrs().removeAttribute(attr);
  }
}

InFlightDiagnostic swift::softenIfAccessNote(const Decl *D,
                                             const DeclAttribute *attr,
                                             InFlightDiagnostic &diag) {
  const ValueDecl *VD = dyn_cast<ValueDecl>(D);
  if (!VD || !attr || !attr->getAddedByAccessNote())
    return std::move(diag);

  SmallString<32> attrString;
  auto attrText = prettyPrintAttrs(VD, llvm::ArrayRef(attr), attrString);

  ASTContext &ctx = D->getASTContext();
  auto behavior = ctx.LangOpts.getAccessNoteFailureLimit();
  return std::move(diag.wrapIn(diag::wrap_invalid_attr_added_by_access_note,
                               D->getModuleContext()->getAccessNotes()->Reason,
                               ctx.AllocateCopy(attrText), VD)
                       .limitBehavior(behavior));
}

static void applyAccessNote(ValueDecl *VD, const AccessNote &note,
                            const AccessNotesFile &notes) {
  ASTContext &ctx = VD->getASTContext();
  SmallVector<DeclAttribute *, 2> removedAttrs;

  addOrRemoveAttr<ObjCAttr>(VD, notes, note.ObjC, removedAttrs, [&] {
    return ObjCAttr::create(ctx, note.ObjCName, false);
  });

  addOrRemoveAttr<DynamicAttr>(VD, notes, note.Dynamic, removedAttrs,
                               [&] { return new (ctx) DynamicAttr(true); });

  // FIXME: If we ever have more attributes, we'll need to sort removedAttrs by
  // SourceLoc. As it is, attrs are always before modifiers, so we're okay now.

  diagnoseChangesByAccessNote(VD, removedAttrs,
                              diag::attr_removed_by_access_note,
                              diag::fixit_attr_removed_by_access_note,
                              [&](InFlightDiagnostic diag, StringRef code) {
                                for (auto attr : llvm::reverse(removedAttrs))
                                  diag.fixItRemove(attr->getRangeWithAt());
                              });

  if (note.ObjCName) {
    auto newName = note.ObjCName.value();

    // addOrRemoveAttr above guarantees there's an ObjCAttr on this decl.
    auto attr = VD->getAttrs().getAttribute<ObjCAttr>();
    assert(attr && "ObjCName set, but ObjCAttr not true or did not apply???");

    if (!attr->hasName()) {
      // There was already an @objc attribute with no selector. Set it.
      attr->setName(newName, true);

      if (!ctx.LangOpts.shouldRemarkOnAccessNoteSuccess())
        return;

      VD->diagnose(diag::attr_objc_name_changed_by_access_note, notes.Reason,
                   VD, newName);

      auto fixIt =
          VD->diagnose(diag::fixit_attr_objc_name_changed_by_access_note);
      fixDeclarationObjCName(fixIt, VD, ObjCSelector(), newName);
    } else if (attr->getName() != newName) {
      // There was already an @objc
      auto behavior = ctx.LangOpts.getAccessNoteFailureLimit();

      VD->diagnose(diag::attr_objc_name_conflicts_with_access_note,
                   notes.Reason, VD, attr->getName().value(), newName)
          .highlight(attr->getRangeWithAt())
          .limitBehavior(behavior);
    }
  }
}

void TypeChecker::applyAccessNote(ValueDecl *VD) {
  (void)evaluateOrDefault(VD->getASTContext().evaluator,
                          ApplyAccessNoteRequest{VD}, {});
}

void swift::diagnoseAttrsAddedByAccessNote(SourceFile &SF) {
  if (!SF.getASTContext().LangOpts.shouldRemarkOnAccessNoteSuccess())
    return;

  for (auto declAndAttrs : SF.AttrsAddedByAccessNotes) {
    auto D = declAndAttrs.getFirst();
    SmallVector<DeclAttribute *, 4> sortedAttrs;
    llvm::append_range(sortedAttrs, declAndAttrs.getSecond());

    // Filter out invalid attributes.
    sortedAttrs.erase(llvm::remove_if(sortedAttrs,
                                      [](DeclAttribute *attr) {
                                        assert(attr->getAddedByAccessNote());
                                        return attr->isInvalid();
                                      }),
                      sortedAttrs.end());
    if (sortedAttrs.empty())
      continue;

    // Sort attributes by name.
    llvm::sort(sortedAttrs, [](DeclAttribute *first, DeclAttribute *second) {
      return first->getAttrName() < second->getAttrName();
    });
    sortedAttrs.erase(std::unique(sortedAttrs.begin(), sortedAttrs.end()),
                      sortedAttrs.end());

    diagnoseChangesByAccessNote(
        D, sortedAttrs, diag::attr_added_by_access_note,
        diag::fixit_attr_added_by_access_note,
        [=](InFlightDiagnostic diag, StringRef code) {
          diag.fixItInsert(D->getAttributeInsertionLoc(/*isModifier=*/true),
                           code);
        });
  }
}

const AccessNotesFile *
LoadAccessNotesRequest::evaluate(Evaluator &evaluator, ASTContext *_ctx) const {
  auto &ctx = *_ctx;
  auto &accessNotesPath = ctx.LangOpts.AccessNotesPath;
  if (accessNotesPath.empty())
    return nullptr;

  auto &SM = ctx.SourceMgr;
  auto &FS = *SM.getFileSystem();
  auto bufferOrError = swift::vfs::getFileOrSTDIN(FS, accessNotesPath);
  if (!bufferOrError) {
    ctx.Diags.diagnose(SourceLoc(), diag::access_notes_file_io_error,
                       accessNotesPath, bufferOrError.getError().message());
    return nullptr;
  }

  int sourceID = SM.addNewSourceBuffer(std::move(bufferOrError.get()));
  auto buffer = SM.getLLVMSourceMgr().getMemoryBuffer(sourceID);

  auto accessNotesFile = AccessNotesFile::load(ctx, buffer);
  if (!accessNotesFile)
    return nullptr;

  auto *result =
      ctx.AllocateObjectCopy<AccessNotesFile>(std::move(*accessNotesFile));
  ctx.addDestructorCleanup(*result);
  return result;
}

evaluator::SideEffect ApplyAccessNoteRequest::evaluate(Evaluator &evaluator,
                                                       ValueDecl *VD) const {
  // Access notes don't apply to ABI-only attributes.
  if (!ABIRoleInfo(VD).providesAPI())
    return {};

  auto *notes = VD->getModuleContext()->getAccessNotes();
  if (!notes)
    return {};

  if (auto note = notes->lookup(VD))
    applyAccessNote(VD, *note.get(), *notes);
  return {};
}
