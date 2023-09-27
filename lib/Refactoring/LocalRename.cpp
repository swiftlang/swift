//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "LocalRename.h"
#include "RefactoringActions.h"
#include "swift/AST/DiagnosticsRefactoring.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Index/Index.h"

using namespace swift::refactoring;
using namespace swift::index;

static const ValueDecl *getRelatedSystemDecl(const ValueDecl *VD) {
  if (VD->getModuleContext()->isNonUserModule())
    return VD;
  for (auto *Req : VD->getSatisfiedProtocolRequirements()) {
    if (Req->getModuleContext()->isNonUserModule())
      return Req;
  }
  for (auto Over = VD->getOverriddenDecl(); Over;
       Over = Over->getOverriddenDecl()) {
    if (Over->getModuleContext()->isNonUserModule())
      return Over;
  }
  return nullptr;
}

/// Stores information about the reference that rename availability is being
/// queried on.
struct RenameRefInfo {
  SourceFile *SF;  ///< The source file containing the reference.
  SourceLoc Loc;   ///< The reference's source location.
  bool IsArgLabel; ///< Whether Loc is on an arg label, rather than base name.
};

static llvm::Optional<RefactorAvailabilityInfo>
renameAvailabilityInfo(const ValueDecl *VD,
                       llvm::Optional<RenameRefInfo> RefInfo) {
  RefactorAvailableKind AvailKind = RefactorAvailableKind::Available;
  if (getRelatedSystemDecl(VD)) {
    AvailKind = RefactorAvailableKind::Unavailable_system_symbol;
  } else if (VD->getClangDecl()) {
    AvailKind = RefactorAvailableKind::Unavailable_decl_from_clang;
  } else if (!VD->hasName()) {
    AvailKind = RefactorAvailableKind::Unavailable_has_no_name;
  }

  auto isInMacroExpansionBuffer = [](const ValueDecl *VD) -> bool {
    auto *module = VD->getModuleContext();
    auto *file = module->getSourceFileContainingLocation(VD->getLoc());
    if (!file)
      return false;

    return file->getFulfilledMacroRole() != llvm::None;
  };

  if (AvailKind == RefactorAvailableKind::Available) {
    SourceLoc Loc = VD->getLoc();
    if (!Loc.isValid()) {
      AvailKind = RefactorAvailableKind::Unavailable_has_no_location;
    } else if (isInMacroExpansionBuffer(VD)) {
      AvailKind = RefactorAvailableKind::Unavailable_decl_in_macro;
    }
  }

  if (isa<AbstractFunctionDecl>(VD)) {
    // Disallow renaming accessors.
    if (isa<AccessorDecl>(VD))
      return llvm::None;

    // Disallow renaming deinit.
    if (isa<DestructorDecl>(VD))
      return llvm::None;

    // Disallow renaming init with no arguments.
    if (auto CD = dyn_cast<ConstructorDecl>(VD)) {
      if (!CD->getParameters()->size())
        return llvm::None;

      if (RefInfo && !RefInfo->IsArgLabel) {
        NameMatcher Matcher(*(RefInfo->SF));
        auto Resolved = Matcher.resolve({RefInfo->Loc, /*ResolveArgs*/ true});
        if (Resolved.LabelRanges.empty())
          return llvm::None;
      }
    }

    // Disallow renaming 'callAsFunction' method with no arguments.
    if (auto FD = dyn_cast<FuncDecl>(VD)) {
      // FIXME: syntactic rename can only decide by checking the spelling, not
      // whether it's an instance method, so we do the same here for now.
      if (FD->getBaseIdentifier() == FD->getASTContext().Id_callAsFunction) {
        if (!FD->getParameters()->size())
          return llvm::None;

        if (RefInfo && !RefInfo->IsArgLabel) {
          NameMatcher Matcher(*(RefInfo->SF));
          auto Resolved = Matcher.resolve({RefInfo->Loc, /*ResolveArgs*/ true});
          if (Resolved.LabelRanges.empty())
            return llvm::None;
        }
      }
    }
  }

  // Always return local rename for parameters.
  // FIXME: if the cursor is on the argument, we should return global rename.
  if (isa<ParamDecl>(VD))
    return RefactorAvailabilityInfo{RefactoringKind::LocalRename, AvailKind};

  // If the indexer considers VD a global symbol, then we apply global rename.
  if (index::isLocalSymbol(VD))
    return RefactorAvailabilityInfo{RefactoringKind::LocalRename, AvailKind};
  return RefactorAvailabilityInfo{RefactoringKind::GlobalRename, AvailKind};
}

/// Given a cursor, return the decl and its rename availability. \c None if
/// the cursor did not resolve to a decl or it resolved to a decl that we do
/// not allow renaming on.
llvm::Optional<RenameInfo>
swift::refactoring::getRenameInfo(ResolvedCursorInfoPtr cursorInfo) {
  auto valueCursor = dyn_cast<ResolvedValueRefCursorInfo>(cursorInfo);
  if (!valueCursor)
    return llvm::None;

  ValueDecl *VD = valueCursor->typeOrValue();
  if (!VD)
    return llvm::None;

  llvm::Optional<RenameRefInfo> refInfo;
  if (!valueCursor->getShorthandShadowedDecls().empty()) {
    // Find the outermost decl for a shorthand if let/closure capture
    VD = valueCursor->getShorthandShadowedDecls().back();
  } else if (valueCursor->isRef()) {
    refInfo = {valueCursor->getSourceFile(), valueCursor->getLoc(),
               valueCursor->isKeywordArgument()};
  }

  llvm::Optional<RefactorAvailabilityInfo> info =
      renameAvailabilityInfo(VD, refInfo);
  if (!info)
    return llvm::None;

  return RenameInfo{VD, *info};
}

class RenameRangeCollector : public IndexDataConsumer {
public:
  RenameRangeCollector(StringRef USR, StringRef newName)
      : USR(USR), newName(newName) {}

  RenameRangeCollector(const ValueDecl *D, StringRef newName)
      : newName(newName) {
    SmallString<64> SS;
    llvm::raw_svector_ostream OS(SS);
    printValueDeclUSR(D, OS);
    USR = stringStorage.copyString(SS.str());
  }

  RenameRangeCollector(RenameRangeCollector &&collector) = default;

  ArrayRef<RenameLoc> results() const { return locations; }

private:
  bool indexLocals() override { return true; }
  void failed(StringRef error) override {}
  bool startDependency(StringRef name, StringRef path, bool isClangModule,
                       bool isSystem) override {
    return true;
  }
  bool finishDependency(bool isClangModule) override { return true; }

  Action startSourceEntity(const IndexSymbol &symbol) override {
    if (symbol.USR == USR) {
      if (auto loc = indexSymbolToRenameLoc(symbol, newName)) {
        // Inside capture lists like `{ [test] in }`, 'test' refers to both the
        // newly declared, captured variable and the referenced variable it is
        // initialized from. Make sure to only rename it once.
        auto existingLoc = llvm::find_if(locations, [&](RenameLoc searchLoc) {
          return searchLoc.Line == loc->Line && searchLoc.Column == loc->Column;
        });
        if (existingLoc == locations.end()) {
          locations.push_back(std::move(*loc));
        } else {
          assert(existingLoc->OldName == loc->OldName &&
                 existingLoc->NewName == loc->NewName &&
                 existingLoc->IsFunctionLike == loc->IsFunctionLike &&
                 existingLoc->IsNonProtocolType == loc->IsNonProtocolType &&
                 "Asked to do a different rename for the same location?");
        }
      }
    }
    return IndexDataConsumer::Continue;
  }

  bool finishSourceEntity(SymbolInfo symInfo, SymbolRoleSet roles) override {
    return true;
  }

  llvm::Optional<RenameLoc>
  indexSymbolToRenameLoc(const index::IndexSymbol &symbol, StringRef NewName);

private:
  StringRef USR;
  StringRef newName;
  StringScratchSpace stringStorage;
  std::vector<RenameLoc> locations;
};

llvm::Optional<RenameLoc>
RenameRangeCollector::indexSymbolToRenameLoc(const index::IndexSymbol &symbol,
                                             StringRef newName) {
  if (symbol.roles & (unsigned)index::SymbolRole::Implicit) {
    return llvm::None;
  }

  NameUsage usage = NameUsage::Unknown;
  if (symbol.roles & (unsigned)index::SymbolRole::Call) {
    usage = NameUsage::Call;
  } else if (symbol.roles & (unsigned)index::SymbolRole::Definition) {
    usage = NameUsage::Definition;
  } else if (symbol.roles & (unsigned)index::SymbolRole::Reference) {
    usage = NameUsage::Reference;
  } else {
    llvm_unreachable("unexpected role");
  }

  bool isFunctionLike = false;
  bool isNonProtocolType = false;

  switch (symbol.symInfo.Kind) {
  case index::SymbolKind::EnumConstant:
  case index::SymbolKind::Function:
  case index::SymbolKind::Constructor:
  case index::SymbolKind::ConversionFunction:
  case index::SymbolKind::InstanceMethod:
  case index::SymbolKind::ClassMethod:
  case index::SymbolKind::StaticMethod:
    isFunctionLike = true;
    break;
  case index::SymbolKind::Class:
  case index::SymbolKind::Enum:
  case index::SymbolKind::Struct:
    isNonProtocolType = true;
    break;
  default:
    break;
  }
  StringRef oldName = stringStorage.copyString(symbol.name);
  return RenameLoc{symbol.line,    symbol.column,    usage, oldName, newName,
                   isFunctionLike, isNonProtocolType};
}

bool RefactoringActionLocalRename::isApplicable(
    ResolvedCursorInfoPtr CursorInfo, DiagnosticEngine &Diag) {
  llvm::Optional<RenameInfo> Info = getRenameInfo(CursorInfo);
  return Info &&
         Info->Availability.AvailableKind == RefactorAvailableKind::Available &&
         Info->Availability.Kind == RefactoringKind::LocalRename;
}

static void analyzeRenameScope(ValueDecl *VD,
                               SmallVectorImpl<DeclContext *> &Scopes) {
  auto *Scope = VD->getDeclContext();
  // There may be sibling decls that the renamed symbol is visible from.
  switch (Scope->getContextKind()) {
  case DeclContextKind::GenericTypeDecl:
  case DeclContextKind::ExtensionDecl:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::SubscriptDecl:
  case DeclContextKind::EnumElementDecl:
  case DeclContextKind::AbstractFunctionDecl:
    Scope = Scope->getParent();
    break;
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::SerializedLocal:
  case DeclContextKind::Package:
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::MacroDecl:
    break;
  }

  Scopes.push_back(Scope);
}

static llvm::Optional<RenameRangeCollector>
localRenames(SourceFile *SF, SourceLoc startLoc, StringRef preferredName,
             DiagnosticEngine &diags) {
  auto cursorInfo =
      evaluateOrDefault(SF->getASTContext().evaluator,
                        CursorInfoRequest{CursorInfoOwner(SF, startLoc)},
                        new ResolvedCursorInfo());

  llvm::Optional<RenameInfo> info = getRenameInfo(cursorInfo);
  if (!info) {
    diags.diagnose(startLoc, diag::unresolved_location);
    return llvm::None;
  }

  switch (info->Availability.AvailableKind) {
  case RefactorAvailableKind::Available:
    break;
  case RefactorAvailableKind::Unavailable_system_symbol:
    diags.diagnose(startLoc, diag::decl_is_system_symbol, info->VD->getName());
    return llvm::None;
  case RefactorAvailableKind::Unavailable_has_no_location:
    diags.diagnose(startLoc, diag::value_decl_no_loc, info->VD->getName());
    return llvm::None;
  case RefactorAvailableKind::Unavailable_has_no_name:
    diags.diagnose(startLoc, diag::decl_has_no_name);
    return llvm::None;
  case RefactorAvailableKind::Unavailable_has_no_accessibility:
    diags.diagnose(startLoc, diag::decl_no_accessibility);
    return llvm::None;
  case RefactorAvailableKind::Unavailable_decl_from_clang:
    diags.diagnose(startLoc, diag::decl_from_clang);
    return llvm::None;
  case RefactorAvailableKind::Unavailable_decl_in_macro:
    diags.diagnose(startLoc, diag::decl_in_macro);
    return llvm::None;
  }

  SmallVector<DeclContext *, 8> scopes;
  analyzeRenameScope(info->VD, scopes);
  if (scopes.empty())
    return llvm::None;

  RenameRangeCollector rangeCollector(info->VD, preferredName);
  for (DeclContext *DC : scopes)
    indexDeclContext(DC, rangeCollector);

  return rangeCollector;
}

bool RefactoringActionLocalRename::performChange() {
  if (StartLoc.isInvalid()) {
    DiagEngine.diagnose(SourceLoc(), diag::invalid_location);
    return true;
  }
  if (!DeclNameViewer(PreferredName).isValid()) {
    DiagEngine.diagnose(SourceLoc(), diag::invalid_name, PreferredName);
    return true;
  }
  if (!TheFile) {
    DiagEngine.diagnose(StartLoc, diag::location_module_mismatch,
                        MD->getNameStr());
    return true;
  }

  llvm::Optional<RenameRangeCollector> rangeCollector =
      localRenames(TheFile, StartLoc, PreferredName, DiagEngine);
  if (!rangeCollector)
    return true;

  auto consumers = DiagEngine.takeConsumers();
  assert(consumers.size() == 1);
  return syntacticRename(TheFile, rangeCollector->results(), EditConsumer,
                         *consumers[0]);
}

int swift::ide::findLocalRenameRanges(SourceFile *SF, RangeConfig Range,
                                      FindRenameRangesConsumer &RenameConsumer,
                                      DiagnosticConsumer &DiagConsumer) {
  assert(SF && "null source file");

  SourceManager &SM = SF->getASTContext().SourceMgr;
  DiagnosticEngine Diags(SM);
  Diags.addConsumer(DiagConsumer);

  auto StartLoc = Lexer::getLocForStartOfToken(SM, Range.getStart(SM));
  llvm::Optional<RenameRangeCollector> RangeCollector =
      localRenames(SF, StartLoc, StringRef(), Diags);
  if (!RangeCollector)
    return true;

  return findSyntacticRenameRanges(SF, RangeCollector->results(),
                                   RenameConsumer, DiagConsumer);
}
