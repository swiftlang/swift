//===----------------------------------------------------------------------===//
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

#include "swift/Refactoring/Refactoring.h"
#include "ContextFinder.h"
#include "ExtractExprBase.h"
#include "RefactoringActions.h"
#include "Renamer.h"
#include "Utils.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsRefactoring.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ForeignAsyncConvention.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Edit.h"
#include "swift/Basic/StringExtras.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Frontend/Frontend.h"
#include "swift/IDE/IDERequests.h"
#include "swift/Index/Index.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Subsystems.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSet.h"

using namespace swift;
using namespace swift::ide;
using namespace swift::index;
using namespace swift::refactoring;

class TextReplacementsRenamer : public Renamer {
  llvm::StringSet<> &ReplaceTextContext;
  SmallVector<Replacement> Replacements;

public:
  const DeclNameViewer New;

private:
  StringRef registerText(StringRef Text) {
    if (Text.empty())
      return Text;
    return ReplaceTextContext.insert(Text).first->getKey();
  }

  StringRef getCallArgLabelReplacement(StringRef OldLabelRange,
                                       StringRef NewLabel) {
    return NewLabel.empty() ? "" : NewLabel;
  }

  StringRef getCallArgColonReplacement(StringRef OldLabelRange,
                                       StringRef NewLabel) {
    // Expected OldLabelRange: foo( []3, a[: ]2,  b[ : ]3 ...)
    // FIXME: Preserve comments: foo([a/*:*/ : /*:*/ ]2, ...)
    if (NewLabel.empty())
      return "";
    if (OldLabelRange.empty())
      return ": ";
    return registerText(OldLabelRange);
  }

  StringRef getCallArgCombinedReplacement(StringRef OldArgLabel,
                                          StringRef NewArgLabel) {
    // This case only happens when going from foo([]1) to foo([a: ]1).
    assert(OldArgLabel.empty());
    if (NewArgLabel.empty())
      return "";
    return registerText((Twine(NewArgLabel) + ": ").str());
  }

  StringRef getParamNameReplacement(StringRef OldParam, StringRef OldArgLabel,
                                    StringRef NewArgLabel) {
    // We don't want to get foo(a a: Int), so drop the parameter name if the
    // argument label will match the original name.
    // Note: the leading whitespace is part of the parameter range.
    if (!NewArgLabel.empty() && OldParam.ltrim() == NewArgLabel)
      return "";

    // If we're renaming foo(x: Int) to foo(_:), then use the original argument
    // label as the parameter name so as to not break references in the body.
    if (NewArgLabel.empty() && !OldArgLabel.empty() && OldParam.empty())
      return registerText((Twine(" ") + OldArgLabel).str());

    return registerText(OldParam);
  }

  StringRef getDeclArgumentLabelReplacement(StringRef OldLabelRange,
                                            StringRef NewArgLabel) {
      // OldLabelRange is subscript([]a: Int), foo([a]: Int) or foo([a] b: Int)
      if (NewArgLabel.empty())
        return OldLabelRange.empty() ? "" : "_";

      if (OldLabelRange.empty())
        return registerText((Twine(NewArgLabel) + " ").str());
      return registerText(NewArgLabel);
  }

  StringRef getReplacementText(StringRef LabelRange,
                               RefactoringRangeKind RangeKind,
                               StringRef OldLabel, StringRef NewLabel) {
    switch (RangeKind) {
    case RefactoringRangeKind::CallArgumentLabel:
      return getCallArgLabelReplacement(LabelRange, NewLabel);
    case RefactoringRangeKind::CallArgumentColon:
      return getCallArgColonReplacement(LabelRange, NewLabel);
    case RefactoringRangeKind::CallArgumentCombined:
      return getCallArgCombinedReplacement(LabelRange, NewLabel);
    case RefactoringRangeKind::ParameterName:
      return getParamNameReplacement(LabelRange, OldLabel, NewLabel);
    case RefactoringRangeKind::NoncollapsibleParameterName:
      return LabelRange;
    case RefactoringRangeKind::DeclArgumentLabel:
      return getDeclArgumentLabelReplacement(LabelRange, NewLabel);
    case RefactoringRangeKind::SelectorArgumentLabel:
      return NewLabel.empty() ? "_" : registerText(NewLabel);
    default:
      llvm_unreachable("label range type is none but there are labels");
    }
  }

  void addReplacement(CharSourceRange LabelRange,
                      RefactoringRangeKind RangeKind, StringRef OldLabel,
                      StringRef NewLabel) {
    StringRef ExistingLabel = LabelRange.str();
    StringRef Text =
        getReplacementText(ExistingLabel, RangeKind, OldLabel, NewLabel);
    if (Text != ExistingLabel)
      Replacements.push_back({/*Path=*/{}, LabelRange, /*BufferName=*/{}, Text,
                              /*RegionsWorthNote=*/{}});
  }

  void doRenameLabel(CharSourceRange Label, RefactoringRangeKind RangeKind,
                     unsigned NameIndex) override {
    addReplacement(Label, RangeKind, Old.args()[NameIndex],
                   New.args()[NameIndex]);
  }

  void doRenameBase(CharSourceRange Range, RefactoringRangeKind) override {
    if (Old.base() != New.base())
      Replacements.push_back({/*Path=*/{}, Range, /*BufferName=*/{},
                              registerText(New.base()),
                              /*RegionsWorthNote=*/{}});
  }

public:
  TextReplacementsRenamer(const SourceManager &SM, StringRef OldName,
                          StringRef NewName,
                          llvm::StringSet<> &ReplaceTextContext)
      : Renamer(SM, OldName), ReplaceTextContext(ReplaceTextContext),
        New(NewName) {
    assert(Old.isValid() && New.isValid());
    assert(Old.partsCount() == New.partsCount());
  }

  ArrayRef<Replacement> getReplacements() const { return Replacements; }
};

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

struct RenameInfo {
  ValueDecl *VD;
  RefactorAvailabilityInfo Availability;
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
static llvm::Optional<RenameInfo>
getRenameInfo(ResolvedCursorInfoPtr cursorInfo) {
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
  bool startDependency(StringRef name, StringRef path, bool isClangModule, bool isSystem) override {
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

StringRef getDefaultPreferredName(RefactoringKind Kind) {
  switch(Kind) {
    case RefactoringKind::None:
      llvm_unreachable("Should be a valid refactoring kind");
    case RefactoringKind::GlobalRename:
    case RefactoringKind::LocalRename:
      return "newName";
    case RefactoringKind::ExtractExpr:
    case RefactoringKind::ExtractRepeatedExpr:
      return "extractedExpr";
    case RefactoringKind::ExtractFunction:
      return "extractedFunc";
    default:
      return "";
  }
}

static SmallVector<RefactorAvailabilityInfo, 0>
collectRefactoringsAtCursor(SourceFile *SF, unsigned Line, unsigned Column,
                            ArrayRef<DiagnosticConsumer *> DiagConsumers) {
  // Prepare the tool box.
  ASTContext &Ctx = SF->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;
  DiagnosticEngine DiagEngine(SM);
  std::for_each(DiagConsumers.begin(), DiagConsumers.end(),
                [&](DiagnosticConsumer *Con) { DiagEngine.addConsumer(*Con); });
  SourceLoc Loc = SM.getLocForLineCol(SF->getBufferID().value(), Line, Column);
  if (Loc.isInvalid())
    return {};

  ResolvedCursorInfoPtr Tok =
      evaluateOrDefault(SF->getASTContext().evaluator,
                        CursorInfoRequest{CursorInfoOwner(
                            SF, Lexer::getLocForStartOfToken(SM, Loc))},
                        new ResolvedCursorInfo());
  return collectRefactorings(Tok, /*ExcludeRename=*/false);
}

static bool collectRangeStartRefactorings(const ResolvedRangeInfo &Info) {
  switch (Info.Kind) {
  case RangeKind::SingleExpression:
  case RangeKind::SingleStatement:
  case RangeKind::SingleDecl:
  case RangeKind::PartOfExpression:
    return true;
  case RangeKind::MultiStatement:
  case RangeKind::MultiTypeMemberDecl:
  case RangeKind::Invalid:
    return false;
  }
}

namespace asyncrefactorings {

// TODO: Should probably split the refactorings into separate files

/// Whether the given type is (or conforms to) the stdlib Error type
bool isErrorType(Type Ty, ModuleDecl *MD) {
  if (!Ty)
    return false;
  return !MD->conformsToProtocol(Ty, Ty->getASTContext().getErrorDecl())
              .isInvalid();
}

// The single Decl* subject of a switch statement, or nullptr if none
Decl *singleSwitchSubject(const SwitchStmt *Switch) {
  if (auto *DRE = dyn_cast<DeclRefExpr>(Switch->getSubjectExpr()))
    return DRE->getDecl();
  return nullptr;
}

/// A more aggressive variant of \c Expr::getReferencedDecl that also looks
/// through autoclosures created to pass the \c self parameter to a member funcs
ValueDecl *getReferencedDecl(const Expr *Fn) {
  Fn = Fn->getSemanticsProvidingExpr();
  if (auto *DRE = dyn_cast<DeclRefExpr>(Fn))
    return DRE->getDecl();
  if (auto ApplyE = dyn_cast<SelfApplyExpr>(Fn))
    return getReferencedDecl(ApplyE->getFn());
  if (auto *ACE = dyn_cast<AutoClosureExpr>(Fn)) {
    if (auto *Unwrapped = ACE->getUnwrappedCurryThunkExpr())
      return getReferencedDecl(Unwrapped);
  }
  return nullptr;
}

FuncDecl *getUnderlyingFunc(const Expr *Fn) {
  return dyn_cast_or_null<FuncDecl>(getReferencedDecl(Fn));
}

/// Find the outermost call of the given location
CallExpr *findOuterCall(ResolvedCursorInfoPtr CursorInfo) {
  auto IncludeInContext = [](ASTNode N) {
    if (auto *E = N.dyn_cast<Expr *>())
      return !E->isImplicit();
    return false;
  };

  // TODO: Bit pointless using the "ContextFinder" here. Ideally we would have
  //       already generated a slice of the AST for anything that contains
  //       the cursor location
  ContextFinder Finder(*CursorInfo->getSourceFile(), CursorInfo->getLoc(),
                       IncludeInContext);
  Finder.resolve();
  auto Contexts = Finder.getContexts();
  if (Contexts.empty())
    return nullptr;

  CallExpr *CE = dyn_cast<CallExpr>(Contexts[0].get<Expr *>());
  if (!CE)
    return nullptr;

  SourceManager &SM = CursorInfo->getSourceFile()->getASTContext().SourceMgr;
  if (!SM.rangeContains(CE->getFn()->getSourceRange(), CursorInfo->getLoc()))
    return nullptr;
  return CE;
}

/// Find the function matching the given location if it is not an accessor and
/// either has a body or is a member of a protocol
FuncDecl *findFunction(ResolvedCursorInfoPtr CursorInfo) {
  auto IncludeInContext = [](ASTNode N) {
    if (auto *D = N.dyn_cast<Decl *>())
      return !D->isImplicit();
    return false;
  };

  ContextFinder Finder(*CursorInfo->getSourceFile(), CursorInfo->getLoc(),
                       IncludeInContext);
  Finder.resolve();

  auto Contexts = Finder.getContexts();
  if (Contexts.empty())
    return nullptr;

  if (Contexts.back().isDecl(DeclKind::Param))
    Contexts = Contexts.drop_back();

  auto *FD = dyn_cast_or_null<FuncDecl>(Contexts.back().get<Decl *>());
  if (!FD || isa<AccessorDecl>(FD))
    return nullptr;

  auto *Body = FD->getBody();
  if (!Body && !isa<ProtocolDecl>(FD->getDeclContext()))
    return nullptr;

  SourceManager &SM = CursorInfo->getSourceFile()->getASTContext().SourceMgr;
  SourceLoc DeclEnd = Body ? Body->getLBraceLoc() : FD->getEndLoc();
  if (!SM.rangeContains(SourceRange(FD->getStartLoc(), DeclEnd),
                        CursorInfo->getLoc()))
    return nullptr;

  return FD;
}

FuncDecl *isOperator(const BinaryExpr *BE) {
  auto *AE = dyn_cast<ApplyExpr>(BE->getFn());
  if (AE) {
    auto *Callee = AE->getCalledValue();
    if (Callee && Callee->isOperator() && isa<FuncDecl>(Callee))
      return cast<FuncDecl>(Callee);
  }
  return nullptr;
}

/// Describes the expressions to be kept from a call to the handler in a
/// function that has (or will have ) and async alternative. Eg.
/// ```
/// func toBeAsync(completion: (String?, Error?) -> Void) {
///   ...
///   completion("something", nil) // Result = ["something"], IsError = false
///   ...
///   completion(nil, MyError.Bad) // Result = [MyError.Bad], IsError = true
/// }
class HandlerResult {
  SmallVector<Argument, 2> Args;
  bool IsError = false;

public:
  HandlerResult() {}

  HandlerResult(ArrayRef<Argument> ArgsRef)
      : Args(ArgsRef.begin(), ArgsRef.end()) {}

  HandlerResult(Argument Arg, bool IsError) : IsError(IsError) {
    Args.push_back(Arg);
  }

  bool isError() { return IsError; }

  ArrayRef<Argument> args() { return Args; }
};

/// The type of the handler, ie. whether it takes regular parameters or a
/// single parameter of `Result` type.
enum class HandlerType { INVALID, PARAMS, RESULT };

/// A single return type of a refactored async function. If the async function
/// returns a tuple, each element of the tuple (represented by a \c
/// LabeledReturnType) might have a label, otherwise the \p Label is empty.
struct LabeledReturnType {
  Identifier Label;
  swift::Type Ty;

  LabeledReturnType(Identifier Label, swift::Type Ty) : Label(Label), Ty(Ty) {}
};

/// Given a function with an async alternative (or one that *could* have an
/// async alternative), stores information about the completion handler.
/// The completion handler can be either a variable (which includes a parameter)
/// or a function
struct AsyncHandlerDesc {
  PointerUnion<const VarDecl *, const AbstractFunctionDecl *> Handler = nullptr;
  HandlerType Type = HandlerType::INVALID;
  bool HasError = false;

  static AsyncHandlerDesc get(const ValueDecl *Handler, bool RequireName) {
    AsyncHandlerDesc HandlerDesc;
    if (auto Var = dyn_cast<VarDecl>(Handler)) {
      HandlerDesc.Handler = Var;
    } else if (auto Func = dyn_cast<AbstractFunctionDecl>(Handler)) {
      HandlerDesc.Handler = Func;
    } else {
      // The handler must be a variable or function
      return AsyncHandlerDesc();
    }

    // Callback must have a completion-like name
    if (RequireName && !isCompletionHandlerParamName(HandlerDesc.getNameStr()))
      return AsyncHandlerDesc();

    // Callback must be a function type and return void. Doesn't need to have
    // any parameters - may just be a "I'm done" callback
    auto *HandlerTy = HandlerDesc.getType()->getAs<AnyFunctionType>();
    if (!HandlerTy || !HandlerTy->getResult()->isVoid())
      return AsyncHandlerDesc();

    // Find the type of result in the handler (eg. whether it's a Result<...>,
    // just parameters, or nothing).
    auto HandlerParams = HandlerTy->getParams();
    if (HandlerParams.size() == 1) {
      auto ParamTy =
          HandlerParams.back().getPlainType()->getAs<BoundGenericType>();
      if (ParamTy && ParamTy->isResult()) {
        auto GenericArgs = ParamTy->getGenericArgs();
        assert(GenericArgs.size() == 2 && "Result should have two params");
        HandlerDesc.Type = HandlerType::RESULT;
        HandlerDesc.HasError = !GenericArgs.back()->isUninhabited();
      }
    }

    if (HandlerDesc.Type != HandlerType::RESULT) {
      // Only handle non-result parameters
      for (auto &Param : HandlerParams) {
        if (Param.getPlainType() && Param.getPlainType()->isResult())
          return AsyncHandlerDesc();
      }

      HandlerDesc.Type = HandlerType::PARAMS;
      if (!HandlerParams.empty()) {
        auto LastParamTy = HandlerParams.back().getParameterType();
        HandlerDesc.HasError = isErrorType(LastParamTy->getOptionalObjectType(),
                                           Handler->getModuleContext());
      }
    }

    return HandlerDesc;
  }

  bool isValid() const { return Type != HandlerType::INVALID; }

  /// Return the declaration of the completion handler as a \c ValueDecl.
  /// In practice, the handler will always be a \c VarDecl or \c
  /// AbstractFunctionDecl.
  /// \c getNameStr and \c getType provide access functions that are available
  /// for both variables and functions, but not on \c ValueDecls.
  const ValueDecl *getHandler() const {
    if (!Handler) {
      return nullptr;
    }
    if (auto Var = Handler.dyn_cast<const VarDecl *>()) {
      return Var;
    } else if (auto Func = Handler.dyn_cast<const AbstractFunctionDecl *>()) {
      return Func;
    } else {
      llvm_unreachable("Unknown handler type");
    }
  }

  /// Return the name of the completion handler. If it is a variable, the
  /// variable name, if it's a function, the function base name.
  StringRef getNameStr() const {
    if (auto Var = Handler.dyn_cast<const VarDecl *>()) {
      return Var->getNameStr();
    } else if (auto Func = Handler.dyn_cast<const AbstractFunctionDecl *>()) {
      return Func->getNameStr();
    } else {
      llvm_unreachable("Unknown handler type");
    }
  }

  HandlerType getHandlerType() const { return Type; }

  /// Get the type of the completion handler.
  swift::Type getType() const {
    if (auto Var = Handler.dyn_cast<const VarDecl *>()) {
      return Var->getTypeInContext();
    } else if (auto Func = Handler.dyn_cast<const AbstractFunctionDecl *>()) {
      auto Type = Func->getInterfaceType();
      // Undo the self curry thunk if we are referencing a member function.
      if (Func->hasImplicitSelfDecl()) {
        assert(Type->is<AnyFunctionType>());
        Type = Type->getAs<AnyFunctionType>()->getResult();
      }
      return Type;
    } else {
      llvm_unreachable("Unknown handler type");
    }
  }

  ArrayRef<AnyFunctionType::Param> params() const {
    auto Ty = getType()->getAs<AnyFunctionType>();
    assert(Ty && "Type must be a function type");
    return Ty->getParams();
  }

  /// Retrieve the parameters relevant to a successful return from the
  /// completion handler. This drops the Error parameter if present.
  ArrayRef<AnyFunctionType::Param> getSuccessParams() const {
    if (HasError && Type == HandlerType::PARAMS)
      return params().drop_back();
    return params();
  }

  /// If the completion handler has an Error parameter, return it.
  llvm::Optional<AnyFunctionType::Param> getErrorParam() const {
    if (HasError && Type == HandlerType::PARAMS)
      return params().back();
    return llvm::None;
  }

  /// Get the type of the error that will be thrown by the \c async method or \c
  /// None if the completion handler doesn't accept an error parameter.
  /// This may be more specialized than the generic 'Error' type if the
  /// completion handler of the converted function takes a more specialized
  /// error type.
  llvm::Optional<swift::Type> getErrorType() const {
    if (HasError) {
      switch (Type) {
      case HandlerType::INVALID:
        return llvm::None;
      case HandlerType::PARAMS:
        // The last parameter of the completion handler is the error param
        return params().back().getPlainType()->lookThroughSingleOptionalType();
      case HandlerType::RESULT:
        assert(
            params().size() == 1 &&
            "Result handler should have the Result type as the only parameter");
        auto ResultType =
            params().back().getPlainType()->getAs<BoundGenericType>();
        auto GenericArgs = ResultType->getGenericArgs();
        assert(GenericArgs.size() == 2 && "Result should have two params");
        // The second (last) generic parameter of the Result type is the error
        // type.
        return GenericArgs.back();
      }
    } else {
      return llvm::None;
    }
  }

  /// The `CallExpr` if the given node is a call to the `Handler`
  CallExpr *getAsHandlerCall(ASTNode Node) const {
    if (!isValid())
      return nullptr;

    if (auto E = Node.dyn_cast<Expr *>()) {
      if (auto *CE = dyn_cast<CallExpr>(E->getSemanticsProvidingExpr())) {
        if (CE->getFn()->getReferencedDecl().getDecl() == getHandler()) {
          return CE;
        }
      }
    }
    return nullptr;
  }

  /// Returns \c true if the call to the completion handler contains possibly
  /// non-nil values for both the success and error parameters, e.g.
  /// \code
  ///   completion(result, error)
  /// \endcode
  /// This can only happen if the completion handler is a params handler.
  bool isAmbiguousCallToParamHandler(const CallExpr *CE) const {
    if (!HasError || Type != HandlerType::PARAMS) {
      // Only param handlers with an error can pass both an error AND a result.
      return false;
    }
    auto Args = CE->getArgs()->getArgExprs();
    if (!isa<NilLiteralExpr>(Args.back())) {
      // We've got an error parameter. If any of the success params is not nil,
      // the call is ambiguous.
      for (auto &Arg : Args.drop_back()) {
        if (!isa<NilLiteralExpr>(Arg)) {
          return true;
        }
      }
    }
    return false;
  }

  /// Given a call to the `Handler`, extract the expressions to be returned or
  /// thrown, taking care to remove the `.success`/`.failure` if it's a
  /// `RESULT` handler type.
  /// If the call is ambiguous (contains potentially non-nil arguments to both
  /// the result and the error parameters), the \p ReturnErrorArgsIfAmbiguous
  /// determines whether the success or error parameters are passed.
  HandlerResult extractResultArgs(const CallExpr *CE,
                                  bool ReturnErrorArgsIfAmbiguous) const {
    auto *ArgList = CE->getArgs();
    SmallVector<Argument, 2> Scratch(ArgList->begin(), ArgList->end());
    auto Args = llvm::makeArrayRef(Scratch);

    if (Type == HandlerType::PARAMS) {
      bool IsErrorResult;
      if (isAmbiguousCallToParamHandler(CE)) {
        IsErrorResult = ReturnErrorArgsIfAmbiguous;
      } else {
        // If there's an error parameter and the user isn't passing nil to it,
        // assume this is the error path.
        IsErrorResult =
            (HasError && !isa<NilLiteralExpr>(Args.back().getExpr()));
      }
      if (IsErrorResult)
        return HandlerResult(Args.back(), true);

      // We can drop the args altogether if they're just Void.
      if (willAsyncReturnVoid())
        return HandlerResult();

      return HandlerResult(HasError ? Args.drop_back() : Args);
    } else if (Type == HandlerType::RESULT) {
      if (Args.size() != 1)
        return HandlerResult(Args);

      auto *ResultCE = dyn_cast<CallExpr>(Args[0].getExpr());
      if (!ResultCE)
        return HandlerResult(Args);

      auto *DSC = dyn_cast<DotSyntaxCallExpr>(ResultCE->getFn());
      if (!DSC)
        return HandlerResult(Args);

      auto *D = dyn_cast<EnumElementDecl>(
          DSC->getFn()->getReferencedDecl().getDecl());
      if (!D)
        return HandlerResult(Args);

      auto ResultArgList = ResultCE->getArgs();
      auto isFailure = D->getNameStr() == StringRef("failure");

      // We can drop the arg altogether if it's just Void.
      if (!isFailure && willAsyncReturnVoid())
        return HandlerResult();

      // Otherwise the arg gets the .success() or .failure() call dropped.
      return HandlerResult(ResultArgList->get(0), isFailure);
    }

    llvm_unreachable("Unhandled result type");
  }

  // Convert the type of a success parameter in the completion handler function
  // to a return type suitable for an async function. If there is an error
  // parameter present e.g (T?, Error?) -> Void, this unwraps a level of
  // optionality from T?. If this is a Result<T, U> type, returns the success
  // type T.
  swift::Type getSuccessParamAsyncReturnType(swift::Type Ty) const {
    switch (Type) {
    case HandlerType::PARAMS: {
      // If there's an Error parameter in the handler, the success branch can
      // be unwrapped.
      if (HasError)
        Ty = Ty->lookThroughSingleOptionalType();

      return Ty;
    }
    case HandlerType::RESULT: {
      // Result<T, U> maps to T.
      return Ty->castTo<BoundGenericType>()->getGenericArgs()[0];
    }
    case HandlerType::INVALID:
      llvm_unreachable("Invalid handler type");
    }
  }

  /// If the async function returns a tuple, the label of the \p Index -th
  /// element in the returned tuple. If the function doesn't return a tuple or
  /// the element is unlabeled, an empty identifier is returned.
  Identifier getAsyncReturnTypeLabel(size_t Index) const {
    assert(Index < getSuccessParams().size());
    if (getSuccessParams().size() <= 1) {
      // There can't be any labels if the async function doesn't return a tuple.
      return Identifier();
    } else {
      return getSuccessParams()[Index].getInternalLabel();
    }
  }

  /// Gets the return value types for the async equivalent of this handler.
  ArrayRef<LabeledReturnType>
  getAsyncReturnTypes(SmallVectorImpl<LabeledReturnType> &Scratch) const {
    for (size_t I = 0; I < getSuccessParams().size(); ++I) {
      auto Ty = getSuccessParams()[I].getParameterType();
      Scratch.emplace_back(getAsyncReturnTypeLabel(I),
                           getSuccessParamAsyncReturnType(Ty));
    }
    return Scratch;
  }

  /// Whether the async equivalent of this handler returns Void.
  bool willAsyncReturnVoid() const {
    // If all of the success params will be converted to Void return types,
    // this will be a Void async function.
    return llvm::all_of(getSuccessParams(), [&](auto &param) {
      auto Ty = param.getParameterType();
      return getSuccessParamAsyncReturnType(Ty)->isVoid();
    });
  }

  // TODO: If we have an async alternative we should check its result types
  //       for whether to unwrap or not
  bool shouldUnwrap(swift::Type Ty) const {
    return HasError && Ty->isOptional();
  }
};

/// Given a completion handler that is part of a function signature, stores
/// information about that completion handler and its index within the function
/// declaration.
struct AsyncHandlerParamDesc : public AsyncHandlerDesc {
  /// Enum to represent the position of the completion handler param within
  /// the parameter list. Given `(A, B, C, D)`:
  ///   - A is `First`
  ///   - B and C are `Middle`
  ///   - D is `Last`
  /// The position is `Only` if there's a single parameter that is the
  /// completion handler and `None` if there is no handler.
  enum class Position {
    First, Middle, Last, Only, None
  };

  /// The function the completion handler is a parameter of.
  const FuncDecl *Func = nullptr;
  /// The index of the completion handler in the function that declares it.
  unsigned Index = 0;
  /// The async alternative, if one is found.
  const AbstractFunctionDecl *Alternative = nullptr;

  AsyncHandlerParamDesc() : AsyncHandlerDesc() {}
  AsyncHandlerParamDesc(const AsyncHandlerDesc &Handler, const FuncDecl *Func,
                        unsigned Index,
                        const AbstractFunctionDecl *Alternative)
      : AsyncHandlerDesc(Handler), Func(Func), Index(Index),
        Alternative(Alternative) {}

  static AsyncHandlerParamDesc find(const FuncDecl *FD,
                                    bool RequireAttributeOrName) {
    if (!FD || FD->hasAsync() || FD->hasThrows() ||
        !FD->getResultInterfaceType()->isVoid())
      return AsyncHandlerParamDesc();

    const auto *Alternative = FD->getAsyncAlternative();
    llvm::Optional<unsigned> Index =
        FD->findPotentialCompletionHandlerParam(Alternative);
    if (!Index)
      return AsyncHandlerParamDesc();

    bool RequireName = RequireAttributeOrName && !Alternative;
    return AsyncHandlerParamDesc(
        AsyncHandlerDesc::get(FD->getParameters()->get(*Index), RequireName),
        FD, *Index, Alternative);
  }

  /// Build an @available attribute with the name of the async alternative as
  /// the \c renamed argument, followed by a newline.
  SmallString<128> buildRenamedAttribute() const {
    SmallString<128> AvailabilityAttr;
    llvm::raw_svector_ostream OS(AvailabilityAttr);

    // If there's an alternative then there must already be an attribute,
    // don't add another.
    if (!isValid() || Alternative)
      return AvailabilityAttr;

    DeclName Name = Func->getName();
    OS << "@available(*, renamed: \"" << Name.getBaseName() << "(";
    ArrayRef<Identifier> ArgNames = Name.getArgumentNames();
    for (size_t I = 0; I < ArgNames.size(); ++I) {
      if (I != Index) {
        OS << ArgNames[I] << tok::colon;
      }
    }
    OS << ")\")\n";

    return AvailabilityAttr;
  }

  /// Retrieves the parameter decl for the completion handler parameter, or
  /// \c nullptr if no valid completion parameter is present.
  const ParamDecl *getHandlerParam() const {
    if (!isValid())
      return nullptr;
    return cast<ParamDecl>(getHandler());
  }

  /// See \c Position
  Position handlerParamPosition() const {
    if (!isValid())
      return Position::None;
    const auto *Params = Func->getParameters();
    if (Params->size() == 1)
      return Position::Only;
    if (Index == 0)
      return Position::First;
    if (Index == Params->size() - 1)
      return Position::Last;
    return Position::Middle;
  }

  bool operator==(const AsyncHandlerParamDesc &Other) const {
    return Handler == Other.Handler && Type == Other.Type &&
           HasError == Other.HasError && Index == Other.Index;
  }

  bool alternativeIsAccessor() const {
    return isa_and_nonnull<AccessorDecl>(Alternative);
  }
};

/// The type of a condition in a conditional statement.
enum class ConditionType {
  NIL,             // == nil
  NOT_NIL,         // != nil
  IS_TRUE,         // if b
  IS_FALSE,        // if !b
  SUCCESS_PATTERN, // case .success
  FAILURE_PATTEN   // case .failure
};

/// Indicates whether a condition describes a success or failure path. For
/// example, a check for whether an error parameter is present is a failure
/// path. A check for a nil error parameter is a success path. This is distinct
/// from ConditionType, as it relies on contextual information about what values
/// need to be checked for success or failure.
enum class ConditionPath { SUCCESS, FAILURE };

static ConditionPath flippedConditionPath(ConditionPath Path) {
  switch (Path) {
  case ConditionPath::SUCCESS:
    return ConditionPath::FAILURE;
  case ConditionPath::FAILURE:
    return ConditionPath::SUCCESS;
  }
  llvm_unreachable("Unhandled case in switch!");
}

/// Finds the `Subject` being compared to in various conditions. Also finds any
/// pattern that may have a bound name.
struct CallbackCondition {
  llvm::Optional<ConditionType> Type;
  const Decl *Subject = nullptr;
  const Pattern *BindPattern = nullptr;

  /// Initializes a `CallbackCondition` with a `!=` or `==` comparison of
  /// an `Optional` typed `Subject` to `nil`, or a `Bool` typed `Subject` to a
  /// boolean literal, ie.
  ///   - `<Subject> != nil`
  ///   - `<Subject> == nil`
  ///   - `<Subject> != true`
  ///   - `<Subject> == false`
  CallbackCondition(const BinaryExpr *BE, const FuncDecl *Operator) {
    bool FoundNil = false;
    BooleanLiteralExpr *FoundBool = nullptr;
    bool DidUnwrapOptional = false;

    for (auto *Operand : {BE->getLHS(), BE->getRHS()}) {
      Operand = Operand->getSemanticsProvidingExpr();
      if (auto *IIOE = dyn_cast<InjectIntoOptionalExpr>(Operand)) {
        Operand = IIOE->getSubExpr()->getSemanticsProvidingExpr();
        DidUnwrapOptional = true;
      }
      if (isa<NilLiteralExpr>(Operand)) {
        FoundNil = true;
      } else if (auto *BLE = dyn_cast<BooleanLiteralExpr>(Operand)) {
        FoundBool = BLE;
      } else if (auto *DRE = dyn_cast<DeclRefExpr>(Operand)) {
        Subject = DRE->getDecl();
      }
    }

    if (!Subject)
      return;

    if (FoundNil) {
      if (Operator->getBaseName() == "==") {
        Type = ConditionType::NIL;
      } else if (Operator->getBaseName() == "!=") {
        Type = ConditionType::NOT_NIL;
      }
    } else if (FoundBool) {
      if (Operator->getBaseName() == "==") {
        Type = FoundBool->getValue() ? ConditionType::IS_TRUE
                                     : ConditionType::IS_FALSE;
      } else if (Operator->getBaseName() == "!=" && !DidUnwrapOptional) {
        // Note that we don't consider this case if we unwrapped an optional,
        // as e.g optBool != false is a check for true *or* nil.
        Type = FoundBool->getValue() ? ConditionType::IS_FALSE
                                     : ConditionType::IS_TRUE;
      }
    }
  }

  /// A bool condition expression.
  explicit CallbackCondition(const Expr *E) {
    // FIXME: Sema should produce ErrorType.
    if (!E->getType() || !E->getType()->isBool())
      return;

    auto CondType = ConditionType::IS_TRUE;
    E = E->getSemanticsProvidingExpr();

    // If we have a prefix negation operator, this is a check for false.
    if (auto *PrefixOp = dyn_cast<PrefixUnaryExpr>(E)) {
      auto *Callee = PrefixOp->getCalledValue();
      if (Callee && Callee->isOperator() && Callee->getBaseName() == "!") {
        CondType = ConditionType::IS_FALSE;
        E = PrefixOp->getOperand()->getSemanticsProvidingExpr();
      }
    }

    auto *DRE = dyn_cast<DeclRefExpr>(E);
    if (!DRE)
      return;

    Subject = DRE->getDecl();
    Type = CondType;
  }

  /// Initializes a `CallbackCondition` with binding of an `Optional` or
  /// `Result` typed `Subject`, ie.
  ///   - `let bind = <Subject>`
  ///   - `case .success(let bind) = <Subject>`
  ///   - `case .failure(let bind) = <Subject>`
  ///   - `let bind = try? <Subject>.get()`
  CallbackCondition(const Pattern *P, const Expr *Init) {
    Init = Init->getSemanticsProvidingExpr();
    P = P->getSemanticsProvidingPattern();

    if (auto *DRE = dyn_cast<DeclRefExpr>(Init)) {
      if (auto *OSP = dyn_cast<OptionalSomePattern>(P)) {
        // `let bind = <Subject>`
        Type = ConditionType::NOT_NIL;
        Subject = DRE->getDecl();
        BindPattern = OSP->getSubPattern();
      } else if (auto *EEP = dyn_cast<EnumElementPattern>(P)) {
        // `case .<func>(let <bind>) = <Subject>`
        initFromEnumPattern(DRE->getDecl(), EEP);
      }
    } else if (auto *OTE = dyn_cast<OptionalTryExpr>(Init)) {
      // `let bind = try? <Subject>.get()`
      if (auto *OSP = dyn_cast<OptionalSomePattern>(P))
        initFromOptionalTry(OSP->getSubPattern(), OTE);
    }
  }

  /// Initializes a `CallbackCondtion` from a case statement inside a switch
  /// on `Subject` with `Result` type, ie.
  /// ```
  /// switch <Subject> {
  /// case .success(let bind):
  /// case .failure(let bind):
  /// }
  /// ```
  CallbackCondition(const Decl *Subject, const CaseLabelItem *CaseItem) {
    if (auto *EEP = dyn_cast<EnumElementPattern>(
            CaseItem->getPattern()->getSemanticsProvidingPattern())) {
      // `case .<func>(let <bind>)`
      initFromEnumPattern(Subject, EEP);
    }
  }

  bool isValid() const { return Type.has_value(); }

private:
  void initFromEnumPattern(const Decl *D, const EnumElementPattern *EEP) {
    if (auto *EED = EEP->getElementDecl()) {
      auto eedTy = EED->getParentEnum()->getDeclaredType();
      if (!eedTy || !eedTy->isResult())
        return;
      if (EED->getNameStr() == StringRef("failure")) {
        Type = ConditionType::FAILURE_PATTEN;
      } else {
        Type = ConditionType::SUCCESS_PATTERN;
      }
      Subject = D;
      BindPattern = EEP->getSubPattern();
    }
  }

  void initFromOptionalTry(const class Pattern *P, const OptionalTryExpr *OTE) {
    auto *ICE = dyn_cast<ImplicitConversionExpr>(OTE->getSubExpr());
    if (!ICE)
      return;
    auto *CE = dyn_cast<CallExpr>(ICE->getSyntacticSubExpr());
    if (!CE)
      return;
    auto *DSC = dyn_cast<DotSyntaxCallExpr>(CE->getFn());
    if (!DSC)
      return;

    auto *BaseDRE = dyn_cast<DeclRefExpr>(DSC->getBase());
    if (!BaseDRE->getType() || !BaseDRE->getType()->isResult())
      return;

    auto *FnDRE = dyn_cast<DeclRefExpr>(DSC->getFn());
    if (!FnDRE)
      return;
    auto *FD = dyn_cast<FuncDecl>(FnDRE->getDecl());
    if (!FD || FD->getNameStr() != StringRef("get"))
      return;

    Type = ConditionType::NOT_NIL;
    Subject = BaseDRE->getDecl();
    BindPattern = P;
  }
};

/// A CallbackCondition with additional semantic information about whether it
/// is for a success path or failure path.
struct ClassifiedCondition : public CallbackCondition {
  ConditionPath Path;

  /// Whether this represents an Obj-C style boolean flag check for success.
  bool IsObjCStyleFlagCheck;

  explicit ClassifiedCondition(CallbackCondition Cond, ConditionPath Path,
                               bool IsObjCStyleFlagCheck)
      : CallbackCondition(Cond), Path(Path),
        IsObjCStyleFlagCheck(IsObjCStyleFlagCheck) {}
};

/// A wrapper for a map of parameter decls to their classified conditions, or
/// \c None if they are not present in any conditions.
struct ClassifiedCallbackConditions final
    : llvm::MapVector<const Decl *, ClassifiedCondition> {
  llvm::Optional<ClassifiedCondition> lookup(const Decl *D) const {
    auto Res = find(D);
    if (Res == end())
      return llvm::None;
    return Res->second;
  }
};

/// A list of nodes to print, along with a list of locations that may have
/// preceding comments attached, which also need printing. For example:
///
/// \code
/// if .random() {
///   // a
///   print("hello")
///   // b
/// }
/// \endcode
///
/// To print out the contents of the if statement body, we'll include the AST
/// node for the \c print call. This will also include the preceding comment
/// \c a, but won't include the comment \c b. To ensure the comment \c b gets
/// printed, the SourceLoc for the closing brace \c } is added as a possible
/// comment loc.
class NodesToPrint {
  SmallVector<ASTNode, 0> Nodes;
  SmallVector<SourceLoc, 2> PossibleCommentLocs;

public:
  NodesToPrint() {}
  NodesToPrint(ArrayRef<ASTNode> Nodes, ArrayRef<SourceLoc> PossibleCommentLocs)
      : Nodes(Nodes.begin(), Nodes.end()),
        PossibleCommentLocs(PossibleCommentLocs.begin(),
                            PossibleCommentLocs.end()) {}

  ArrayRef<ASTNode> getNodes() const { return Nodes; }
  ArrayRef<SourceLoc> getPossibleCommentLocs() const {
    return PossibleCommentLocs;
  }

  /// Add an AST node to print.
  void addNode(ASTNode Node) {
    // Note we skip vars as they'll be printed as a part of their
    // PatternBindingDecl.
    if (!Node.isDecl(DeclKind::Var))
      Nodes.push_back(Node);
  }

  /// Add a SourceLoc which may have a preceding comment attached. If so, the
  /// comment will be printed out at the appropriate location.
  void addPossibleCommentLoc(SourceLoc Loc) {
    if (Loc.isValid())
      PossibleCommentLocs.push_back(Loc);
  }

  /// Add all the nodes in the brace statement to the list of nodes to print.
  /// This should be preferred over adding the nodes manually as it picks up the
  /// end location of the brace statement as a possible comment loc, ensuring
  /// that we print any trailing comments in the brace statement.
  void addNodesInBraceStmt(BraceStmt *Brace) {
    for (auto Node : Brace->getElements())
      addNode(Node);

    // Ignore the end locations of implicit braces, as they're likely bogus.
    // e.g for a case statement, the r-brace loc points to the last token of the
    // last node in the body.
    if (!Brace->isImplicit())
      addPossibleCommentLoc(Brace->getRBraceLoc());
  }

  /// Add the nodes and comment locs from another NodesToPrint.
  void addNodes(NodesToPrint OtherNodes) {
    Nodes.append(OtherNodes.Nodes.begin(), OtherNodes.Nodes.end());
    PossibleCommentLocs.append(OtherNodes.PossibleCommentLocs.begin(),
                               OtherNodes.PossibleCommentLocs.end());
  }

  /// Whether the last recorded node is an explicit return or break statement.
  bool hasTrailingReturnOrBreak() const {
    if (Nodes.empty())
      return false;
    return (Nodes.back().isStmt(StmtKind::Return) ||
            Nodes.back().isStmt(StmtKind::Break)) &&
           !Nodes.back().isImplicit();
  }

  /// If the last recorded node is an explicit return or break statement that
  /// can be safely dropped, drop it from the list.
  void dropTrailingReturnOrBreakIfPossible() {
    if (!hasTrailingReturnOrBreak())
      return;

    auto *Node = Nodes.back().get<Stmt *>();

    // If this is a return statement with return expression, let's preserve it.
    if (auto *RS = dyn_cast<ReturnStmt>(Node)) {
      if (RS->hasResult())
        return;
    }

    // Remove the node from the list, but make sure to add it as a possible
    // comment loc to preserve any of its attached comments.
    Nodes.pop_back();
    addPossibleCommentLoc(Node->getStartLoc());
  }

  /// Returns a list of nodes to print in a brace statement. This picks up the
  /// end location of the brace statement as a possible comment loc, ensuring
  /// that we print any trailing comments in the brace statement.
  static NodesToPrint inBraceStmt(BraceStmt *stmt) {
    NodesToPrint Nodes;
    Nodes.addNodesInBraceStmt(stmt);
    return Nodes;
  }
};

/// The statements within the closure of call to a function taking a callback
/// are split into a `SuccessBlock` and `ErrorBlock` (`ClassifiedBlocks`).
/// This class stores the nodes for each block, as well as a mapping of
/// decls to any patterns they are used in.
class ClassifiedBlock {
  NodesToPrint Nodes;

  // A mapping of closure params to a list of patterns that bind them.
  using ParamPatternBindingsMap =
      llvm::MapVector<const Decl *, TinyPtrVector<const Pattern *>>;
  ParamPatternBindingsMap ParamPatternBindings;

public:
  const NodesToPrint &nodesToPrint() const { return Nodes; }

  /// Attempt to retrieve an existing bound name for a closure parameter, or
  /// an empty string if there's no suitable existing binding.
  StringRef boundName(const Decl *D) const {
    // Adopt the same name as the representative single pattern, if it only
    // binds a single var.
    if (auto *P = getSinglePatternFor(D)) {
      if (P->getSingleVar())
        return P->getBoundName().str();
    }
    return StringRef();
  }

  /// Checks whether a closure parameter can be represented by a single pattern
  /// that binds it. If the param is only bound by a single pattern, that will
  /// be returned. If there's a pattern with a single var that binds it, that
  /// will be returned, preferring a 'let' pattern to prefer out of line
  /// printing of 'var' patterns.
  const Pattern *getSinglePatternFor(const Decl *D) const {
    auto Iter = ParamPatternBindings.find(D);
    if (Iter == ParamPatternBindings.end())
      return nullptr;

    const auto &Patterns = Iter->second;
    if (Patterns.empty())
      return nullptr;
    if (Patterns.size() == 1)
      return Patterns[0];

    // If we have multiple patterns, search for the best single var pattern to
    // use, preferring a 'let' binding.
    const Pattern *FirstSingleVar = nullptr;
    for (auto *P : Patterns) {
      if (!P->getSingleVar())
        continue;

      if (!P->hasAnyMutableBindings())
        return P;

      if (!FirstSingleVar)
        FirstSingleVar = P;
    }
    return FirstSingleVar;
  }

  /// Retrieve any bound vars that are effectively aliases of a given closure
  /// parameter.
  llvm::SmallDenseSet<const Decl *> getAliasesFor(const Decl *D) const {
    auto Iter = ParamPatternBindings.find(D);
    if (Iter == ParamPatternBindings.end())
      return {};

    llvm::SmallDenseSet<const Decl *> Aliases;

    // The single pattern that we replace the decl with is always an alias.
    if (auto *P = getSinglePatternFor(D)) {
      if (auto *SingleVar = P->getSingleVar())
        Aliases.insert(SingleVar);
    }

    // Any other let bindings we have are also aliases.
    for (auto *P : Iter->second) {
      if (auto *SingleVar = P->getSingleVar()) {
        if (!P->hasAnyMutableBindings())
          Aliases.insert(SingleVar);
      }
    }
    return Aliases;
  }

  const ParamPatternBindingsMap &paramPatternBindings() const {
    return ParamPatternBindings;
  }

  void addNodesInBraceStmt(BraceStmt *Brace) {
    Nodes.addNodesInBraceStmt(Brace);
  }
  void addPossibleCommentLoc(SourceLoc Loc) {
    Nodes.addPossibleCommentLoc(Loc);
  }
  void addAllNodes(NodesToPrint OtherNodes) {
    Nodes.addNodes(std::move(OtherNodes));
  }

  void addNode(ASTNode Node) {
    Nodes.addNode(Node);
  }

  void addBinding(const ClassifiedCondition &FromCondition) {
    auto *P = FromCondition.BindPattern;
    if (!P)
      return;

    // Patterns that don't bind anything aren't interesting.
    SmallVector<VarDecl *, 2> Vars;
    P->collectVariables(Vars);
    if (Vars.empty())
      return;

    ParamPatternBindings[FromCondition.Subject].push_back(P);
  }

  void addAllBindings(const ClassifiedCallbackConditions &FromConditions) {
    for (auto &Entry : FromConditions)
      addBinding(Entry.second);
  }
};

/// The type of block rewritten code may be placed in.
enum class BlockKind {
  SUCCESS, ERROR, FALLBACK
};

/// A completion handler function parameter that is known to be a Bool flag
/// indicating success or failure.
struct KnownBoolFlagParam {
  const ParamDecl *Param;
  bool IsSuccessFlag;
};

/// A set of parameters for a completion callback closure.
class ClosureCallbackParams final {
  const AsyncHandlerParamDesc &HandlerDesc;
  ArrayRef<const ParamDecl *> AllParams;
  llvm::SetVector<const ParamDecl *> SuccessParams;
  const ParamDecl *ErrParam = nullptr;
  llvm::Optional<KnownBoolFlagParam> BoolFlagParam;

public:
  ClosureCallbackParams(const AsyncHandlerParamDesc &HandlerDesc,
                        const ClosureExpr *Closure)
      : HandlerDesc(HandlerDesc),
        AllParams(Closure->getParameters()->getArray()) {
    assert(AllParams.size() == HandlerDesc.params().size());
    assert(HandlerDesc.Type != HandlerType::RESULT || AllParams.size() == 1);

    SuccessParams.insert(AllParams.begin(), AllParams.end());
    if (HandlerDesc.HasError && HandlerDesc.Type == HandlerType::PARAMS)
      ErrParam = SuccessParams.pop_back_val();

    // Check to see if we have a known bool flag parameter.
    if (auto *AsyncAlt = HandlerDesc.Func->getAsyncAlternative()) {
      if (auto Conv = AsyncAlt->getForeignAsyncConvention()) {
        auto FlagIdx = Conv->completionHandlerFlagParamIndex();
        if (FlagIdx && *FlagIdx >= 0 && *FlagIdx < AllParams.size()) {
          auto IsSuccessFlag = Conv->completionHandlerFlagIsErrorOnZero();
          BoolFlagParam = {AllParams[*FlagIdx], IsSuccessFlag};
        }
      }
    }
  }

  /// Whether the closure has a particular parameter.
  bool hasParam(const ParamDecl *Param) const {
    return Param == ErrParam || SuccessParams.contains(Param);
  }

  /// Whether \p Param is a success param.
  bool isSuccessParam(const ParamDecl *Param) const {
    return SuccessParams.contains(Param);
  }

  /// Whether \p Param is a closure parameter that may be unwrapped. This
  /// includes optional parameters as well as \c Result parameters that may be
  /// unwrapped through e.g 'try? res.get()'.
  bool isUnwrappableParam(const ParamDecl *Param) const {
    if (!hasParam(Param))
      return false;
    if (getResultParam() == Param)
      return true;
    return HandlerDesc.shouldUnwrap(Param->getTypeInContext());
  }

  /// Whether \p Param is the known Bool parameter that indicates success or
  /// failure.
  bool isKnownBoolFlagParam(const ParamDecl *Param) const {
    if (auto BoolFlag = getKnownBoolFlagParam())
      return BoolFlag->Param == Param;
    return false;
  }

  /// Whether \p Param is a closure parameter that has a binding available in
  /// the async variant of the call for a particular \p Block.
  bool hasBinding(const ParamDecl *Param, BlockKind Block) const {
    switch (Block) {
    case BlockKind::SUCCESS:
      // Known bool flags get dropped from the imported async variant.
      if (isKnownBoolFlagParam(Param))
        return false;

      return isSuccessParam(Param);
    case BlockKind::ERROR:
      return Param == ErrParam;
    case BlockKind::FALLBACK:
      // We generally want to bind everything in the fallback case.
      return hasParam(Param);
    }
    llvm_unreachable("Unhandled case in switch");
  }

  /// Retrieve the parameters to bind in a given \p Block.
  TinyPtrVector<const ParamDecl *> getParamsToBind(BlockKind Block) {
    TinyPtrVector<const ParamDecl *> Result;
    for (auto *Param : AllParams) {
      if (hasBinding(Param, Block))
        Result.push_back(Param);
    }
    return Result;
  }

  /// If there is a known Bool flag parameter indicating success or failure,
  /// returns it, \c None otherwise.
  llvm::Optional<KnownBoolFlagParam> getKnownBoolFlagParam() const {
    return BoolFlagParam;
  }

  /// All the parameters of the closure passed as the completion handler.
  ArrayRef<const ParamDecl *> getAllParams() const { return AllParams; }

  /// The success parameters of the closure passed as the completion handler.
  /// Note this includes a \c Result parameter.
  ArrayRef<const ParamDecl *> getSuccessParams() const {
    return SuccessParams.getArrayRef();
  }

  /// The error parameter of the closure passed as the completion handler, or
  /// \c nullptr if there is no error parameter.
  const ParamDecl *getErrParam() const { return ErrParam; }

  /// If the closure has a single \c Result parameter, returns it, \c nullptr
  /// otherwise.
  const ParamDecl *getResultParam() const {
    return HandlerDesc.Type == HandlerType::RESULT ? SuccessParams[0] : nullptr;
  }
};

/// Whether or not the given statement starts a new scope. Note that most
/// statements are handled by the \c BraceStmt check. The others listed are
/// a somewhat special case since they can also declare variables in their
/// condition.
static bool startsNewScope(Stmt *S) {
  switch (S->getKind()) {
  case StmtKind::Brace:
  case StmtKind::If:
  case StmtKind::While:
  case StmtKind::ForEach:
  case StmtKind::Case:
    return true;
  default:
    return false;
  }
}

struct ClassifiedBlocks {
  ClassifiedBlock SuccessBlock;
  ClassifiedBlock ErrorBlock;
};

/// Classifer of callback closure statements that that have either multiple
/// non-Result parameters or a single Result parameter and return Void.
///
/// It performs a (possibly incorrect) best effort and may give up in certain
/// cases. Aims to cover the idiomatic cases of either having no error
/// parameter at all, or having success/error code wrapped in ifs/guards/switch
/// using either pattern binding or nil checks.
///
/// Code outside any clear conditions is assumed to be solely part of the
/// success block for now, though some heuristics could be added to classify
/// these better in the future.
struct CallbackClassifier {
  /// Updates the success and error block of `Blocks` with nodes and bound
  /// names from `Body`. Errors are added through `DiagEngine`, possibly
  /// resulting in partially filled out blocks.
  static void classifyInto(ClassifiedBlocks &Blocks,
                           const ClosureCallbackParams &Params,
                           llvm::DenseSet<SwitchStmt *> &HandledSwitches,
                           DiagnosticEngine &DiagEngine, BraceStmt *Body) {
    assert(!Body->getElements().empty() && "Cannot classify empty body");
    CallbackClassifier Classifier(Blocks, Params, HandledSwitches, DiagEngine);
    Classifier.classifyNodes(Body->getElements(), Body->getRBraceLoc());
  }

private:
  ClassifiedBlocks &Blocks;
  const ClosureCallbackParams &Params;
  llvm::DenseSet<SwitchStmt *> &HandledSwitches;
  DiagnosticEngine &DiagEngine;
  ClassifiedBlock *CurrentBlock;

  /// This is set to \c true if we're currently classifying on a known condition
  /// path, where \c CurrentBlock is set to the appropriate block. This lets us
  /// be more lenient with unhandled conditions as we already know the block
  /// we're supposed to be in.
  bool IsKnownConditionPath = false;

  CallbackClassifier(ClassifiedBlocks &Blocks,
                     const ClosureCallbackParams &Params,
                     llvm::DenseSet<SwitchStmt *> &HandledSwitches,
                     DiagnosticEngine &DiagEngine)
      : Blocks(Blocks), Params(Params), HandledSwitches(HandledSwitches),
        DiagEngine(DiagEngine), CurrentBlock(&Blocks.SuccessBlock) {}

  /// Attempt to apply custom classification logic to a given node, returning
  /// \c true if the node was classified, otherwise \c false.
  bool tryClassifyNode(ASTNode Node) {
    auto *Statement = Node.dyn_cast<Stmt *>();
    if (!Statement)
      return false;

    if (auto *IS = dyn_cast<IfStmt>(Statement)) {
      NodesToPrint TempNodes;
      if (auto *BS = dyn_cast<BraceStmt>(IS->getThenStmt())) {
        TempNodes = NodesToPrint::inBraceStmt(BS);
      } else {
        TempNodes = NodesToPrint({IS->getThenStmt()}, /*commentLocs*/ {});
      }

      classifyConditional(IS, IS->getCond(), std::move(TempNodes),
                          IS->getElseStmt());
      return true;
    } else if (auto *GS = dyn_cast<GuardStmt>(Statement)) {
      classifyConditional(GS, GS->getCond(), NodesToPrint(), GS->getBody());
      return true;
    } else if (auto *SS = dyn_cast<SwitchStmt>(Statement)) {
      classifySwitch(SS);
      return true;
    } else if (auto *RS = dyn_cast<ReturnStmt>(Statement)) {
      // We can look through an implicit Void return of a SingleValueStmtExpr,
      // as that's semantically a statement.
      if (RS->hasResult() && RS->isImplicit()) {
        auto Ty = RS->getResult()->getType();
        if (Ty && Ty->isVoid()) {
          if (auto *SVE = dyn_cast<SingleValueStmtExpr>(RS->getResult()))
            return tryClassifyNode(SVE->getStmt());
        }
      }
    }
    return false;
  }

  /// Classify a node, or add the node to the block if it cannot be classified.
  /// Returns \c true if there was an error.
  bool classifyNode(ASTNode Node) {
    auto DidClassify = tryClassifyNode(Node);
    if (!DidClassify)
      CurrentBlock->addNode(Node);
    return DiagEngine.hadAnyError();
  }

  void classifyNodes(ArrayRef<ASTNode> Nodes, SourceLoc EndCommentLoc) {
    for (auto Node : Nodes) {
      auto HadError = classifyNode(Node);
      if (HadError)
        return;
    }
    // Make sure to pick up any trailing comments.
    CurrentBlock->addPossibleCommentLoc(EndCommentLoc);
  }

  /// Whether any of the provided ASTNodes have a child expression that force
  /// unwraps the error parameter. Note that this doesn't walk into new scopes.
  bool hasForceUnwrappedErrorParam(ArrayRef<ASTNode> Nodes) {
    auto *ErrParam = Params.getErrParam();
    if (!ErrParam)
      return false;

    class ErrUnwrapFinder : public ASTWalker {
      const ParamDecl *ErrParam;
      bool FoundUnwrap = false;

    public:
      explicit ErrUnwrapFinder(const ParamDecl *ErrParam)
          : ErrParam(ErrParam) {}
      bool foundUnwrap() const { return FoundUnwrap; }

      MacroWalking getMacroWalkingBehavior() const override {
        return MacroWalking::Arguments;
      }

      PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
        // Don't walk into ternary conditionals as they may have additional
        // conditions such as err != nil that make a force unwrap now valid.
        if (isa<TernaryExpr>(E))
          return Action::SkipChildren(E);

        auto *FVE = dyn_cast<ForceValueExpr>(E);
        if (!FVE)
          return Action::Continue(E);

        auto *DRE = dyn_cast<DeclRefExpr>(FVE->getSubExpr());
        if (!DRE)
          return Action::Continue(E);

        if (DRE->getDecl() != ErrParam)
          return Action::Continue(E);

        // If we find the node we're looking for, make a note of it, and abort
        // the walk.
        FoundUnwrap = true;
        return Action::Stop();
      }

      PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
        // Don't walk into new explicit scopes, we only want to consider force
        // unwraps in the immediate conditional body.
        if (!S->isImplicit() && startsNewScope(S))
          return Action::SkipChildren(S);
        return Action::Continue(S);
      }

      PreWalkAction walkToDeclPre(Decl *D) override {
        // Don't walk into new explicit DeclContexts.
        return Action::VisitChildrenIf(D->isImplicit() || !isa<DeclContext>(D));
      }
    };
    for (auto Node : Nodes) {
      ErrUnwrapFinder walker(ErrParam);
      Node.walk(walker);
      if (walker.foundUnwrap())
        return true;
    }
    return false;
  }

  /// Given a callback condition, classify it as a success or failure path.
  llvm::Optional<ClassifiedCondition>
  classifyCallbackCondition(const CallbackCondition &Cond,
                            const NodesToPrint &SuccessNodes, Stmt *ElseStmt) {
    if (!Cond.isValid())
      return llvm::None;

    // If the condition involves a refutable pattern, we can't currently handle
    // it.
    if (Cond.BindPattern && Cond.BindPattern->isRefutablePattern())
      return llvm::None;

    auto *SubjectParam = dyn_cast<ParamDecl>(Cond.Subject);
    if (!SubjectParam)
      return llvm::None;

    // For certain types of condition, they need to be certain kinds of params.
    auto CondType = *Cond.Type;
    switch (CondType) {
    case ConditionType::NOT_NIL:
    case ConditionType::NIL:
      if (!Params.isUnwrappableParam(SubjectParam))
        return llvm::None;
      break;
    case ConditionType::IS_TRUE:
    case ConditionType::IS_FALSE:
      if (!Params.isSuccessParam(SubjectParam))
        return llvm::None;
      break;
    case ConditionType::SUCCESS_PATTERN:
    case ConditionType::FAILURE_PATTEN:
      if (SubjectParam != Params.getResultParam())
        return llvm::None;
      break;
    }

    // Let's start with a success path, and flip any negative conditions.
    auto Path = ConditionPath::SUCCESS;

    // If it's an error param, that's a flip.
    if (SubjectParam == Params.getErrParam())
      Path = flippedConditionPath(Path);

    // If we have a nil, false, or failure condition, that's a flip.
    switch (CondType) {
    case ConditionType::NIL:
    case ConditionType::IS_FALSE:
    case ConditionType::FAILURE_PATTEN:
      Path = flippedConditionPath(Path);
      break;
    case ConditionType::IS_TRUE:
    case ConditionType::NOT_NIL:
    case ConditionType::SUCCESS_PATTERN:
      break;
    }

    // If we have a bool condition, it could be an Obj-C style flag check, which
    // we do some extra checking for. Otherwise, we're done.
    if (CondType != ConditionType::IS_TRUE &&
        CondType != ConditionType::IS_FALSE) {
      return ClassifiedCondition(Cond, Path, /*ObjCFlagCheck*/ false);
    }

    // Check to see if we have a known bool flag parameter that indicates
    // success or failure.
    if (auto KnownBoolFlag = Params.getKnownBoolFlagParam()) {
      if (KnownBoolFlag->Param != SubjectParam)
        return llvm::None;

      // The path may need to be flipped depending on whether the flag indicates
      // success.
      if (!KnownBoolFlag->IsSuccessFlag)
        Path = flippedConditionPath(Path);

      return ClassifiedCondition(Cond, Path, /*ObjCStyleFlagCheck*/ true);
    }

    // If we've reached here, we have a bool flag check that isn't specified in
    // the async convention. We apply a heuristic to see if the error param is
    // force unwrapped in the conditional body. In that case, the user is
    // expecting it to be the error path, and it's more likely than not that the
    // flag value conveys no more useful information in the error block.

    // First check the success block.
    auto FoundInSuccessBlock =
        hasForceUnwrappedErrorParam(SuccessNodes.getNodes());

    // Then check the else block if we have it.
    if (ASTNode ElseNode = ElseStmt) {
      // Unwrap the BraceStmt of the else clause if needed. This is needed as
      // we won't walk into BraceStmts by default as they introduce new
      // scopes.
      ArrayRef<ASTNode> Nodes;
      if (auto *BS = dyn_cast<BraceStmt>(ElseStmt)) {
        Nodes = BS->getElements();
      } else {
        Nodes = llvm::makeArrayRef(ElseNode);
      }
      if (hasForceUnwrappedErrorParam(Nodes)) {
        // If we also found an unwrap in the success block, we don't know what's
        // happening here.
        if (FoundInSuccessBlock)
          return llvm::None;

        // Otherwise we can determine this as a success condition. Note this is
        // flipped as if the error is present in the else block, this condition
        // is for success.
        return ClassifiedCondition(Cond, ConditionPath::SUCCESS,
                                   /*ObjCStyleFlagCheck*/ true);
      }
    }

    if (FoundInSuccessBlock) {
      // Note that the path is flipped as if the error is present in the success
      // block, this condition is for failure.
      return ClassifiedCondition(Cond, ConditionPath::FAILURE,
                                 /*ObjCStyleFlagCheck*/ true);
    }

    // Otherwise we can't classify this.
    return llvm::None;
  }

  /// Classifies all the conditions present in a given StmtCondition, taking
  /// into account its success body and failure body. Returns \c true if there
  /// were any conditions that couldn't be classified, \c false otherwise.
  bool classifyConditionsOf(StmtCondition Cond,
                            const NodesToPrint &ThenNodesToPrint,
                            Stmt *ElseStmt,
                            ClassifiedCallbackConditions &Conditions) {
    bool UnhandledConditions = false;
    llvm::Optional<ClassifiedCondition> ObjCFlagCheck;
    auto TryAddCond = [&](CallbackCondition CC) {
      auto Classified =
          classifyCallbackCondition(CC, ThenNodesToPrint, ElseStmt);

      // If we couldn't classify this, or if there are multiple Obj-C style flag
      // checks, this is unhandled.
      if (!Classified || (ObjCFlagCheck && Classified->IsObjCStyleFlagCheck)) {
        UnhandledConditions = true;
        return;
      }

      // If we've seen multiple conditions for the same subject, don't handle
      // this.
      if (!Conditions.insert({CC.Subject, *Classified}).second) {
        UnhandledConditions = true;
        return;
      }

      if (Classified->IsObjCStyleFlagCheck)
        ObjCFlagCheck = Classified;
    };

    for (auto &CondElement : Cond) {
      if (auto *BoolExpr = CondElement.getBooleanOrNull()) {
        SmallVector<Expr *, 1> Exprs;
        Exprs.push_back(BoolExpr);

        while (!Exprs.empty()) {
          auto *Next = Exprs.pop_back_val()->getSemanticsProvidingExpr();
          if (auto *ACE = dyn_cast<AutoClosureExpr>(Next))
            Next = ACE->getSingleExpressionBody()->getSemanticsProvidingExpr();

          if (auto *BE = dyn_cast_or_null<BinaryExpr>(Next)) {
            auto *Operator = isOperator(BE);
            if (Operator) {
              // If we have an && operator, decompose its arguments.
              if (Operator->getBaseName() == "&&") {
                Exprs.push_back(BE->getLHS());
                Exprs.push_back(BE->getRHS());
              } else {
                // Otherwise check to see if we have an == nil or != nil
                // condition.
                TryAddCond(CallbackCondition(BE, Operator));
              }
              continue;
            }
          }

          // Check to see if we have a lone bool condition.
          TryAddCond(CallbackCondition(Next));
        }
      } else if (auto *P = CondElement.getPatternOrNull()) {
        TryAddCond(CallbackCondition(P, CondElement.getInitializer()));
      }
    }
    return UnhandledConditions || Conditions.empty();
  }

  /// Classifies the conditions of a conditional statement, and adds the
  /// necessary nodes to either the success or failure block.
  void classifyConditional(Stmt *Statement, StmtCondition Condition,
                           NodesToPrint ThenNodesToPrint, Stmt *ElseStmt) {
    ClassifiedCallbackConditions CallbackConditions;
    bool UnhandledConditions = classifyConditionsOf(
        Condition, ThenNodesToPrint, ElseStmt, CallbackConditions);
    auto ErrCondition = CallbackConditions.lookup(Params.getErrParam());

    if (UnhandledConditions) {
      // Some unknown conditions. If there's an else, assume we can't handle
      // and use the fallback case. Otherwise add to either the success or
      // error block depending on some heuristics, known conditions will have
      // placeholders added (ideally we'd remove them)
      // TODO: Remove known conditions and split the `if` statement

      if (IsKnownConditionPath) {
        // If we're on a known condition path, we can be lenient as we already
        // know what block we're in and can therefore just add the conditional
        // straight to it.
        CurrentBlock->addNode(Statement);
      } else if (CallbackConditions.empty()) {
        // Technically this has a similar problem, ie. the else could have
        // conditions that should be in either success/error
        CurrentBlock->addNode(Statement);
      } else if (ElseStmt) {
        DiagEngine.diagnose(Statement->getStartLoc(),
                            diag::unknown_callback_conditions);
      } else if (ErrCondition && ErrCondition->Path == ConditionPath::FAILURE) {
        Blocks.ErrorBlock.addNode(Statement);
      } else {
        for (auto &Entry : CallbackConditions) {
          if (Entry.second.Path == ConditionPath::FAILURE) {
            Blocks.ErrorBlock.addNode(Statement);
            return;
          }
        }
        Blocks.SuccessBlock.addNode(Statement);
      }
      return;
    }

    // If all the conditions were classified, make sure they're all consistently
    // on the success or failure path.
    llvm::Optional<ConditionPath> Path;
    for (auto &Entry : CallbackConditions) {
      auto &Cond = Entry.second;
      if (!Path) {
        Path = Cond.Path;
      } else if (*Path != Cond.Path) {
        // Similar to the unknown conditions case. Add the whole if unless
        // there's an else, in which case use the fallback instead.
        // TODO: Split the `if` statement

        if (ElseStmt) {
          DiagEngine.diagnose(Statement->getStartLoc(),
                              diag::mixed_callback_conditions);
        } else {
          CurrentBlock->addNode(Statement);
        }
        return;
      }
    }
    assert(Path && "Didn't classify a path?");

    auto *ThenBlock = &Blocks.SuccessBlock;
    auto *ElseBlock = &Blocks.ErrorBlock;

    // If the condition is for a failure path, the error block is ThenBlock, and
    // the success block is ElseBlock.
    if (*Path == ConditionPath::FAILURE)
      std::swap(ThenBlock, ElseBlock);

    // We'll be dropping the statement, but make sure to keep any attached
    // comments.
    CurrentBlock->addPossibleCommentLoc(Statement->getStartLoc());

    ThenBlock->addAllBindings(CallbackConditions);

    // TODO: Handle nested ifs
    setNodes(ThenBlock, ElseBlock, std::move(ThenNodesToPrint));

    if (ElseStmt) {
      if (auto *BS = dyn_cast<BraceStmt>(ElseStmt)) {
        // If this is a guard statement, we know that we'll always exit,
        // allowing us to classify any additional nodes into the opposite block.
        auto AlwaysExits = isa<GuardStmt>(Statement);
        setNodes(ElseBlock, ThenBlock, NodesToPrint::inBraceStmt(BS),
                 AlwaysExits);
      } else {
        // If we reached here, we should have an else if statement. Given we
        // know we're in the else of a known condition, temporarily flip the
        // current block, and set that we know what path we're on.
        llvm::SaveAndRestore<bool> CondScope(IsKnownConditionPath, true);
        llvm::SaveAndRestore<ClassifiedBlock *> BlockScope(CurrentBlock,
                                                           ElseBlock);
        classifyNodes(ArrayRef<ASTNode>(ElseStmt),
                      /*endCommentLoc*/ SourceLoc());
      }
    }
  }

  /// Adds \p Nodes to \p Block, potentially flipping the current block if we
  /// can determine that the nodes being added will cause control flow to leave
  /// the scope.
  ///
  /// \param Block The block to add the nodes to.
  /// \param OtherBlock The block for the opposing condition path.
  /// \param Nodes The nodes to add.
  /// \param AlwaysExitsScope Whether the nodes being added always exit the
  /// scope, and therefore whether the current block should be flipped.
  void setNodes(ClassifiedBlock *Block, ClassifiedBlock *OtherBlock,
                NodesToPrint Nodes, bool AlwaysExitsScope = false) {
    // Drop an explicit trailing 'return' or 'break' if we can.
    bool HasTrailingReturnOrBreak = Nodes.hasTrailingReturnOrBreak();
    if (HasTrailingReturnOrBreak)
      Nodes.dropTrailingReturnOrBreakIfPossible();

    // If we know we're exiting the scope, we can set IsKnownConditionPath, as
    // we know any future nodes should be classified into the other block.
    if (HasTrailingReturnOrBreak || AlwaysExitsScope) {
      CurrentBlock = OtherBlock;
      IsKnownConditionPath = true;
      Block->addAllNodes(std::move(Nodes));
    } else {
      Block->addAllNodes(std::move(Nodes));
    }
  }

  void classifySwitch(SwitchStmt *SS) {
    auto *ResultParam = Params.getResultParam();
    if (singleSwitchSubject(SS) != ResultParam) {
      CurrentBlock->addNode(SS);
      return;
    }

    // We'll be dropping the switch, but make sure to keep any attached
    // comments.
    CurrentBlock->addPossibleCommentLoc(SS->getStartLoc());

    // Push the cases into a vector. This is only done to eagerly evaluate the
    // AsCaseStmtRange sequence so we can know what the last case is.
    SmallVector<CaseStmt *, 2> Cases;
    Cases.append(SS->getCases().begin(), SS->getCases().end());

    for (auto *CS : Cases) {
      if (CS->hasFallthroughDest()) {
        DiagEngine.diagnose(CS->getLoc(), diag::callback_with_fallthrough);
        return;
      }

      if (CS->isDefault()) {
        DiagEngine.diagnose(CS->getLoc(), diag::callback_with_default);
        return;
      }

      auto Items = CS->getCaseLabelItems();
      if (Items.size() > 1) {
        DiagEngine.diagnose(CS->getLoc(), diag::callback_multiple_case_items);
        return;
      }

      if (Items[0].getWhereLoc().isValid()) {
        DiagEngine.diagnose(CS->getLoc(), diag::callback_where_case_item);
        return;
      }

      auto *Block = &Blocks.SuccessBlock;
      auto *OtherBlock = &Blocks.ErrorBlock;
      auto SuccessNodes = NodesToPrint::inBraceStmt(CS->getBody());

      // Classify the case pattern.
      auto CC = classifyCallbackCondition(
          CallbackCondition(ResultParam, &Items[0]), SuccessNodes,
          /*elseStmt*/ nullptr);
      if (!CC) {
        DiagEngine.diagnose(CS->getLoc(), diag::unknown_callback_case_item);
        return;
      }

      if (CC->Path == ConditionPath::FAILURE)
        std::swap(Block, OtherBlock);

      // We'll be dropping the case, but make sure to keep any attached
      // comments. Because these comments will effectively be part of the
      // previous case, add them to CurrentBlock.
      CurrentBlock->addPossibleCommentLoc(CS->getStartLoc());

      // Make sure to grab trailing comments in the last case stmt.
      if (CS == Cases.back())
        Block->addPossibleCommentLoc(SS->getRBraceLoc());

      setNodes(Block, OtherBlock, std::move(SuccessNodes));
      Block->addBinding(*CC);
    }
    // Mark this switch statement as having been transformed.
    HandledSwitches.insert(SS);
  }
};

/// Base name of a decl if it has one, an empty \c DeclBaseName otherwise.
static DeclBaseName getDeclName(const Decl *D) {
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    if (VD->hasName())
      return VD->getBaseName();
  }
  return DeclBaseName();
}

class DeclCollector : private SourceEntityWalker {
  llvm::DenseSet<const Decl *> &Decls;

public:
  /// Collect all explicit declarations declared in \p Scope (or \p SF if
  /// \p Scope is a nullptr) that are not within their own scope.
  static void collect(BraceStmt *Scope, SourceFile &SF,
                      llvm::DenseSet<const Decl *> &Decls) {
    DeclCollector Collector(Decls);
    if (Scope) {
      for (auto Node : Scope->getElements()) {
        Collector.walk(Node);
      }
    } else {
      Collector.walk(SF);
    }
  }

private:
  DeclCollector(llvm::DenseSet<const Decl *> &Decls)
      : Decls(Decls) {}

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    // Want to walk through top level code decls (which are implicitly added
    // for top level non-decl code) and pattern binding decls (which contain
    // the var decls that we care about).
    if (isa<TopLevelCodeDecl>(D) || isa<PatternBindingDecl>(D))
      return true;

    if (!D->isImplicit())
      Decls.insert(D);
    return false;
  }

  bool walkToExprPre(Expr *E) override {
    return !isa<ClosureExpr>(E);
  }

  bool walkToStmtPre(Stmt *S) override {
    return S->isImplicit() || !startsNewScope(S);
  }
};

class ReferenceCollector : private SourceEntityWalker {
  SourceManager *SM;
  llvm::DenseSet<const Decl *> DeclaredDecls;
  llvm::DenseSet<const Decl *> &ReferencedDecls;

  ASTNode Target;
  bool AfterTarget;

public:
  /// Collect all explicit references in \p Scope (or \p SF if \p Scope is
  /// a nullptr) that are after \p Target and not first declared. That is,
  /// references that we don't want to shadow with hoisted declarations.
  ///
  /// Also collect all declarations that are \c DeclContexts, which is an
  /// over-appoximation but let's us ignore them elsewhere.
  static void collect(ASTNode Target, BraceStmt *Scope, SourceFile &SF,
                      llvm::DenseSet<const Decl *> &Decls) {
    ReferenceCollector Collector(Target, &SF.getASTContext().SourceMgr,
                                 Decls);
    if (Scope)
      Collector.walk(Scope);
    else
      Collector.walk(SF);
  }

private:
  ReferenceCollector(ASTNode Target, SourceManager *SM,
                     llvm::DenseSet<const Decl *> &Decls)
      : SM(SM), DeclaredDecls(), ReferencedDecls(Decls), Target(Target),
        AfterTarget(false) {}

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    // Bit of a hack, include all contexts so they're never renamed (seems worse
    // to rename a class/function than it does a variable). Again, an
    // over-approximation, but hopefully doesn't come up too often.
    if (isa<DeclContext>(D) && !D->isImplicit()) {
      ReferencedDecls.insert(D);
    }

    if (AfterTarget && !D->isImplicit()) {
      DeclaredDecls.insert(D);
    } else if (D == Target.dyn_cast<Decl *>()) {
      AfterTarget = true;
    }
    return shouldWalkInto(D->getSourceRange());
  }

  bool walkToExprPre(Expr *E) override {
    if (AfterTarget && !E->isImplicit()) {
      if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
        if (auto *D = DRE->getDecl()) {
          // Only care about references that aren't declared, as seen decls will
          // be renamed (if necessary) during the refactoring.
          if (!D->isImplicit() && !DeclaredDecls.count(D)) {
            ReferencedDecls.insert(D);

            // Also add the async alternative of a function to prevent
            // collisions if a call is replaced with the alternative.
            if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
              if (auto *Alternative = AFD->getAsyncAlternative())
                ReferencedDecls.insert(Alternative);
            }
          }
        }
      }
    } else if (E == Target.dyn_cast<Expr *>()) {
      AfterTarget = true;
    }
    return shouldWalkInto(E->getSourceRange());
  }

  bool walkToStmtPre(Stmt *S) override {
    if (S == Target.dyn_cast<Stmt *>())
      AfterTarget = true;
    return shouldWalkInto(S->getSourceRange());
  }

  bool walkToPatternPre(Pattern *P) override {
    if (P == Target.dyn_cast<Pattern *>())
      AfterTarget = true;
    return shouldWalkInto(P->getSourceRange());
  }

  bool shouldWalkInto(SourceRange Range) {
    return AfterTarget || (SM &&
        SM->rangeContainsTokenLoc(Range, Target.getStartLoc()));
  }
};

/// Similar to the \c ReferenceCollector but collects references in all scopes
/// without any starting point in each scope. In addition, it tracks the number
/// of references to a decl in a given scope.
class ScopedDeclCollector : private SourceEntityWalker {
public:
  using DeclsTy = llvm::DenseSet<const Decl *>;
  using RefDeclsTy = llvm::DenseMap<const Decl *, /*numRefs*/ unsigned>;

private:
  using ScopedDeclsTy = llvm::DenseMap<const Stmt *, RefDeclsTy>;

  struct Scope {
    DeclsTy DeclaredDecls;
    RefDeclsTy *ReferencedDecls;
    Scope(RefDeclsTy *ReferencedDecls) : DeclaredDecls(),
        ReferencedDecls(ReferencedDecls) {}
  };

  ScopedDeclsTy ReferencedDecls;
  llvm::SmallVector<Scope, 4> ScopeStack;

public:
  /// Starting at \c Scope, collect all explicit references in every scope
  /// within (including the initial) that are not first declared, ie. those that
  /// could end up shadowed. Also include all \c DeclContext declarations as
  /// we'd like to avoid renaming functions and types completely.
  void collect(ASTNode Node) {
    walk(Node);
  }

  const RefDeclsTy *getReferencedDecls(Stmt *Scope) const {
    auto Res = ReferencedDecls.find(Scope);
    if (Res == ReferencedDecls.end())
      return nullptr;
    return &Res->second;
  }

private:
  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (ScopeStack.empty() || D->isImplicit())
      return true;

    ScopeStack.back().DeclaredDecls.insert(D);
    if (isa<DeclContext>(D))
      (*ScopeStack.back().ReferencedDecls)[D] += 1;
    return true;
  }

  bool walkToExprPre(Expr *E) override {
    if (ScopeStack.empty())
      return true;

    if (!E->isImplicit()) {
      if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
        if (auto *D = DRE->getDecl()) {
          // If we have a reference that isn't declared in the same scope,
          // increment the number of references to that decl.
          if (!D->isImplicit() && !ScopeStack.back().DeclaredDecls.count(D)) {
            (*ScopeStack.back().ReferencedDecls)[D] += 1;

            // Also add the async alternative of a function to prevent
            // collisions if a call is replaced with the alternative.
            if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
              if (auto *Alternative = AFD->getAsyncAlternative())
                (*ScopeStack.back().ReferencedDecls)[Alternative] += 1;
            }
          }
        }
      }
    }
    return true;
  }

  bool walkToStmtPre(Stmt *S) override {
    // Purposely check \c BraceStmt here rather than \c startsNewScope.
    // References in the condition should be applied to the previous scope, not
    // the scope of that statement.
    if (isa<BraceStmt>(S))
      ScopeStack.emplace_back(&ReferencedDecls[S]);
    return true;
  }

  bool walkToStmtPost(Stmt *S) override {
    if (isa<BraceStmt>(S)) {
      size_t NumScopes = ScopeStack.size();
      if (NumScopes >= 2) {
        // Add any referenced decls to the parent scope that weren't declared
        // there.
        auto &ParentStack = ScopeStack[NumScopes - 2];
        for (auto DeclAndNumRefs : *ScopeStack.back().ReferencedDecls) {
          auto *D = DeclAndNumRefs.first;
          if (!ParentStack.DeclaredDecls.count(D))
            (*ParentStack.ReferencedDecls)[D] += DeclAndNumRefs.second;
        }
      }
      ScopeStack.pop_back();
    }
    return true;
  }
};

/// Checks whether an ASTNode contains a reference to a given declaration.
class DeclReferenceFinder : private SourceEntityWalker {
  bool HasFoundReference = false;
  const Decl *Search;

  bool walkToExprPre(Expr *E) override {
    if (auto DRE = dyn_cast<DeclRefExpr>(E)) {
      if (DRE->getDecl() == Search) {
        HasFoundReference = true;
        return false;
      }
    }
    return true;
  }

  DeclReferenceFinder(const Decl *Search) : Search(Search) {}

public:
  /// Returns \c true if \p node contains a reference to \p Search, \c false
  /// otherwise.
  static bool containsReference(ASTNode Node, const ValueDecl *Search) {
    DeclReferenceFinder Checker(Search);
    Checker.walk(Node);
    return Checker.HasFoundReference;
  }
};

/// Builds up async-converted code for an AST node.
///
/// If it is a function, its declaration will have `async` added. If a
/// completion handler is present, it will be removed and the return type of
/// the function will reflect the parameters of the handler, including an
/// added `throws` if necessary.
///
/// Calls to the completion handler are replaced with either a `return` or
/// `throws` depending on the arguments.
///
/// Calls to functions with an async alternative will be replaced with a call
/// to the alternative, possibly wrapped in a do/catch. The do/catch is skipped
/// if the the closure either:
///   1. Has no error
///   2. Has an error but no error handling (eg. just ignores)
///   3. Has error handling that only calls the containing function's handler
///      with an error matching the error argument
///
/// (2) is technically not the correct translation, but in practice it's likely
/// the code a user would actually want.
///
/// If the success vs error handling split inside the closure cannot be
/// determined and the closure takes regular parameters (ie. not a Result), a
/// fallback translation is used that keeps all the same variable names and
/// simply moves the code within the closure out.
///
/// The fallback is generally avoided, however, since it's quite unlikely to be
/// the code the user intended. In most cases the refactoring will continue,
/// with any unhandled decls wrapped in placeholders instead.
class AsyncConverter : private SourceEntityWalker {
  struct Scope {
    llvm::DenseSet<DeclBaseName> Names;

    /// If this scope is wrapped in a \c withChecked(Throwing)Continuation, the
    /// name of the continuation that must be resumed where there previously was
    /// a call to the function's completion handler.
    /// Otherwise an empty identifier.
    Identifier ContinuationName;

    Scope(Identifier ContinuationName)
        : Names(), ContinuationName(ContinuationName) {}

    /// Whether this scope is wrapped in a \c withChecked(Throwing)Continuation.
    bool isWrappedInContination() const { return !ContinuationName.empty(); }
  };
  SourceFile *SF;
  SourceManager &SM;
  DiagnosticEngine &DiagEngine;

  // Node to convert
  ASTNode StartNode;

  // Completion handler of `StartNode` (if it's a function with an async
  // alternative)
  AsyncHandlerParamDesc TopHandler;

  SmallString<0> Buffer;
  llvm::raw_svector_ostream OS;

  // Decls where any force unwrap or optional chain of that decl should be
  // elided, e.g for a previously optional closure parameter that has become a
  // non-optional local.
  llvm::DenseSet<const Decl *> Unwraps;

  // Decls whose references should be replaced with, either because they no
  // longer exist or are a different type. Any replaced code should ideally be
  // handled by the refactoring properly, but that's not possible in all cases
  llvm::DenseSet<const Decl *> Placeholders;

  // Mapping from decl -> name, used as the name of possible new local
  // declarations of old completion handler parametes, as well as the
  // replacement for other hoisted declarations and their references
  llvm::DenseMap<const Decl *, Identifier> Names;

  /// The scopes (containing all name decls and whether the scope is wrapped in
  /// a continuation) as the AST is being walked. The first element is the
  /// initial scope and the last is the current scope.
  llvm::SmallVector<Scope, 4> Scopes;

  // Mapping of \c BraceStmt -> declarations referenced in that statement
  // without first being declared. These are used to fill the \c ScopeNames
  // map on entering that scope.
  ScopedDeclCollector ScopedDecls;

  /// The switch statements that have been re-written by this transform.
  llvm::DenseSet<SwitchStmt *> HandledSwitches;

  // The last source location that has been output. Used to output the source
  // between handled nodes
  SourceLoc LastAddedLoc;

  // Number of expressions (or pattern binding decl) currently nested in, taking
  // into account hoisting and the possible removal of ifs/switches
  int NestedExprCount = 0;

  // Whether a completion handler body is currently being hoisted out of its
  // call
  bool Hoisting = false;

  /// Whether a pattern is currently being converted.
  bool ConvertingPattern = false;

  /// A mapping of inline patterns to print for closure parameters.
  using InlinePatternsToPrint = llvm::DenseMap<const Decl *, const Pattern *>;

public:
  /// Convert a function
  AsyncConverter(SourceFile *SF, SourceManager &SM,
                 DiagnosticEngine &DiagEngine, AbstractFunctionDecl *FD,
                 const AsyncHandlerParamDesc &TopHandler)
      : SF(SF), SM(SM), DiagEngine(DiagEngine), StartNode(FD),
        TopHandler(TopHandler), OS(Buffer) {
    Placeholders.insert(TopHandler.getHandler());
    ScopedDecls.collect(FD);

    // Shouldn't strictly be necessary, but prefer possible shadowing over
    // crashes caused by a missing scope
    addNewScope({});
  }

  /// Convert a call
  AsyncConverter(SourceFile *SF, SourceManager &SM,
                 DiagnosticEngine &DiagEngine, CallExpr *CE, BraceStmt *Scope)
      : SF(SF), SM(SM), DiagEngine(DiagEngine), StartNode(CE), OS(Buffer) {
    ScopedDecls.collect(CE);

    // Create the initial scope, can be more accurate than the general
    // \c ScopedDeclCollector as there is a starting point.
    llvm::DenseSet<const Decl *> UsedDecls;
    DeclCollector::collect(Scope, *SF, UsedDecls);
    ReferenceCollector::collect(StartNode, Scope, *SF, UsedDecls);
    addNewScope(UsedDecls);
  }

  ASTContext &getASTContext() const { return SF->getASTContext(); }

  bool convert() {
    assert(Buffer.empty() && "AsyncConverter can only be used once");

    if (auto *FD = dyn_cast_or_null<FuncDecl>(StartNode.dyn_cast<Decl *>())) {
      addFuncDecl(FD);
      if (FD->getBody()) {
        convertNode(FD->getBody());
      }
    } else {
      convertNode(StartNode, /*StartOverride=*/{}, /*ConvertCalls=*/true,
                  /*IncludeComments=*/false);
    }
    return !DiagEngine.hadAnyError();
  }

  /// When adding an async alternative method for the function declaration \c
  /// FD, this function tries to create a function body for the legacy function
  /// (the one with a completion handler), which calls the newly converted async
  /// function. There are certain situations in which we fail to create such a
  /// body, e.g. if the completion handler has the signature `(String, Error?)
  /// -> Void` in which case we can't synthesize the result of type \c String in
  /// the error case.
  bool createLegacyBody() {
    assert(Buffer.empty() &&
           "AsyncConverter can only be used once");

    if (!canCreateLegacyBody())
      return false;

    FuncDecl *FD = cast<FuncDecl>(StartNode.get<Decl *>());
    OS << tok::l_brace << "\n"; // start function body
    OS << "Task " << tok::l_brace << "\n";
    addHoistedNamedCallback(FD, TopHandler, TopHandler.getNameStr(), [&]() {
      if (TopHandler.HasError) {
        OS << tok::kw_try << " ";
      }
      OS << "await ";

      // Since we're *creating* the async alternative here, there shouldn't
      // already be one. Thus, just assume that the call to the alternative is
      // the same as the call to the old completion handler function, minus the
      // completion handler arg.
      addForwardingCallTo(FD, /*HandlerReplacement=*/"");
    });
    OS << "\n";
    OS << tok::r_brace << "\n"; // end 'Task'
    OS << tok::r_brace << "\n"; // end function body
    return true;
  }

  /// Creates an async alternative function that forwards onto the completion
  /// handler function through
  /// withCheckedContinuation/withCheckedThrowingContinuation.
  bool createAsyncWrapper() {
    assert(Buffer.empty() && "AsyncConverter can only be used once");
    auto *FD = cast<FuncDecl>(StartNode.get<Decl *>());

    // First add the new async function declaration.
    addFuncDecl(FD);
    OS << tok::l_brace << "\n";

    // Then add the body.
    OS << tok::kw_return << " ";
    if (TopHandler.HasError)
      OS << tok::kw_try << " ";

    OS << "await ";

    // withChecked[Throwing]Continuation { continuation in
    if (TopHandler.HasError) {
      OS << "withCheckedThrowingContinuation";
    } else {
      OS << "withCheckedContinuation";
    }
    OS << " " << tok::l_brace << " continuation " << tok::kw_in << "\n";

    // fnWithHandler(args...) { ... }
    auto ClosureStr =
        getAsyncWrapperCompletionClosure("continuation", TopHandler);
    addForwardingCallTo(FD, /*HandlerReplacement=*/ClosureStr);

    OS << "\n";
    OS << tok::r_brace << "\n"; // end continuation closure
    OS << tok::r_brace << "\n"; // end function body
    return true;
  }

  void replace(ASTNode Node, SourceEditConsumer &EditConsumer,
               SourceLoc StartOverride = SourceLoc()) {
    SourceRange Range = Node.getSourceRange();
    if (StartOverride.isValid()) {
      Range = SourceRange(StartOverride, Range.End);
    }
    CharSourceRange CharRange =
        Lexer::getCharSourceRangeFromSourceRange(SM, Range);
    EditConsumer.accept(SM, CharRange, Buffer.str());
    Buffer.clear();
  }

  void insertAfter(ASTNode Node, SourceEditConsumer &EditConsumer) {
    EditConsumer.insertAfter(SM, Node.getEndLoc(), "\n\n");
    EditConsumer.insertAfter(SM, Node.getEndLoc(), Buffer.str());
    Buffer.clear();
  }

private:
  bool canCreateLegacyBody() {
    FuncDecl *FD = dyn_cast<FuncDecl>(StartNode.dyn_cast<Decl *>());
    if (!FD) {
      return false;
    }
    if (FD == nullptr || FD->getBody() == nullptr) {
      return false;
    }
    if (FD->hasThrows()) {
      assert(!TopHandler.isValid() && "We shouldn't have found a handler desc "
                                       "if the original function throws");
      return false;
    }
    return TopHandler.isValid();
  }

  /// Prints a tuple of elements, or a lone single element if only one is
  /// present, using the provided printing function.
  template <typename Container, typename PrintFn>
  void addTupleOf(const Container &Elements, llvm::raw_ostream &OS,
                  PrintFn PrintElt) {
    if (Elements.size() == 1) {
      PrintElt(Elements[0]);
      return;
    }
    OS << tok::l_paren;
    llvm::interleave(Elements, PrintElt, [&]() { OS << tok::comma << " "; });
    OS << tok::r_paren;
  }

  /// Retrieve the completion handler closure argument for an async wrapper
  /// function.
  std::string
  getAsyncWrapperCompletionClosure(StringRef ContName,
                                   const AsyncHandlerParamDesc &HandlerDesc) {
    std::string OutputStr;
    llvm::raw_string_ostream OS(OutputStr);

    OS << tok::l_brace; // start closure

    // Prepare parameter names for the closure.
    auto SuccessParams = HandlerDesc.getSuccessParams();
    SmallVector<SmallString<4>, 2> SuccessParamNames;
    for (auto idx : indices(SuccessParams)) {
      SuccessParamNames.emplace_back("result");

      // If we have multiple success params, number them e.g res1, res2...
      if (SuccessParams.size() > 1)
        SuccessParamNames.back().append(std::to_string(idx + 1));
    }
    llvm::Optional<SmallString<4>> ErrName;
    if (HandlerDesc.getErrorParam())
      ErrName.emplace("error");

    auto HasAnyParams = !SuccessParamNames.empty() || ErrName;
    if (HasAnyParams)
      OS << " ";

    // res1, res2
    llvm::interleave(
        SuccessParamNames, [&](auto Name) { OS << Name; },
        [&]() { OS << tok::comma << " "; });

    // , err
    if (ErrName) {
      if (!SuccessParamNames.empty())
        OS << tok::comma << " ";

      OS << *ErrName;
    }
    if (HasAnyParams)
      OS << " " << tok::kw_in;

    OS << "\n";

    // The closure body.
    switch (HandlerDesc.Type) {
    case HandlerType::PARAMS: {
      // For a (Success?, Error?) -> Void handler, we do an if let on the error.
      if (ErrName) {
        // if let err = err {
        OS << tok::kw_if << " " << tok::kw_let << " ";
        OS << *ErrName << " " << tok::equal << " " << *ErrName << " ";
        OS << tok::l_brace << "\n";
        for (auto Idx : indices(SuccessParamNames)) {
          auto ParamTy = SuccessParams[Idx].getParameterType();
          if (!HandlerDesc.shouldUnwrap(ParamTy))
            continue;
        }

        // continuation.resume(throwing: err)
        OS << ContName << tok::period << "resume" << tok::l_paren;
        OS << "throwing" << tok::colon << " " << *ErrName;
        OS << tok::r_paren << "\n";

        // return }
        OS << tok::kw_return << "\n";
        OS << tok::r_brace << "\n";
      }

      // If we have any success params that we need to unwrap, insert a guard.
      for (auto Idx : indices(SuccessParamNames)) {
        auto &Name = SuccessParamNames[Idx];
        auto ParamTy = SuccessParams[Idx].getParameterType();
        if (!HandlerDesc.shouldUnwrap(ParamTy))
          continue;

        // guard let res = res else {
        OS << tok::kw_guard << " " << tok::kw_let << " ";
        OS << Name << " " << tok::equal << " " << Name << " " << tok::kw_else;
        OS << " " << tok::l_brace << "\n";

        // fatalError(...)
        OS << "fatalError" << tok::l_paren;
        OS << "\"Expected non-nil result '" << Name << "' for nil error\"";
        OS << tok::r_paren << "\n";

        // End guard.
        OS << tok::r_brace << "\n";
      }

      // continuation.resume(returning: (res1, res2, ...))
      OS << ContName << tok::period << "resume" << tok::l_paren;
      OS << "returning" << tok::colon << " ";
      addTupleOf(SuccessParamNames, OS, [&](auto Ref) { OS << Ref; });
      OS << tok::r_paren << "\n";
      break;
    }
    case HandlerType::RESULT: {
      // continuation.resume(with: res)
      assert(SuccessParamNames.size() == 1);
      OS << ContName << tok::period << "resume" << tok::l_paren;
      OS << "with" << tok::colon << " " << SuccessParamNames[0];
      OS << tok::r_paren << "\n";
      break;
    }
    case HandlerType::INVALID:
      llvm_unreachable("Should not have an invalid handler here");
    }

    OS << tok::r_brace; // end closure
    return OutputStr;
  }

  /// Retrieves the SourceRange of the preceding comment, or an invalid range if
  /// there is no preceding comment.
  CharSourceRange getPrecedingCommentRange(SourceLoc Loc) {
    auto Tokens = SF->getAllTokens();
    auto TokenIter = token_lower_bound(Tokens, Loc);
    if (TokenIter == Tokens.end() || !TokenIter->hasComment())
      return CharSourceRange();
    return TokenIter->getCommentRange();
  }

  /// Retrieves the location for the start of a comment attached to the token
  /// at the provided location, or the location itself if there is no comment.
  SourceLoc getLocIncludingPrecedingComment(SourceLoc Loc) {
    auto CommentRange = getPrecedingCommentRange(Loc);
    if (CommentRange.isInvalid())
      return Loc;
    return CommentRange.getStart();
  }

  /// If the provided SourceLoc has a preceding comment, print it out.
  void printCommentIfNeeded(SourceLoc Loc) {
    auto CommentRange = getPrecedingCommentRange(Loc);
    if (CommentRange.isValid())
      OS << "\n" << CommentRange.str();
  }

  void convertNodes(const NodesToPrint &ToPrint) {
    // Sort the possible comment locs in reverse order so we can pop them as we
    // go.
    SmallVector<SourceLoc, 2> CommentLocs;
    CommentLocs.append(ToPrint.getPossibleCommentLocs().begin(),
                       ToPrint.getPossibleCommentLocs().end());
    llvm::sort(CommentLocs.begin(), CommentLocs.end(), [](auto lhs, auto rhs) {
      return lhs.getOpaquePointerValue() > rhs.getOpaquePointerValue();
    });

    // First print the nodes we've been asked to print.
    for (auto Node : ToPrint.getNodes()) {
      // If we need to print comments, do so now.
      while (!CommentLocs.empty()) {
        auto CommentLoc = CommentLocs.back().getOpaquePointerValue();
        auto NodeLoc = Node.getStartLoc().getOpaquePointerValue();
        assert(CommentLoc != NodeLoc &&
               "Added node to both comment locs and nodes to print?");

        // If the comment occurs after the node, don't print now. Wait until
        // the right node comes along.
        if (CommentLoc > NodeLoc)
          break;

        printCommentIfNeeded(CommentLocs.pop_back_val());
      }
      OS << "\n";
      convertNode(Node);
    }

    // We're done printing nodes. Make sure to output the remaining comments.
    while (!CommentLocs.empty())
      printCommentIfNeeded(CommentLocs.pop_back_val());
  }

  void convertNode(ASTNode Node, SourceLoc StartOverride = {},
                   bool ConvertCalls = true,
                   bool IncludePrecedingComment = true) {
    if (!StartOverride.isValid())
      StartOverride = Node.getStartLoc();

    // Make sure to include any preceding comments attached to the loc
    if (IncludePrecedingComment)
      StartOverride = getLocIncludingPrecedingComment(StartOverride);

    llvm::SaveAndRestore<SourceLoc> RestoreLoc(LastAddedLoc, StartOverride);
    llvm::SaveAndRestore<int> RestoreCount(NestedExprCount,
                                           ConvertCalls ? 0 : 1);

    walk(Node);
    addRange(LastAddedLoc, Node.getEndLoc(), /*ToEndOfToken=*/true);
  }

  void convertPattern(const Pattern *P) {
    // Only print semantic patterns. This cleans up the output of the transform
    // and works around some bogus source locs that can appear with typed
    // patterns in if let statements.
    P = P->getSemanticsProvidingPattern();

    // Set up the start of the pattern as the last loc printed to make sure we
    // accurately fill in the gaps as we customize the printing of sub-patterns.
    llvm::SaveAndRestore<SourceLoc> RestoreLoc(LastAddedLoc, P->getStartLoc());
    llvm::SaveAndRestore<bool> RestoreFlag(ConvertingPattern, true);

    walk(const_cast<Pattern *>(P));
    addRange(LastAddedLoc, P->getEndLoc(), /*ToEndOfToken*/ true);
  }

  /// Check whether \p Node requires the remainder of this scope to be wrapped
  /// in a \c withChecked(Throwing)Continuation. If it is necessary, add
  /// a call to \c withChecked(Throwing)Continuation and modify the current
  /// scope (\c Scopes.back() ) so that it knows it's wrapped in a continuation.
  ///
  /// Wrapping a node in a continuation is necessary if the following conditions
  /// are satisfied:
  ///  - It contains a reference to the \c TopHandler's completion hander,
  ///    because these completion handler calls need to be promoted to \c return
  ///    statements in the refactored method, but
  ///  - We cannot hoist the completion handler of \p Node, because it doesn't
  ///    have an async alternative by our heuristics (e.g. because of a
  ///    completion handler name mismatch or because it also returns a value
  ///    synchronously).
  void wrapScopeInContinationIfNecessary(ASTNode Node) {
    if (NestedExprCount != 0) {
      // We can't start a continuation in the middle of an expression
      return;
    }
    if (Scopes.back().isWrappedInContination()) {
      // We are already in a continuation. No need to add another one.
      return;
    }
    if (!DeclReferenceFinder::containsReference(Node,
                                                TopHandler.getHandler())) {
      // The node doesn't have a reference to the function's completion handler.
      // It can stay a call with a completion handler, because we don't need to
      // promote a completion handler call to a 'return'.
      return;
    }

    // Wrap the current call in a continuation

    Identifier contName = createUniqueName("continuation");
    Scopes.back().Names.insert(contName);
    Scopes.back().ContinuationName = contName;

    insertCustom(Node.getStartLoc(), [&]() {
      OS << tok::kw_return << ' ';
      if (TopHandler.HasError) {
        OS << tok::kw_try << ' ';
      }
      OS << "await ";
      if (TopHandler.HasError) {
        OS << "withCheckedThrowingContinuation ";
      } else {
        OS << "withCheckedContinuation ";
      }
      OS << tok::l_brace << ' ' << contName << ' ' << tok::kw_in << '\n';
    });
  }

  bool walkToPatternPre(Pattern *P) override {
    // If we're not converting a pattern, there's nothing extra to do.
    if (!ConvertingPattern)
      return true;

    // When converting a pattern, don't print the 'let' or 'var' of binding
    // subpatterns, as they're illegal when nested in PBDs, and we print a
    // top-level one.
    if (auto *BP = dyn_cast<BindingPattern>(P)) {
      return addCustom(BP->getSourceRange(), [&]() {
        convertPattern(BP->getSubPattern());
      });
    }
    return true;
  }

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (isa<PatternBindingDecl>(D)) {
      // We can't hoist a closure inside a PatternBindingDecl. If it contains
      // a call to the completion handler, wrap it in a continuation.
      wrapScopeInContinationIfNecessary(D);
      NestedExprCount++;
      return true;
    }

    // Functions and types already have their names in \c Scopes.Names, only
    // variables should need to be renamed.
    if (isa<VarDecl>(D)) {
      // If we don't already have a name for the var, assign it one. Note that
      // vars in binding patterns may already have assigned names here.
      if (Names.find(D) == Names.end()) {
        auto Ident = assignUniqueName(D, StringRef());
        Scopes.back().Names.insert(Ident);
      }
      addCustom(D->getSourceRange(), [&]() {
        OS << newNameFor(D);
      });
    }

    // Note we don't walk into any nested local function decls. If we start
    // doing so in the future, be sure to update the logic that deals with
    // converting unhandled returns into placeholders in walkToStmtPre.
    return false;
  }

  bool walkToDeclPost(Decl *D) override {
    NestedExprCount--;
    return true;
  }

#define PLACEHOLDER_START "<#"
#define PLACEHOLDER_END "#>"
  bool walkToExprPre(Expr *E) override {
    // TODO: Handle Result.get as well
    if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
      if (auto *D = DRE->getDecl()) {
        // Look through to the parent var decl if we have one. This ensures we
        // look at the var in a case stmt's pattern rather than the var that's
        // implicitly declared in the body.
        if (auto *VD = dyn_cast<VarDecl>(D)) {
          if (auto *Parent = VD->getParentVarDecl())
            D = Parent;
        }

        bool AddPlaceholder = Placeholders.count(D);
        StringRef Name = newNameFor(D, false);
        if (AddPlaceholder || !Name.empty())
          return addCustom(DRE->getSourceRange(), [&]() {
            if (AddPlaceholder)
              OS << PLACEHOLDER_START;
            if (!Name.empty())
              OS << Name;
            else
              D->getName().print(OS);
            if (AddPlaceholder)
              OS << PLACEHOLDER_END;
          });
      }
    } else if (isa<ForceValueExpr>(E) || isa<BindOptionalExpr>(E)) {
      // Remove a force unwrap or optional chain of a returned success value,
      // as it will no longer be optional. For force unwraps, this is always a
      // valid transform. For optional chains, it is a locally valid transform
      // within the optional chain e.g foo?.x -> foo.x, but may change the type
      // of the overall chain, which could cause errors elsewhere in the code.
      // However this is generally more useful to the user than just leaving
      // 'foo' as a placeholder. Note this is only the case when no other
      // optionals are involved in the chain, e.g foo?.x?.y -> foo.x?.y is
      // completely valid.
      if (auto *D = E->getReferencedDecl().getDecl()) {
        if (Unwraps.count(D))
          return addCustom(E->getSourceRange(),
                           [&]() { OS << newNameFor(D, true); });
      }
    } else if (CallExpr *CE = TopHandler.getAsHandlerCall(E)) {
      if (Scopes.back().isWrappedInContination()) {
        return addCustom(E->getSourceRange(),
                         [&]() { convertHandlerToContinuationResume(CE); });
      } else if (NestedExprCount == 0) {
        return addCustom(E->getSourceRange(),
                         [&]() { convertHandlerToReturnOrThrows(CE); });
      }
    } else if (auto *CE = dyn_cast<CallExpr>(E)) {
      // Try and hoist a call's completion handler. Don't do so if
      //  - the current expression is nested (we can't start hoisting in the
      //    middle of an expression)
      //  - the current scope is wrapped in a continuation (we can't have await
      //    calls in the continuation block)
      if (NestedExprCount == 0 && !Scopes.back().isWrappedInContination()) {
        // If the refactoring is on the call itself, do not require the callee
        // to have the @available attribute or a completion-like name.
        auto HandlerDesc = AsyncHandlerParamDesc::find(
            getUnderlyingFunc(CE->getFn()),
            /*RequireAttributeOrName=*/StartNode.dyn_cast<Expr *>() != CE);
        if (HandlerDesc.isValid()) {
          return addCustom(CE->getSourceRange(),
                           [&]() { addHoistedCallback(CE, HandlerDesc); });
        }
      }
    }

    // A void SingleValueStmtExpr is semantically more like a statement than
    // an expression, so recurse without bumping the expr depth or wrapping in
    // continuation.
    if (auto *SVE = dyn_cast<SingleValueStmtExpr>(E)) {
      auto ty = SVE->getType();
      if (!ty || ty->isVoid())
        return true;
    }

    // We didn't do any special conversion for this expression. If needed, wrap
    // it in a continuation.
    wrapScopeInContinationIfNecessary(E);

    NestedExprCount++;
    return true;
  }

  bool replaceRangeWithPlaceholder(SourceRange range) {
    return addCustom(range, [&]() {
      OS << PLACEHOLDER_START;
      addRange(range, /*toEndOfToken*/ true);
      OS << PLACEHOLDER_END;
    });
  }

  bool walkToExprPost(Expr *E) override {
    if (auto *SVE = dyn_cast<SingleValueStmtExpr>(E)) {
      auto ty = SVE->getType();
      if (!ty || ty->isVoid())
        return true;
    }
    NestedExprCount--;
    return true;
  }

#undef PLACEHOLDER_START
#undef PLACEHOLDER_END

  bool walkToStmtPre(Stmt *S) override {
    // CaseStmt has an implicit BraceStmt inside it, which *should* start a new
    // scope, so don't check isImplicit here.
    if (startsNewScope(S)) {
      // Add all names of decls referenced within this statement that aren't
      // also declared first, plus any contexts. Note that \c getReferencedDecl
      // will only return a value for a \c BraceStmt. This means that \c IfStmt
      // (and other statements with conditions) will have their own empty scope,
      // which is fine for our purposes - their existing names are always valid.
      // The body of those statements will include the decls if they've been
      // referenced, so shadowing is still avoided there.
      if (auto *ReferencedDecls = ScopedDecls.getReferencedDecls(S)) {
        llvm::DenseSet<const Decl *> Decls;
        for (auto DeclAndNumRefs : *ReferencedDecls)
          Decls.insert(DeclAndNumRefs.first);
        addNewScope(Decls);
      } else {
        addNewScope({});
      }
    } else if (Hoisting && !S->isImplicit()) {
      // Some break and return statements need to be turned into placeholders,
      // as they may no longer perform the control flow that the user is
      // expecting.
      if (auto *BS = dyn_cast<BreakStmt>(S)) {
        // For a break, if it's jumping out of a switch statement that we've
        // re-written as a part of the transform, turn it into a placeholder, as
        // it would have been lifted out of the switch statement.
        if (auto *SS = dyn_cast<SwitchStmt>(BS->getTarget())) {
          if (HandledSwitches.contains(SS))
            return replaceRangeWithPlaceholder(S->getSourceRange());
        }
      } else if (isa<ReturnStmt>(S) && NestedExprCount == 0) {
        // For a return, if it's not nested inside another closure or function,
        // turn it into a placeholder, as it will be lifted out of the callback.
        // Note that we only turn the 'return' token into a placeholder as we
        // still want to be able to apply transforms to the argument.
        replaceRangeWithPlaceholder(S->getStartLoc());
      }
    }
    return true;
  }

  bool walkToStmtPost(Stmt *S) override {
    if (startsNewScope(S)) {
      bool ClosedScopeWasWrappedInContinuation =
          Scopes.back().isWrappedInContination();
      Scopes.pop_back();
      if (ClosedScopeWasWrappedInContinuation &&
          !Scopes.back().isWrappedInContination()) {
        // The nested scope was wrapped in a continuation but the current one
        // isn't anymore. Add the '}' that corresponds to the the call to
        // withChecked(Throwing)Continuation.
        insertCustom(S->getEndLoc(), [&]() { OS << tok::r_brace << '\n'; });
      }
    }
    return true;
  }

  bool addCustom(SourceRange Range, llvm::function_ref<void()> Custom = {}) {
    addRange(LastAddedLoc, Range.Start);
    Custom();
    LastAddedLoc = Lexer::getLocForEndOfToken(SM, Range.End);
    return false;
  }

  /// Insert custom text at the given \p Loc that shouldn't replace any existing
  /// source code.
  bool insertCustom(SourceLoc Loc, llvm::function_ref<void()> Custom = {}) {
    addRange(LastAddedLoc, Loc);
    Custom();
    LastAddedLoc = Loc;
    return false;
  }

  void addRange(SourceLoc Start, SourceLoc End, bool ToEndOfToken = false) {
    if (ToEndOfToken) {
      OS << Lexer::getCharSourceRangeFromSourceRange(SM,
                                                     SourceRange(Start, End))
                .str();
    } else {
      OS << CharSourceRange(SM, Start, End).str();
    }
  }

  void addRange(SourceRange Range, bool ToEndOfToken = false) {
    addRange(Range.Start, Range.End, ToEndOfToken);
  }

  void addFuncDecl(const FuncDecl *FD) {
    auto *Params = FD->getParameters();
    auto *HandlerParam = TopHandler.getHandlerParam();
    auto ParamPos = TopHandler.handlerParamPosition();

    // If the completion handler parameter has a default argument, the async
    // version is effectively @discardableResult, as not all the callers care
    // about receiving the completion call.
    if (HandlerParam && HandlerParam->isDefaultArgument())
      OS << tok::at_sign << "discardableResult" << "\n";

    // First chunk: start -> the parameter to remove (if any)
    SourceLoc LeftEndLoc;
    switch (ParamPos) {
    case AsyncHandlerParamDesc::Position::None:
    case AsyncHandlerParamDesc::Position::Only:
    case AsyncHandlerParamDesc::Position::First:
      // Handler is the first param (or there is none), so only include the (
      LeftEndLoc = Params->getLParenLoc().getAdvancedLoc(1);
      break;
    case AsyncHandlerParamDesc::Position::Middle:
      // Handler is somewhere in the middle of the params, so we need to
      // include any comments and comma up until the handler
      LeftEndLoc = Params->get(TopHandler.Index)->getStartLoc();
      LeftEndLoc = getLocIncludingPrecedingComment(LeftEndLoc);
      break;
    case AsyncHandlerParamDesc::Position::Last:
      // Handler is the last param, which means we don't want the comma. This
      // is a little annoying since we *do* want the comments past for the
      // last parameter
      LeftEndLoc = Lexer::getLocForEndOfToken(
          SM, Params->get(TopHandler.Index - 1)->getEndLoc());
      // Skip to the end of any comments
      Token Next = Lexer::getTokenAtLocation(SM, LeftEndLoc,
                                             CommentRetentionMode::None);
      if (Next.getKind() != tok::NUM_TOKENS)
        LeftEndLoc = Next.getLoc();
      break;
    }
    addRange(FD->getSourceRangeIncludingAttrs().Start, LeftEndLoc);

    // Second chunk: end of the parameter to remove -> right parenthesis
    SourceLoc MidStartLoc;
    SourceLoc MidEndLoc = Params->getRParenLoc().getAdvancedLoc(1);
    switch (ParamPos) {
    case AsyncHandlerParamDesc::Position::None:
      // No handler param, so make sure to include them all
      MidStartLoc = LeftEndLoc;
      break;
    case AsyncHandlerParamDesc::Position::First:
    case AsyncHandlerParamDesc::Position::Middle:
      // Handler param is either the first or one of the middle params. Skip
      // past it but make sure to include comments preceding the param after
      // the handler
      MidStartLoc = Params->get(TopHandler.Index + 1)->getStartLoc();
      MidStartLoc = getLocIncludingPrecedingComment(MidStartLoc);
      break;
    case AsyncHandlerParamDesc::Position::Only:
    case AsyncHandlerParamDesc::Position::Last:
      // Handler param is last, this is easy since there's no other params
      // to copy over
      MidStartLoc = Params->getRParenLoc();
      break;
    }
    addRange(MidStartLoc, MidEndLoc);

    // Third chunk: add in async and throws if necessary
    if (!FD->hasAsync())
      OS << " async";
    if (FD->hasThrows() || TopHandler.HasError)
      // TODO: Add throws if converting a function and it has a converted call
      //       without a do/catch
      OS << " " << tok::kw_throws;

    // Fourth chunk: if no parent handler (ie. not adding an async
    // alternative), the rest of the decl. Otherwise, add in the new return
    // type
    if (!TopHandler.isValid()) {
      SourceLoc RightStartLoc = MidEndLoc;
      if (FD->hasThrows()) {
        RightStartLoc = Lexer::getLocForEndOfToken(SM, FD->getThrowsLoc());
      }
      SourceLoc RightEndLoc =
          FD->getBody() ? FD->getBody()->getLBraceLoc() : RightStartLoc;
      addRange(RightStartLoc, RightEndLoc);
      return;
    }

    SmallVector<LabeledReturnType, 2> Scratch;
    auto ReturnTypes = TopHandler.getAsyncReturnTypes(Scratch);
    if (ReturnTypes.empty()) {
      OS << " ";
      return;
    }

    // Print the function result type, making sure to omit a '-> Void' return.
    if (!TopHandler.willAsyncReturnVoid()) {
      OS << " -> ";
      addAsyncFuncReturnType(TopHandler);
    }

    if (FD->hasBody())
      OS << " ";

    // TODO: Should remove the generic param and where clause for the error
    //       param if it exists (and no other parameter uses that type)
    TrailingWhereClause *TWC = FD->getTrailingWhereClause();
    if (TWC && TWC->getWhereLoc().isValid()) {
      auto Range = TWC->getSourceRange();
      OS << Lexer::getCharSourceRangeFromSourceRange(SM, Range).str();
      if (FD->hasBody())
        OS << " ";
    }
  }

  void addFallbackVars(ArrayRef<const ParamDecl *> FallbackParams,
                       const ClosureCallbackParams &AllParams) {
    for (auto *Param : FallbackParams) {
      auto Ty = Param->getTypeInContext();
      auto ParamName = newNameFor(Param);

      // If this is the known bool success param, we can use 'let' and type it
      // as non-optional, as it gets bound in both blocks.
      if (AllParams.isKnownBoolFlagParam(Param)) {
        OS << tok::kw_let << " " << ParamName << ": ";
        Ty->print(OS);
        OS << "\n";
        continue;
      }

      OS << tok::kw_var << " " << ParamName << ": ";
      Ty->print(OS);
      if (!Ty->getOptionalObjectType())
        OS << "?";

      OS << " = " << tok::kw_nil << "\n";
    }
  }

  void addDo() { OS << tok::kw_do << " " << tok::l_brace << "\n"; }

  /// Assuming that \p Result represents an error result to completion handler,
  /// returns \c true if the error has already been handled through a
  /// 'try await'.
  bool isErrorAlreadyHandled(HandlerResult Result) {
    assert(Result.isError());
    assert(Result.args().size() == 1 &&
           "There should only be one error parameter");
    // We assume that the error has already been handled if its variable
    // declaration doesn't exist anymore, which is the case if it's in
    // Placeholders but not in Unwraps (if it's in Placeholders and Unwraps
    // an optional Error has simply been promoted to a non-optional Error).
    if (auto *DRE = dyn_cast<DeclRefExpr>(Result.args().back().getExpr())) {
      if (Placeholders.count(DRE->getDecl()) &&
          !Unwraps.count(DRE->getDecl())) {
        return true;
      }
    }
    return false;
  }

  /// Returns \c true if the source representation of \p E can be interpreted
  /// as an expression returning an Optional value.
  bool isExpressionOptional(Expr *E) {
    if (isa<InjectIntoOptionalExpr>(E)) {
      // E is downgrading a non-Optional result to an Optional. Its source
      // representation isn't Optional.
      return false;
    }
    if (auto DRE = dyn_cast<DeclRefExpr>(E)) {
      if (Unwraps.count(DRE->getDecl())) {
        // E has been promoted to a non-Optional value. It can't be used as an
        // Optional anymore.
        return false;
      }
    }
    if (!E->getType().isNull() && E->getType()->isOptional()) {
      return true;
    }
    // We couldn't determine the type. Assume non-Optional.
    return false;
  }

  /// Converts a call \p CE to a completion handler. Depending on the call it
  /// will be interpreted as a call that's returning a success result, an error
  /// or, if the call is completely ambiguous, adds an if-let that checks if the
  /// error is \c nil at runtime and dispatches to the success or error case
  /// depending on it.
  /// \p AddConvertedHandlerCall needs to add the converted version of the
  /// completion handler. Depending on the given \c HandlerResult, it must be
  /// intepreted as a success or error call.
  /// \p AddConvertedErrorCall must add the converted equivalent of returning an
  /// error. The passed \c StringRef contains the name of a variable that is of
  /// type 'Error'.
  void convertHandlerCall(
      const CallExpr *CE,
      llvm::function_ref<void(HandlerResult)> AddConvertedHandlerCall,
      llvm::function_ref<void(StringRef)> AddConvertedErrorCall) {
    auto Result =
        TopHandler.extractResultArgs(CE, /*ReturnErrorArgsIfAmbiguous=*/true);
    if (!TopHandler.isAmbiguousCallToParamHandler(CE)) {
      if (Result.isError()) {
        if (!isErrorAlreadyHandled(Result)) {
          // If the error has already been handled, we don't need to add another
          // throwing call.
          AddConvertedHandlerCall(Result);
        }
      } else {
        AddConvertedHandlerCall(Result);
      }
    } else {
      assert(Result.isError() && "If the call was ambiguous, we should have "
                                 "retrieved its error representation");
      assert(Result.args().size() == 1 &&
             "There should only be one error parameter");
      Expr *ErrorExpr = Result.args().back().getExpr();
      if (isErrorAlreadyHandled(Result)) {
        // The error has already been handled, interpret the call as a success
        // call.
        auto SuccessExprs = TopHandler.extractResultArgs(
            CE, /*ReturnErrorArgsIfAmbiguous=*/false);
        AddConvertedHandlerCall(SuccessExprs);
      } else if (!isExpressionOptional(ErrorExpr)) {
        // The error is never nil. No matter what the success param is, we
        // interpret it as an error call.
        AddConvertedHandlerCall(Result);
      } else {
        // The call was truly ambiguous. Add an
        // if let error = <convert error arg> {
        //   throw error // or equivalent
        // } else {
        //   <interpret call as success call>
        // }
        auto SuccessExprs = TopHandler.extractResultArgs(
            CE, /*ReturnErrorArgsIfAmbiguous=*/false);

        // The variable 'error' is only available in the 'if let' scope, so we
        // don't need to create a new unique one.
        StringRef ErrorName = "error";
        OS << tok::kw_if << ' ' << tok::kw_let << ' ' << ErrorName << ' '
           << tok::equal << ' ';
        convertNode(ErrorExpr, /*StartOverride=*/{}, /*ConvertCalls=*/false);
        OS << ' ' << tok::l_brace << '\n';
        AddConvertedErrorCall(ErrorName);
        OS << tok::r_brace << ' ' << tok::kw_else << ' ' << tok::l_brace
           << '\n';
        AddConvertedHandlerCall(SuccessExprs);
        OS << '\n' << tok::r_brace;
      }
    }
  }

  /// Convert a call \p CE to a completion handler to its 'return' or 'throws'
  /// equivalent.
  void convertHandlerToReturnOrThrows(const CallExpr *CE) {
    return convertHandlerCall(
        CE,
        [&](HandlerResult Exprs) {
          convertHandlerToReturnOrThrowsImpl(CE, Exprs);
        },
        [&](StringRef ErrorName) {
          OS << tok::kw_throw << ' ' << ErrorName << '\n';
        });
  }

  /// Convert the call \p CE to a completion handler to its 'return' or 'throws'
  /// equivalent, where \p Result determines whether the call should be
  /// interpreted as an error or success call.
  void convertHandlerToReturnOrThrowsImpl(const CallExpr *CE,
                                          HandlerResult Result) {
    bool AddedReturnOrThrow = true;
    if (!Result.isError()) {
      // It's possible the user has already written an explicit return statement
      // for the completion handler call, e.g 'return completion(args...)'. In
      // that case, be sure not to add another return.
      auto *parent = getWalker().Parent.getAsStmt();
      if (isa_and_nonnull<ReturnStmt>(parent) &&
          !cast<ReturnStmt>(parent)->isImplicit()) {
        // The statement already has a return keyword. Don't add another one.
        AddedReturnOrThrow = false;
      } else {
        OS << tok::kw_return;
      }
    } else {
      OS << tok::kw_throw;
    }

    auto Args = Result.args();
    if (!Args.empty()) {
      if (AddedReturnOrThrow)
        OS << ' ';

      addTupleOf(Args, OS, [&](Argument Arg) {
        // Special case: If the completion handler is a params handler that
        // takes an error, we could pass arguments to it without unwrapping
        // them. E.g.
        //   simpleWithError { (res: String?, error: Error?) in
        //     completion(res, nil)
        //   }
        // But after refactoring `simpleWithError` to an async function we have
        //   let res: String = await simple()
        // and `res` is no longer an `Optional`. Thus it's in `Placeholders` and
        // `Unwraps` and any reference to it will be replaced by a placeholder
        // unless it is wrapped in an unwrapping expression. This would cause us
        // to create `return <#res# >`.
        // Under our assumption that either the error or the result parameter
        // are non-nil, the above call to the completion handler is equivalent
        // to
        //   completion(res!, nil)
        // which correctly yields
        //   return res
        // Synthesize the force unwrap so that we get the expected results.
        auto *E = Arg.getExpr();
        if (TopHandler.getHandlerType() == HandlerType::PARAMS &&
            TopHandler.HasError) {
          if (auto DRE =
                  dyn_cast<DeclRefExpr>(E->getSemanticsProvidingExpr())) {
            auto D = DRE->getDecl();
            if (Unwraps.count(D)) {
              E = new (getASTContext()) ForceValueExpr(E, SourceLoc());
            }
          }
        }
        // Can't just add the range as we need to perform replacements
        convertNode(E, /*StartOverride=*/Arg.getLabelLoc(),
                    /*ConvertCalls=*/false);
      });
    }
  }

  /// Convert a call \p CE to a completion handler to resumes of the
  /// continuation that's currently on top of the stack.
  void convertHandlerToContinuationResume(const CallExpr *CE) {
    return convertHandlerCall(
        CE,
        [&](HandlerResult Exprs) {
          convertHandlerToContinuationResumeImpl(CE, Exprs);
        },
        [&](StringRef ErrorName) {
          Identifier ContinuationName = Scopes.back().ContinuationName;
          OS << ContinuationName << tok::period << "resume" << tok::l_paren
             << "throwing" << tok::colon << ' ' << ErrorName;
          OS << tok::r_paren << '\n';
        });
  }

  /// Convert a call \p CE to a completion handler to resumes of the
  /// continuation that's currently on top of the stack.
  /// \p Result determines whether the call should be interpreted as a success
  /// or error call.
  void convertHandlerToContinuationResumeImpl(const CallExpr *CE,
                                              HandlerResult Result) {
    assert(Scopes.back().isWrappedInContination());

    std::vector<Argument> Args;
    StringRef ResumeArgumentLabel;
    switch (TopHandler.getHandlerType()) {
    case HandlerType::PARAMS: {
      Args = Result.args();
      if (!Result.isError()) {
        ResumeArgumentLabel = "returning";
      } else {
        ResumeArgumentLabel = "throwing";
      }
      break;
    }
    case HandlerType::RESULT: {
      Args = {CE->getArgs()->begin(), CE->getArgs()->end()};
      ResumeArgumentLabel = "with";
      break;
    }
    case HandlerType::INVALID:
      llvm_unreachable("Invalid top handler");
    }

    // A vector in which each argument of Result has an entry. If the entry is
    // not empty then that argument has been unwrapped using 'guard let' into
    // a variable with that name.
    SmallVector<Identifier, 4> ArgNames;
    ArgNames.reserve(Args.size());

    /// When unwrapping a result argument \p Arg into a variable using
    /// 'guard let' return a suitable name for the unwrapped variable.
    /// \p ArgIndex is the index of \p Arg in the results passed to the
    /// completion handler.
    auto GetSuitableNameForGuardUnwrap = [&](Expr *Arg,
                                             unsigned ArgIndex) -> Identifier {
      // If Arg is a DeclRef, use its name for the guard unwrap.
      // guard let myVar1 = myVar.
      if (auto DRE = dyn_cast<DeclRefExpr>(Arg)) {
        return createUniqueName(DRE->getDecl()->getBaseIdentifier().str());
      } else if (auto IIOE = dyn_cast<InjectIntoOptionalExpr>(Arg)) {
        if (auto DRE = dyn_cast<DeclRefExpr>(IIOE->getSubExpr())) {
          return createUniqueName(DRE->getDecl()->getBaseIdentifier().str());
        }
      }
      if (Args.size() == 1) {
        // We only have a single result. 'result' seems a resonable name.
        return createUniqueName("result");
      } else {
        // We are returning a tuple. Name the result elements 'result' +
        // index in tuple.
        return createUniqueName("result" + std::to_string(ArgIndex));
      }
    };

    unsigned ArgIndex = 0;
    for (auto Arg : Args) {
      auto *ArgExpr = Arg.getExpr();
      Identifier ArgName;
      if (isExpressionOptional(ArgExpr) && TopHandler.HasError) {
        ArgName = GetSuitableNameForGuardUnwrap(ArgExpr, ArgIndex);
        Scopes.back().Names.insert(ArgName);
        OS << tok::kw_guard << ' ' << tok::kw_let << ' ' << ArgName << ' '
           << tok::equal << ' ';

        // If the argument is a call with a trailing closure, the generated
        // guard statement will not compile.
        // e.g. 'guard let result1 = value.map { $0 + 1 } else { ... }'
        // doesn't compile. Adding parentheses makes the code compile.
        auto HasTrailingClosure = false;
        if (auto *CE = dyn_cast<CallExpr>(ArgExpr)) {
          if (CE->getArgs()->hasAnyTrailingClosures())
            HasTrailingClosure = true;
        }

        if (HasTrailingClosure)
          OS << tok::l_paren;

        convertNode(ArgExpr, /*StartOverride=*/Arg.getLabelLoc(),
                    /*ConvertCalls=*/false);

        if (HasTrailingClosure)
          OS << tok::r_paren;

        OS << ' ' << tok::kw_else << ' ' << tok::l_brace << '\n';
        OS << "fatalError" << tok::l_paren;
        OS << "\"Expected non-nil result ";
        if (ArgName.str() != "result") {
          OS << "'" << ArgName << "' ";
        }
        OS << "in the non-error case\"";
        OS << tok::r_paren << '\n';
        OS << tok::r_brace << '\n';
      }
      ArgNames.push_back(ArgName);
      ArgIndex++;
    }

    Identifier ContName = Scopes.back().ContinuationName;
    OS << ContName << tok::period << "resume" << tok::l_paren
       << ResumeArgumentLabel << tok::colon << ' ';

    ArgIndex = 0;
    addTupleOf(Args, OS, [&](Argument Arg) {
      Identifier ArgName = ArgNames[ArgIndex];
      if (!ArgName.empty()) {
        OS << ArgName;
      } else {
        // Can't just add the range as we need to perform replacements
        convertNode(Arg.getExpr(), /*StartOverride=*/Arg.getLabelLoc(),
                    /*ConvertCalls=*/false);
      }
      ArgIndex++;
    });
    OS << tok::r_paren;
  }

  /// From the given expression \p E, which is an argument to a function call,
  /// extract the passed closure if there is one. Otherwise return \c nullptr.
  ClosureExpr *extractCallback(Expr *E) {
    E = lookThroughFunctionConversionExpr(E);
    if (auto Closure = dyn_cast<ClosureExpr>(E)) {
      return Closure;
    } else if (auto CaptureList = dyn_cast<CaptureListExpr>(E)) {
      return dyn_cast<ClosureExpr>(CaptureList->getClosureBody());
    } else {
      return nullptr;
    }
  }

  /// Callback arguments marked as e.g. `@convention(block)` produce arguments
  /// that are `FunctionConversionExpr`.
  /// We don't care about the conversions and want to shave them off.
  Expr *lookThroughFunctionConversionExpr(Expr *E) {
    if (auto FunctionConversion = dyn_cast<FunctionConversionExpr>(E)) {
      return lookThroughFunctionConversionExpr(
          FunctionConversion->getSubExpr());
    } else {
      return E;
    }
  }

  void addHoistedCallback(const CallExpr *CE,
                          const AsyncHandlerParamDesc &HandlerDesc) {
    llvm::SaveAndRestore<bool> RestoreHoisting(Hoisting, true);

    auto *ArgList = CE->getArgs();
    if (HandlerDesc.Index >= ArgList->size()) {
      DiagEngine.diagnose(CE->getStartLoc(), diag::missing_callback_arg);
      return;
    }

    Expr *CallbackArg =
        lookThroughFunctionConversionExpr(ArgList->getExpr(HandlerDesc.Index));
    if (ClosureExpr *Callback = extractCallback(CallbackArg)) {
      // The user is using a closure for the completion handler
      addHoistedClosureCallback(CE, HandlerDesc, Callback);
      return;
    }
    if (auto CallbackDecl = getReferencedDecl(CallbackArg)) {
      if (CallbackDecl == TopHandler.getHandler()) {
        // We are refactoring the function that declared the completion handler
        // that would be called here. We can't call the completion handler
        // anymore because it will be removed. But since the function that
        // declared it is being refactored to async, we can just return the
        // values.
        if (!HandlerDesc.willAsyncReturnVoid()) {
          OS << tok::kw_return << " ";
        }
        InlinePatternsToPrint InlinePatterns;
        addAwaitCall(CE, ClassifiedBlock(), {}, InlinePatterns, HandlerDesc,
                     /*AddDeclarations*/ false);
        return;
      }
      // We are not removing the completion handler, so we can call it once the
      // async function returns.

      // The completion handler that is called as part of the \p CE call.
      // This will be called once the async function returns.
      auto CompletionHandler =
          AsyncHandlerDesc::get(CallbackDecl, /*RequireAttributeOrName=*/false);
      if (CompletionHandler.isValid()) {
        if (auto CalledFunc = getUnderlyingFunc(CE->getFn())) {
          StringRef HandlerName = Lexer::getCharSourceRangeFromSourceRange(
              SM, CallbackArg->getSourceRange()).str();
          addHoistedNamedCallback(
              CalledFunc, CompletionHandler, HandlerName, [&] {
                InlinePatternsToPrint InlinePatterns;
                addAwaitCall(CE, ClassifiedBlock(), {}, InlinePatterns,
                             HandlerDesc, /*AddDeclarations*/ false);
              });
          return;
        }
      }
    }
    DiagEngine.diagnose(CE->getStartLoc(), diag::missing_callback_arg);
  }

  /// Add a binding to a known bool flag that indicates success or failure.
  void addBoolFlagParamBindingIfNeeded(llvm::Optional<KnownBoolFlagParam> Flag,
                                       BlockKind Block) {
    if (!Flag)
      return;
    // Figure out the polarity of the binding based on the block we're in and
    // whether the flag indicates success.
    auto Polarity = true;
    switch (Block) {
    case BlockKind::SUCCESS:
      break;
    case BlockKind::ERROR:
      Polarity = !Polarity;
      break;
    case BlockKind::FALLBACK:
      llvm_unreachable("Not a valid place to bind");
    }
    if (!Flag->IsSuccessFlag)
      Polarity = !Polarity;

    OS << newNameFor(Flag->Param) << " " << tok::equal << " ";
    OS << (Polarity ? tok::kw_true : tok::kw_false) << "\n";
  }

  /// Add a call to the async alternative of \p CE and convert the \p Callback
  /// to be executed after the async call. \p HandlerDesc describes the
  /// completion handler in the function that's called by \p CE and \p ArgList
  /// are the arguments being passed in \p CE.
  void addHoistedClosureCallback(const CallExpr *CE,
                                 const AsyncHandlerParamDesc &HandlerDesc,
                                 const ClosureExpr *Callback) {
    if (HandlerDesc.params().size() != Callback->getParameters()->size()) {
      DiagEngine.diagnose(CE->getStartLoc(), diag::mismatched_callback_args);
      return;
    }
    ClosureCallbackParams CallbackParams(HandlerDesc, Callback);
    ClassifiedBlocks Blocks;
    auto *CallbackBody = Callback->getBody();
    if (!HandlerDesc.HasError) {
      Blocks.SuccessBlock.addNodesInBraceStmt(CallbackBody);
    } else if (!CallbackBody->getElements().empty()) {
      CallbackClassifier::classifyInto(Blocks, CallbackParams, HandledSwitches,
                                       DiagEngine, CallbackBody);
    }

    auto SuccessBindings = CallbackParams.getParamsToBind(BlockKind::SUCCESS);
    auto *ErrParam = CallbackParams.getErrParam();
    if (DiagEngine.hadAnyError()) {
      // For now, only fallback when the results are params with an error param,
      // in which case only the names are used (defaulted to the names of the
      // params if none).
      if (HandlerDesc.Type != HandlerType::PARAMS || !HandlerDesc.HasError)
        return;
      DiagEngine.resetHadAnyError();

      // Note that we don't print any inline patterns here as we just want
      // assignments to the names in the outer scope.
      InlinePatternsToPrint InlinePatterns;

      auto AllBindings = CallbackParams.getParamsToBind(BlockKind::FALLBACK);

      prepareNames(ClassifiedBlock(), AllBindings, InlinePatterns);
      preparePlaceholdersAndUnwraps(HandlerDesc, CallbackParams,
                                    BlockKind::FALLBACK);
      addFallbackVars(AllBindings, CallbackParams);
      addDo();
      addAwaitCall(CE, Blocks.SuccessBlock, SuccessBindings, InlinePatterns,
                   HandlerDesc, /*AddDeclarations*/ false);
      OS << "\n";

      // If we have a known Bool success param, we need to bind it.
      addBoolFlagParamBindingIfNeeded(CallbackParams.getKnownBoolFlagParam(),
                                      BlockKind::SUCCESS);
      addFallbackCatch(CallbackParams);
      OS << "\n";
      convertNodes(NodesToPrint::inBraceStmt(CallbackBody));

      clearNames(AllBindings);
      return;
    }

    auto *ErrOrResultParam = ErrParam;
    if (auto *ResultParam = CallbackParams.getResultParam())
      ErrOrResultParam = ResultParam;

    auto ErrorNodes = Blocks.ErrorBlock.nodesToPrint().getNodes();
    bool RequireDo = !ErrorNodes.empty();
    // Check if we *actually* need a do/catch (see class comment)
    if (ErrorNodes.size() == 1) {
      auto Node = ErrorNodes[0];
      if (auto *HandlerCall = TopHandler.getAsHandlerCall(Node)) {
        auto Res = TopHandler.extractResultArgs(
            HandlerCall, /*ReturnErrorArgsIfAmbiguous=*/true);
        if (Res.args().size() == 1) {
          // Skip if we have the param itself or the name it's bound to
          auto *ArgExpr = Res.args()[0].getExpr();
          auto *SingleDecl = ArgExpr->getReferencedDecl().getDecl();
          auto ErrName = Blocks.ErrorBlock.boundName(ErrOrResultParam);
          RequireDo = SingleDecl != ErrOrResultParam &&
                      !(Res.isError() && SingleDecl &&
                        SingleDecl->getName().isSimpleName(ErrName));
        }
      }
    }

    // If we're not requiring a 'do', we'll be dropping the error block. But
    // let's make sure we at least preserve the comments in the error block by
    // transplanting them into the success block. This should make sure they
    // maintain a sensible ordering.
    if (!RequireDo) {
      auto ErrorNodes = Blocks.ErrorBlock.nodesToPrint();
      for (auto CommentLoc : ErrorNodes.getPossibleCommentLocs())
        Blocks.SuccessBlock.addPossibleCommentLoc(CommentLoc);
    }

    if (RequireDo) {
      addDo();
    }

    auto InlinePatterns = getInlinePatternsToPrint(Blocks.SuccessBlock,
                                                   SuccessBindings, Callback);

    prepareNames(Blocks.SuccessBlock, SuccessBindings, InlinePatterns);
    preparePlaceholdersAndUnwraps(HandlerDesc, CallbackParams,
                                  BlockKind::SUCCESS);

    addAwaitCall(CE, Blocks.SuccessBlock, SuccessBindings, InlinePatterns,
                 HandlerDesc, /*AddDeclarations=*/true);
    printOutOfLineBindingPatterns(Blocks.SuccessBlock, InlinePatterns);
    convertNodes(Blocks.SuccessBlock.nodesToPrint());
    clearNames(SuccessBindings);

    if (RequireDo) {
      // We don't use inline patterns for the error path.
      InlinePatternsToPrint ErrInlinePatterns;

      // Always use the ErrParam name if none is bound.
      prepareNames(Blocks.ErrorBlock, llvm::makeArrayRef(ErrOrResultParam),
                   ErrInlinePatterns,
                   /*AddIfMissing=*/HandlerDesc.Type != HandlerType::RESULT);
      preparePlaceholdersAndUnwraps(HandlerDesc, CallbackParams,
                                    BlockKind::ERROR);

      addCatch(ErrOrResultParam);
      convertNodes(Blocks.ErrorBlock.nodesToPrint());
      OS << "\n" << tok::r_brace;
      clearNames(llvm::makeArrayRef(ErrOrResultParam));
    }
  }

  /// Add a call to the async alternative of \p FD. Afterwards, pass the results
  /// of the async call to the completion handler, named \p HandlerName and
  /// described by \p HandlerDesc.
  /// \p AddAwaitCall adds the call to the refactored async method to the output
  /// stream without storing the result to any variables.
  /// This is used when the user didn't use a closure for the callback, but
  /// passed in a variable or function name for the completion handler.
  void addHoistedNamedCallback(const FuncDecl *FD,
                               const AsyncHandlerDesc &HandlerDesc,
                               StringRef HandlerName,
                               std::function<void(void)> AddAwaitCall) {
    if (HandlerDesc.HasError) {
      // "result" and "error" always okay to use here since they're added
      // in their own scope, which only contains new code.
      addDo();
      if (!HandlerDesc.willAsyncReturnVoid()) {
        OS << tok::kw_let << " result";
        addResultTypeAnnotationIfNecessary(FD, HandlerDesc);
        OS << " " << tok::equal << " ";
      }
      AddAwaitCall();
      OS << "\n";
      addCallToCompletionHandler("result", HandlerDesc, HandlerName);
      OS << "\n";
      OS << tok::r_brace << " " << tok::kw_catch << " " << tok::l_brace << "\n";
      addCallToCompletionHandler(StringRef(), HandlerDesc, HandlerName);
      OS << "\n" << tok::r_brace; // end catch
    } else {
      // This code may be placed into an existing scope, in that case create
      // a unique "result" name so that it doesn't cause shadowing or redecls.
      StringRef ResultName;
      if (!HandlerDesc.willAsyncReturnVoid()) {
        Identifier Unique = createUniqueName("result");
        Scopes.back().Names.insert(Unique);
        ResultName = Unique.str();

        OS << tok::kw_let << " " << ResultName;
        addResultTypeAnnotationIfNecessary(FD, HandlerDesc);
        OS << " " << tok::equal << " ";
      } else {
        // The name won't end up being used, just give it a bogus one so that
        // the result path is taken (versus the error path).
        ResultName = "result";
      }
      AddAwaitCall();
      OS << "\n";
      addCallToCompletionHandler(ResultName, HandlerDesc, HandlerName);
    }
  }

  /// Checks whether a binding pattern for a given decl can be printed inline in
  /// an await call, e.g 'let ((x, y), z) = await foo()', where '(x, y)' is the
  /// inline pattern.
  const Pattern *
  bindingPatternToPrintInline(const Decl *D, const ClassifiedBlock &Block,
                              const ClosureExpr *CallbackClosure) {
    // Only currently done for callback closures.
    if (!CallbackClosure)
      return nullptr;

    // If we can reduce the pattern bindings down to a single pattern, we may
    // be able to print it inline.
    auto *P = Block.getSinglePatternFor(D);
    if (!P)
      return nullptr;

    // Patterns that bind a single var are always printed inline.
    if (P->getSingleVar())
      return P;

    // If we have a multi-var binding, and the decl being bound is referenced
    // elsewhere in the block, we cannot print the pattern immediately in the
    // await call. Instead, we'll print it out of line.
    auto *Decls = ScopedDecls.getReferencedDecls(CallbackClosure->getBody());
    assert(Decls);
    auto NumRefs = Decls->lookup(D);
    return NumRefs == 1 ? P : nullptr;
  }

  /// Retrieve a map of patterns to print inline for an array of param decls.
  InlinePatternsToPrint
  getInlinePatternsToPrint(const ClassifiedBlock &Block,
                           ArrayRef<const ParamDecl *> Params,
                           const ClosureExpr *CallbackClosure) {
    InlinePatternsToPrint Patterns;
    for (auto *Param : Params) {
      if (auto *P = bindingPatternToPrintInline(Param, Block, CallbackClosure))
        Patterns[Param] = P;
    }
    return Patterns;
  }

  /// Print any out of line binding patterns that could not be printed as inline
  /// patterns. These typically appear directly after an await call, e.g:
  /// \code
  /// let x = await foo()
  /// let (y, z) = x
  /// \endcode
  void
  printOutOfLineBindingPatterns(const ClassifiedBlock &Block,
                                const InlinePatternsToPrint &InlinePatterns) {
    for (auto &Entry : Block.paramPatternBindings()) {
      auto *D = Entry.first;
      auto Aliases = Block.getAliasesFor(D);

      for (auto *P : Entry.second) {
        // If we already printed this as an inline pattern, there's nothing else
        // to do.
        if (InlinePatterns.lookup(D) == P)
          continue;

        // If this is an alias binding, it can be elided.
        if (auto *SingleVar = P->getSingleVar()) {
          if (Aliases.contains(SingleVar))
            continue;
        }

        auto HasMutable = P->hasAnyMutableBindings();
        OS << "\n" << (HasMutable ? tok::kw_var : tok::kw_let) << " ";
        convertPattern(P);
        OS << " = ";
        OS << newNameFor(D);
      }
    }
  }

  /// Prints an \c await call to an \c async function, binding any return values
  /// into variables.
  ///
  /// \param CE The call expr to convert.
  /// \param SuccessBlock The nodes present in the success block following the
  /// call.
  /// \param SuccessParams The success parameters, which will be printed as
  /// return values.
  /// \param InlinePatterns A map of patterns that can be printed inline for
  /// a given param.
  /// \param HandlerDesc A description of the completion handler.
  /// \param AddDeclarations Whether or not to add \c let or \c var keywords to
  /// the return value bindings.
  void addAwaitCall(const CallExpr *CE, const ClassifiedBlock &SuccessBlock,
                    ArrayRef<const ParamDecl *> SuccessParams,
                    const InlinePatternsToPrint &InlinePatterns,
                    const AsyncHandlerParamDesc &HandlerDesc,
                    bool AddDeclarations) {
    auto *Args = CE->getArgs();

    // Print the bindings to match the completion handler success parameters,
    // making sure to omit in the case of a Void return.
    if (!SuccessParams.empty() && !HandlerDesc.willAsyncReturnVoid()) {
      auto AllLet = true;

      // Gather the items to print for the variable bindings. This can either be
      // a param decl, or a pattern that binds it.
      using DeclOrPattern = llvm::PointerUnion<const Decl *, const Pattern *>;
      SmallVector<DeclOrPattern, 4> ToPrint;
      for (auto *Param : SuccessParams) {
        // Check if we have an inline pattern to print.
        if (auto *P = InlinePatterns.lookup(Param)) {
          if (P->hasAnyMutableBindings())
            AllLet = false;
          ToPrint.push_back(P);
          continue;
        }
        ToPrint.push_back(Param);
      }

      if (AddDeclarations) {
        if (AllLet) {
          OS << tok::kw_let;
        } else {
          OS << tok::kw_var;
        }
        OS << " ";
      }
      // 'res =' or '(res1, res2, ...) ='
      addTupleOf(ToPrint, OS, [&](DeclOrPattern Elt) {
        if (auto *P = Elt.dyn_cast<const Pattern *>()) {
          convertPattern(P);
          return;
        }
        OS << newNameFor(Elt.get<const Decl *>());
      });
      OS << " " << tok::equal << " ";
    }

    if (HandlerDesc.HasError) {
      OS << tok::kw_try << " ";
    }
    OS << "await ";

    // Try to replace the name with that of the alternative. Use the existing
    // name if for some reason that's not possible.
    bool NameAdded = false;
    if (HandlerDesc.Alternative) {
      const ValueDecl *Named = HandlerDesc.Alternative;
      if (auto *Accessor = dyn_cast<AccessorDecl>(HandlerDesc.Alternative))
        Named = Accessor->getStorage();
      if (!Named->getBaseName().isSpecial()) {
        Names.try_emplace(HandlerDesc.Func,
                          Named->getBaseName().getIdentifier());
        convertNode(CE->getFn(), /*StartOverride=*/{}, /*ConvertCalls=*/false,
                    /*IncludeComments=*/false);
        NameAdded = true;
      }
    }
    if (!NameAdded) {
      addRange(CE->getStartLoc(), CE->getFn()->getEndLoc(),
               /*ToEndOfToken=*/true);
    }

    if (!HandlerDesc.alternativeIsAccessor())
      OS << tok::l_paren;

    size_t ConvertedArgIndex = 0;
    ArrayRef<ParamDecl *> AlternativeParams;
    if (HandlerDesc.Alternative)
      AlternativeParams = HandlerDesc.Alternative->getParameters()->getArray();

    for (auto I : indices(*Args)) {
      auto Arg = Args->get(I);
      auto *ArgExpr = Arg.getExpr();
      if (I == HandlerDesc.Index || isa<DefaultArgumentExpr>(ArgExpr))
        continue;

      if (ConvertedArgIndex > 0)
        OS << tok::comma << " ";

      if (HandlerDesc.Alternative) {
        // Skip argument if it's defaulted and has a different name
        while (ConvertedArgIndex < AlternativeParams.size() &&
               AlternativeParams[ConvertedArgIndex]->isDefaultArgument() &&
               AlternativeParams[ConvertedArgIndex]->getArgumentName() !=
               Arg.getLabel()) {
          ConvertedArgIndex++;
        }

        if (ConvertedArgIndex < AlternativeParams.size()) {
          // Could have a different argument label (or none), so add it instead
          auto Name = AlternativeParams[ConvertedArgIndex]->getArgumentName();
          if (!Name.empty())
            OS << Name << ": ";
          convertNode(ArgExpr, /*StartOverride=*/{}, /*ConvertCalls=*/false);

          ConvertedArgIndex++;
          continue;
        }

        // Fallthrough if arguments don't match up for some reason
      }

      // Can't just add the range as we need to perform replacements. Also
      // make sure to include the argument label (if any)
      convertNode(ArgExpr, /*StartOverride=*/Arg.getLabelLoc(),
                  /*ConvertCalls=*/false);
      ConvertedArgIndex++;
    }

    if (!HandlerDesc.alternativeIsAccessor())
      OS << tok::r_paren;
  }

  void addFallbackCatch(const ClosureCallbackParams &Params) {
    auto *ErrParam = Params.getErrParam();
    assert(ErrParam);
    auto ErrName = newNameFor(ErrParam);
    OS << tok::r_brace << " " << tok::kw_catch << " " << tok::l_brace << "\n"
       << ErrName << " = error\n";

    // If we have a known Bool success param, we need to bind it.
    addBoolFlagParamBindingIfNeeded(Params.getKnownBoolFlagParam(),
                                    BlockKind::ERROR);
    OS << tok::r_brace;
  }

  void addCatch(const ParamDecl *ErrParam) {
    OS << "\n" << tok::r_brace << " " << tok::kw_catch << " ";
    auto ErrName = newNameFor(ErrParam, false);
    if (!ErrName.empty() && ErrName != "_") {
      OS << tok::kw_let << " " << ErrName << " ";
    }
    OS << tok::l_brace;
  }

  void preparePlaceholdersAndUnwraps(AsyncHandlerDesc HandlerDesc,
                                     const ClosureCallbackParams &Params,
                                     BlockKind Block) {
    // Params that have been dropped always need placeholdering.
    for (auto *Param : Params.getAllParams()) {
      if (!Params.hasBinding(Param, Block))
        Placeholders.insert(Param);
    }
    // For the fallback case, no other params need placeholdering, as they are
    // all freely accessible in the fallback case.
    if (Block == BlockKind::FALLBACK)
      return;

    switch (HandlerDesc.Type) {
    case HandlerType::PARAMS: {
      auto *ErrParam = Params.getErrParam();
      auto SuccessParams = Params.getSuccessParams();
      switch (Block) {
      case BlockKind::FALLBACK:
        llvm_unreachable("Already handled");
      case BlockKind::ERROR:
        if (ErrParam) {
          if (HandlerDesc.shouldUnwrap(ErrParam->getTypeInContext())) {
            Placeholders.insert(ErrParam);
            Unwraps.insert(ErrParam);
          }
          // Can't use success params in the error body
          Placeholders.insert(SuccessParams.begin(), SuccessParams.end());
        }
        break;
      case BlockKind::SUCCESS:
        for (auto *SuccessParam : SuccessParams) {
          auto Ty = SuccessParam->getTypeInContext();
          if (HandlerDesc.shouldUnwrap(Ty)) {
            // Either unwrap or replace with a placeholder if there's some other
            // reference
            Unwraps.insert(SuccessParam);
            Placeholders.insert(SuccessParam);
          }

          // Void parameters get omitted where possible, so turn any reference
          // into a placeholder, as its usage is unlikely what the user wants.
          if (HandlerDesc.getSuccessParamAsyncReturnType(Ty)->isVoid())
            Placeholders.insert(SuccessParam);
        }
        // Can't use the error param in the success body
        if (ErrParam)
          Placeholders.insert(ErrParam);
        break;
      }
      break;
    }
    case HandlerType::RESULT: {
      // Any uses of the result parameter in the current body (that aren't
      // replaced) are invalid, so replace them with a placeholder.
      auto *ResultParam = Params.getResultParam();
      assert(ResultParam);
      Placeholders.insert(ResultParam);
      break;
    }
    default:
      llvm_unreachable("Unhandled handler type");
    }
  }

  /// Add a mapping from each passed parameter to a new name, possibly
  /// synthesizing a new one if hoisting it would cause a redeclaration or
  /// shadowing. If there's no bound name and \c AddIfMissing is false, no
  /// name will be added.
  void prepareNames(const ClassifiedBlock &Block,
                    ArrayRef<const ParamDecl *> Params,
                    const InlinePatternsToPrint &InlinePatterns,
                    bool AddIfMissing = true) {
    for (auto *PD : Params) {
      // If this param is to be replaced by a pattern that binds multiple
      // separate vars, it's not actually going to be added to the scope, and
      // therefore doesn't need naming. This avoids needing to rename a var with
      // the same name later on in the scope, as it's not actually clashing.
      if (auto *P = InlinePatterns.lookup(PD)) {
        if (!P->getSingleVar())
          continue;
      }
      auto Name = Block.boundName(PD);
      if (Name.empty() && !AddIfMissing)
        continue;

      auto Ident = assignUniqueName(PD, Name);

      // Also propagate the name to any aliases.
      for (auto *Alias : Block.getAliasesFor(PD))
        Names[Alias] = Ident;
    }
  }

  /// Returns a unique name using \c Name as base that doesn't clash with any
  /// other names in the current scope.
  Identifier createUniqueName(StringRef Name) {
    Identifier Ident = getASTContext().getIdentifier(Name);
    if (Name == "_")
      return Ident;

    auto &CurrentNames = Scopes.back().Names;
    if (CurrentNames.count(Ident)) {
      // Add a number to the end of the name until it's unique given the current
      // names in scope.
      llvm::SmallString<32> UniquedName;
      unsigned UniqueId = 1;
      do {
        UniquedName = Name;
        UniquedName.append(std::to_string(UniqueId));
        Ident = getASTContext().getIdentifier(UniquedName);
        UniqueId++;
      } while (CurrentNames.count(Ident));
    }
    return Ident;
  }

  /// Create a unique name for the variable declared by \p D that doesn't
  /// clash with any other names in scope, using \p BoundName as the base name
  /// if not empty and the name of \p D otherwise. Adds this name to both
  /// \c Names and the current scope's names (\c Scopes.Names).
  Identifier assignUniqueName(const Decl *D, StringRef BoundName) {
    Identifier Ident;
    if (BoundName.empty()) {
      BoundName = getDeclName(D).userFacingName();
      if (BoundName.empty())
        return Ident;
    }

    if (BoundName.startswith("$")) {
      llvm::SmallString<8> NewName;
      NewName.append("val");
      NewName.append(BoundName.drop_front());
      Ident = createUniqueName(NewName);
    } else {
      Ident = createUniqueName(BoundName);
    }

    Names.try_emplace(D, Ident);
    Scopes.back().Names.insert(Ident);
    return Ident;
  }

  StringRef newNameFor(const Decl *D, bool Required = true) {
    auto Res = Names.find(D);
    if (Res == Names.end()) {
      assert(!Required && "Missing name for decl when one was required");
      return StringRef();
    }
    return Res->second.str();
  }

  void addNewScope(const llvm::DenseSet<const Decl *> &Decls) {
    if (Scopes.empty()) {
      Scopes.emplace_back(/*ContinuationName=*/Identifier());
    } else {
      // If the parent scope is nested in a continuation, the new one is also.
      // Carry over the continuation name.
      Identifier PreviousContinuationName = Scopes.back().ContinuationName;
      Scopes.emplace_back(PreviousContinuationName);
    }
    for (auto D : Decls) {
      auto Name = getDeclName(D);
      if (!Name.empty())
        Scopes.back().Names.insert(Name);
    }
  }

  void clearNames(ArrayRef<const ParamDecl *> Params) {
    for (auto *Param : Params) {
      Unwraps.erase(Param);
      Placeholders.erase(Param);
      Names.erase(Param);
    }
  }

  /// Adds a forwarding call to the old completion handler function, with
  /// \p HandlerReplacement that allows for a custom replacement or, if empty,
  /// removal of the completion handler closure.
  void addForwardingCallTo(const FuncDecl *FD, StringRef HandlerReplacement) {
    OS << FD->getBaseName() << tok::l_paren;

    auto *Params = FD->getParameters();
    size_t ConvertedArgsIndex = 0;
    for (size_t I = 0, E = Params->size(); I < E; ++I) {
      if (I == TopHandler.Index) {
        /// If we're not replacing the handler with anything, drop it.
        if (HandlerReplacement.empty())
          continue;

        // Use a trailing closure if the handler is the last param
        if (I == E - 1) {
          OS << tok::r_paren << " ";
          OS << HandlerReplacement;
          return;
        }

        // Otherwise fall through to do the replacement.
      }

      if (ConvertedArgsIndex > 0)
        OS << tok::comma << " ";

      const auto *Param = Params->get(I);
      if (!Param->getArgumentName().empty())
        OS << Param->getArgumentName() << tok::colon << " ";

      if (I == TopHandler.Index) {
        OS << HandlerReplacement;
      } else {
        OS << Param->getParameterName();
      }

      ConvertedArgsIndex++;
    }
    OS << tok::r_paren;
  }

  /// Adds a forwarded error argument to a completion handler call. If the error
  /// type of \p HandlerDesc is more specialized than \c Error, an
  /// 'as! CustomError' cast to the more specialized error type will be added to
  /// the output stream.
  void addForwardedErrorArgument(StringRef ErrorName,
                                 const AsyncHandlerDesc &HandlerDesc) {
    // If the error type is already Error, we can pass it as-is.
    auto ErrorType = *HandlerDesc.getErrorType();
    if (ErrorType->getCanonicalType() ==
        getASTContext().getErrorExistentialType()) {
      OS << ErrorName;
      return;
    }

    // Otherwise we need to add a force cast to the destination custom error
    // type. If this is for an Error? parameter, we'll need to add parens around
    // the cast to silence a compiler warning about force casting never
    // producing nil.
    auto RequiresParens = HandlerDesc.getErrorParam().has_value();
    if (RequiresParens)
      OS << tok::l_paren;

    OS << ErrorName << " " << tok::kw_as << tok::exclaim_postfix << " ";
    ErrorType->lookThroughSingleOptionalType()->print(OS);

    if (RequiresParens)
      OS << tok::r_paren;
  }

  /// If \p T has a natural default value like \c nil for \c Optional or \c ()
  /// for \c Void, add that default value to the output. Otherwise, add a
  /// placeholder that contains \p T's name as the hint.
  void addDefaultValueOrPlaceholder(Type T) {
    if (T->isOptional()) {
      OS << tok::kw_nil;
    } else if (T->isVoid()) {
      OS << "()";
    } else {
      OS << "<#";
      T.print(OS);
      OS << "#>";
    }
  }

  /// Adds the \c Index -th parameter to the completion handler described by \p
  /// HanderDesc.
  /// If \p ResultName is not empty, it is assumed that a variable with that
  /// name contains the result returned from the async alternative. If the
  /// callback also takes an error parameter, \c nil passed to the completion
  /// handler for the error. If \p ResultName is empty, it is a assumed that a
  /// variable named 'error' contains the error thrown from the async method and
  /// 'nil' will be passed to the completion handler for all result parameters.
  void addCompletionHandlerArgument(size_t Index, StringRef ResultName,
                                    const AsyncHandlerDesc &HandlerDesc) {
    if (HandlerDesc.HasError && Index == HandlerDesc.params().size() - 1) {
      // The error parameter is the last argument of the completion handler.
      if (ResultName.empty()) {
        addForwardedErrorArgument("error", HandlerDesc);
      } else {
        addDefaultValueOrPlaceholder(HandlerDesc.params()[Index].getPlainType());
      }
    } else {
      if (ResultName.empty()) {
        addDefaultValueOrPlaceholder(HandlerDesc.params()[Index].getPlainType());
      } else if (HandlerDesc
                     .getSuccessParamAsyncReturnType(
                         HandlerDesc.params()[Index].getPlainType())
                     ->isVoid()) {
        // Void return types are not returned by the async function, synthesize
        // a Void instance.
        OS << tok::l_paren << tok::r_paren;
      } else if (HandlerDesc.getSuccessParams().size() > 1) {
        // If the async method returns a tuple, we need to pass its elements to
        // the completion handler separately. For example:
        //
        // func foo() async -> (String, Int) {}
        //
        // causes the following legacy body to be created:
        //
        // func foo(completion: (String, Int) -> Void) {
        //   Task {
        //     let result = await foo()
        //     completion(result.0, result.1)
        //   }
        // }
        OS << ResultName << tok::period;

        auto Label = HandlerDesc.getAsyncReturnTypeLabel(Index);
        if (!Label.empty()) {
          OS << Label;
        } else {
          OS << Index;
        }
      } else {
        OS << ResultName;
      }
    }
  }

  /// Add a call to the completion handler named \p HandlerName and described by
  /// \p HandlerDesc, passing all the required arguments. See \c
  /// getCompletionHandlerArgument for how the arguments are synthesized.
  void addCallToCompletionHandler(StringRef ResultName,
                                  const AsyncHandlerDesc &HandlerDesc,
                                  StringRef HandlerName) {
    OS << HandlerName << tok::l_paren;

    // Construct arguments to pass to the completion handler
    switch (HandlerDesc.Type) {
    case HandlerType::INVALID:
      llvm_unreachable("Cannot be rewritten");
      break;
    case HandlerType::PARAMS: {
      for (size_t I = 0; I < HandlerDesc.params().size(); ++I) {
        if (I > 0) {
          OS << tok::comma << " ";
        }
        addCompletionHandlerArgument(I, ResultName, HandlerDesc);
      }
      break;
    }
    case HandlerType::RESULT: {
      if (!ResultName.empty()) {
        OS << tok::period_prefix << "success" << tok::l_paren;
        if (!HandlerDesc.willAsyncReturnVoid()) {
          OS << ResultName;
        } else {
          OS << tok::l_paren << tok::r_paren;
        }
        OS << tok::r_paren;
      } else {
        OS << tok::period_prefix << "failure" << tok::l_paren;
        addForwardedErrorArgument("error", HandlerDesc);
        OS << tok::r_paren;
      }
      break;
    }
    }
    OS << tok::r_paren; // Close the call to the completion handler
  }

  /// Adds the result type of a refactored async function that previously
  /// returned results via a completion handler described by \p HandlerDesc.
  void addAsyncFuncReturnType(const AsyncHandlerDesc &HandlerDesc) {
    // Type or (Type1, Type2, ...)
    SmallVector<LabeledReturnType, 2> Scratch;
    auto ReturnTypes = HandlerDesc.getAsyncReturnTypes(Scratch);
    if (ReturnTypes.empty()) {
      OS << "Void";
    } else {
      addTupleOf(ReturnTypes, OS, [&](LabeledReturnType LabelAndType) {
        if (!LabelAndType.Label.empty()) {
          OS << LabelAndType.Label << tok::colon << " ";
        }
        LabelAndType.Ty->print(OS);
      });
    }
  }

  /// If \p FD is generic, adds a type annotation with the return type of the
  /// converted async function. This is used when creating a legacy function,
  /// calling the converted 'async' function so that the generic parameters of
  /// the legacy function are passed to the generic function. For example for
  /// \code
  /// func foo<GenericParam>() async -> GenericParam {}
  /// \endcode
  /// we generate
  /// \code
  /// func foo<GenericParam>(completion: (GenericParam) -> Void) {
  ///   Task {
  ///     let result: GenericParam = await foo()
  ///               <------------>
  ///     completion(result)
  ///   }
  /// }
  /// \endcode
  /// This function adds the range marked by \c <----->
  void addResultTypeAnnotationIfNecessary(const FuncDecl *FD,
                                          const AsyncHandlerDesc &HandlerDesc) {
    if (FD->isGeneric()) {
      OS << tok::colon << " ";
      addAsyncFuncReturnType(HandlerDesc);
    }
  }
};

} // namespace asyncrefactorings

bool RefactoringActionConvertCallToAsyncAlternative::isApplicable(
    ResolvedCursorInfoPtr CursorInfo, DiagnosticEngine &Diag) {
  using namespace asyncrefactorings;

  // Currently doesn't check that the call is in an async context. This seems
  // possibly useful in some situations, so we'll see what the feedback is.
  // May need to change in the future
  auto *CE = findOuterCall(CursorInfo);
  if (!CE)
    return false;

  auto HandlerDesc = AsyncHandlerParamDesc::find(
      getUnderlyingFunc(CE->getFn()), /*RequireAttributeOrName=*/false);
  return HandlerDesc.isValid();
}

/// Converts a call of a function with a possible async alternative, to use it
/// instead. Currently this is any function that
///   1. has a void return type,
///   2. has a void returning closure as its last parameter, and
///   3. is not already async
///
/// For now the call need not be in an async context, though this may change
/// depending on feedback.
bool RefactoringActionConvertCallToAsyncAlternative::performChange() {
  using namespace asyncrefactorings;

  auto *CE = findOuterCall(CursorInfo);
  assert(CE &&
         "Should not run performChange when refactoring is not applicable");

  // Find the scope this call is in
  ContextFinder Finder(
      *CursorInfo->getSourceFile(), CursorInfo->getLoc(),
      [](ASTNode N) { return N.isStmt(StmtKind::Brace) && !N.isImplicit(); });
  Finder.resolve();
  auto Scopes = Finder.getContexts();
  BraceStmt *Scope = nullptr;
  if (!Scopes.empty())
    Scope = cast<BraceStmt>(Scopes.back().get<Stmt *>());

  AsyncConverter Converter(TheFile, SM, DiagEngine, CE, Scope);
  if (!Converter.convert())
    return true;

  Converter.replace(CE, EditConsumer);
  return false;
}

bool RefactoringActionConvertToAsync::isApplicable(
    ResolvedCursorInfoPtr CursorInfo, DiagnosticEngine &Diag) {
  using namespace asyncrefactorings;

  // As with the call refactoring, should possibly only apply if there's
  // actually calls to async alternatives. At the moment this will just add
  // `async` if there are no calls, which is probably fine.
  return findFunction(CursorInfo);
}

/// Converts a whole function to async, converting any calls to functions with
/// async alternatives as above.
bool RefactoringActionConvertToAsync::performChange() {
  using namespace asyncrefactorings;

  auto *FD = findFunction(CursorInfo);
  assert(FD &&
         "Should not run performChange when refactoring is not applicable");

  auto HandlerDesc =
      AsyncHandlerParamDesc::find(FD, /*RequireAttributeOrName=*/false);
  AsyncConverter Converter(TheFile, SM, DiagEngine, FD, HandlerDesc);
  if (!Converter.convert())
    return true;

  Converter.replace(FD, EditConsumer, FD->getSourceRangeIncludingAttrs().Start);
  return false;
}

bool RefactoringActionAddAsyncAlternative::isApplicable(
    ResolvedCursorInfoPtr CursorInfo, DiagnosticEngine &Diag) {
  using namespace asyncrefactorings;

  auto *FD = findFunction(CursorInfo);
  if (!FD)
    return false;

  auto HandlerDesc =
      AsyncHandlerParamDesc::find(FD, /*RequireAttributeOrName=*/false);
  return HandlerDesc.isValid();
}

/// Adds an async alternative and marks the current function as deprecated.
/// Equivalent to the conversion but
///   1. only works on functions that themselves are a possible async
///      alternative, and
///   2. has extra handling to convert the completion/handler/callback closure
///      parameter to either `return`/`throws`
bool RefactoringActionAddAsyncAlternative::performChange() {
  using namespace asyncrefactorings;

  auto *FD = findFunction(CursorInfo);
  assert(FD &&
         "Should not run performChange when refactoring is not applicable");

  auto HandlerDesc =
      AsyncHandlerParamDesc::find(FD, /*RequireAttributeOrName=*/false);
  assert(HandlerDesc.isValid() &&
         "Should not run performChange when refactoring is not applicable");

  AsyncConverter Converter(TheFile, SM, DiagEngine, FD, HandlerDesc);
  if (!Converter.convert())
    return true;

  // Add a reference to the async function so that warnings appear when the
  // synchronous function is used in an async context
  SmallString<128> AvailabilityAttr = HandlerDesc.buildRenamedAttribute();
  EditConsumer.accept(SM, FD->getAttributeInsertionLoc(false),
                      AvailabilityAttr);

  AsyncConverter LegacyBodyCreator(TheFile, SM, DiagEngine, FD, HandlerDesc);
  if (LegacyBodyCreator.createLegacyBody()) {
    LegacyBodyCreator.replace(FD->getBody(), EditConsumer);
  }

  // Add the async alternative
  Converter.insertAfter(FD, EditConsumer);

  return false;
}

bool RefactoringActionAddAsyncWrapper::isApplicable(
    ResolvedCursorInfoPtr CursorInfo, DiagnosticEngine &Diag) {
  using namespace asyncrefactorings;

  auto *FD = findFunction(CursorInfo);
  if (!FD)
    return false;

  auto HandlerDesc =
      AsyncHandlerParamDesc::find(FD, /*RequireAttributeOrName=*/false);
  return HandlerDesc.isValid();
}

bool RefactoringActionAddAsyncWrapper::performChange() {
  using namespace asyncrefactorings;

  auto *FD = findFunction(CursorInfo);
  assert(FD &&
         "Should not run performChange when refactoring is not applicable");

  auto HandlerDesc =
      AsyncHandlerParamDesc::find(FD, /*RequireAttributeOrName=*/false);
  assert(HandlerDesc.isValid() &&
         "Should not run performChange when refactoring is not applicable");

  AsyncConverter Converter(TheFile, SM, DiagEngine, FD, HandlerDesc);
  if (!Converter.createAsyncWrapper())
    return true;

  // Add a reference to the async function so that warnings appear when the
  // synchronous function is used in an async context
  SmallString<128> AvailabilityAttr = HandlerDesc.buildRenamedAttribute();
  EditConsumer.accept(SM, FD->getAttributeInsertionLoc(false),
                      AvailabilityAttr);

  // Add the async wrapper.
  Converter.insertAfter(FD, EditConsumer);

  return false;
}

StringRef swift::ide::
getDescriptiveRefactoringKindName(RefactoringKind Kind) {
    switch(Kind) {
      case RefactoringKind::None:
        llvm_unreachable("Should be a valid refactoring kind");
#define REFACTORING(KIND, NAME, ID) case RefactoringKind::KIND: return NAME;
#include "swift/Refactoring/RefactoringKinds.def"
    }
    llvm_unreachable("unhandled kind");
  }

  StringRef swift::ide::getDescriptiveRenameUnavailableReason(
      RefactorAvailableKind Kind) {
    switch(Kind) {
    case RefactorAvailableKind::Available:
      return "";
    case RefactorAvailableKind::Unavailable_system_symbol:
      return "symbol from system module cannot be renamed";
    case RefactorAvailableKind::Unavailable_has_no_location:
      return "symbol without a declaration location cannot be renamed";
    case RefactorAvailableKind::Unavailable_has_no_name:
      return "cannot find the name of the symbol";
    case RefactorAvailableKind::Unavailable_has_no_accessibility:
      return "cannot decide the accessibility of the symbol";
    case RefactorAvailableKind::Unavailable_decl_from_clang:
      return "cannot rename a Clang symbol from its Swift reference";
    case RefactorAvailableKind::Unavailable_decl_in_macro:
      return "cannot rename a symbol declared in a macro";
    }
    llvm_unreachable("unhandled kind");
  }

SourceLoc swift::ide::RangeConfig::getStart(SourceManager &SM) {
  return SM.getLocForLineCol(BufferID, Line, Column);
}

SourceLoc swift::ide::RangeConfig::getEnd(SourceManager &SM) {
  return getStart(SM).getAdvancedLoc(Length);
}

struct swift::ide::FindRenameRangesAnnotatingConsumer::Implementation {
  std::unique_ptr<SourceEditConsumer> pRewriter;
  Implementation(SourceManager &SM, unsigned BufferId, raw_ostream &OS)
  : pRewriter(new SourceEditOutputConsumer(SM, BufferId, OS)) {}
  static StringRef tag(RefactoringRangeKind Kind) {
    switch (Kind) {
      case RefactoringRangeKind::BaseName:
        return "base";
      case RefactoringRangeKind::KeywordBaseName:
        return "keywordBase";
      case RefactoringRangeKind::ParameterName:
        return "param";
      case RefactoringRangeKind::NoncollapsibleParameterName:
        return "noncollapsibleparam";
      case RefactoringRangeKind::DeclArgumentLabel:
        return "arglabel";
      case RefactoringRangeKind::CallArgumentLabel:
        return "callarg";
      case RefactoringRangeKind::CallArgumentColon:
        return "callcolon";
      case RefactoringRangeKind::CallArgumentCombined:
        return "callcombo";
      case RefactoringRangeKind::SelectorArgumentLabel:
        return "sel";
    }
    llvm_unreachable("unhandled kind");
  }
  void accept(SourceManager &SM, const RenameRangeDetail &Range) {
    std::string NewText;
    llvm::raw_string_ostream OS(NewText);
    StringRef Tag = tag(Range.RangeKind);
    OS << "<" << Tag;
    if (Range.Index.has_value())
      OS << " index=" << *Range.Index;
    OS << ">" << Range.Range.str() << "</" << Tag << ">";
    pRewriter->accept(SM, {/*Path=*/{}, Range.Range, /*BufferName=*/{},
                           OS.str(), /*RegionsWorthNote=*/{}});
  }
};

swift::ide::FindRenameRangesAnnotatingConsumer::
FindRenameRangesAnnotatingConsumer(SourceManager &SM, unsigned BufferId,
                                   raw_ostream &OS) :
    Impl(*new Implementation(SM, BufferId, OS)) {}

swift::ide::FindRenameRangesAnnotatingConsumer::~FindRenameRangesAnnotatingConsumer() {
  delete &Impl;
}

void swift::ide::FindRenameRangesAnnotatingConsumer::
accept(SourceManager &SM, RegionType RegionType,
       ArrayRef<RenameRangeDetail> Ranges) {
  if (RegionType == RegionType::Mismatch || RegionType == RegionType::Unmatched)
    return;
  for (const auto &Range : Ranges) {
    Impl.accept(SM, Range);
  }
}

SmallVector<RefactorAvailabilityInfo, 0>
swift::ide::collectRefactorings(ResolvedCursorInfoPtr CursorInfo,
                                bool ExcludeRename) {
  SmallVector<RefactorAvailabilityInfo, 0> Infos;

  DiagnosticEngine DiagEngine(
      CursorInfo->getSourceFile()->getASTContext().SourceMgr);

  // Only macro expansion is available within generated buffers
  if (CursorInfo->getSourceFile()->Kind == SourceFileKind::MacroExpansion) {
    if (RefactoringActionInlineMacro::isApplicable(CursorInfo, DiagEngine)) {
      Infos.emplace_back(RefactoringKind::InlineMacro,
                         RefactorAvailableKind::Available);
    }
    return Infos;
  }

  if (!ExcludeRename) {
    if (auto Info = getRenameInfo(CursorInfo)) {
      Infos.push_back(std::move(Info->Availability));
    }
  }

#define CURSOR_REFACTORING(KIND, NAME, ID)                                     \
  if (RefactoringKind::KIND != RefactoringKind::LocalRename &&                 \
      RefactoringAction##KIND::isApplicable(CursorInfo, DiagEngine))           \
    Infos.emplace_back(RefactoringKind::KIND, RefactorAvailableKind::Available);
#include "swift/Refactoring/RefactoringKinds.def"

  return Infos;
}

SmallVector<RefactorAvailabilityInfo, 0>
swift::ide::collectRefactorings(SourceFile *SF, RangeConfig Range,
                                bool &CollectRangeStartRefactorings,
                                ArrayRef<DiagnosticConsumer *> DiagConsumers) {
  if (Range.Length == 0)
    return collectRefactoringsAtCursor(SF, Range.Line, Range.Column,
                                       DiagConsumers);

  // No refactorings are available within generated buffers
  if (SF->Kind == SourceFileKind::MacroExpansion)
    return {};

  // Prepare the tool box.
  ASTContext &Ctx = SF->getASTContext();
  SourceManager &SM = Ctx.SourceMgr;
  DiagnosticEngine DiagEngine(SM);
  std::for_each(DiagConsumers.begin(), DiagConsumers.end(),
    [&](DiagnosticConsumer *Con) { DiagEngine.addConsumer(*Con); });
  ResolvedRangeInfo Result = evaluateOrDefault(SF->getASTContext().evaluator,
    RangeInfoRequest(RangeInfoOwner({SF,
                      Range.getStart(SF->getASTContext().SourceMgr),
                      Range.getEnd(SF->getASTContext().SourceMgr)})),
                                               ResolvedRangeInfo());

  bool enableInternalRefactoring = getenv("SWIFT_ENABLE_INTERNAL_REFACTORING_ACTIONS");

  SmallVector<RefactorAvailabilityInfo, 0> Infos;

#define RANGE_REFACTORING(KIND, NAME, ID)                                      \
  if (RefactoringAction##KIND::isApplicable(Result, DiagEngine))               \
    Infos.emplace_back(RefactoringKind::KIND, RefactorAvailableKind::Available);
#define INTERNAL_RANGE_REFACTORING(KIND, NAME, ID)                            \
  if (enableInternalRefactoring)                                              \
    RANGE_REFACTORING(KIND, NAME, ID)
#include "swift/Refactoring/RefactoringKinds.def"

  CollectRangeStartRefactorings = collectRangeStartRefactorings(Result);

  return Infos;
}

bool swift::ide::
refactorSwiftModule(ModuleDecl *M, RefactoringOptions Opts,
                    SourceEditConsumer &EditConsumer,
                    DiagnosticConsumer &DiagConsumer) {
  assert(Opts.Kind != RefactoringKind::None && "should have a refactoring kind.");

  // Use the default name if not specified.
  if (Opts.PreferredName.empty()) {
    Opts.PreferredName = getDefaultPreferredName(Opts.Kind).str();
  }

  switch (Opts.Kind) {
#define SEMANTIC_REFACTORING(KIND, NAME, ID)                                   \
case RefactoringKind::KIND: {                                                  \
      RefactoringAction##KIND Action(M, Opts, EditConsumer, DiagConsumer);     \
      if (RefactoringKind::KIND == RefactoringKind::LocalRename ||             \
          RefactoringKind::KIND == RefactoringKind::ExpandMacro ||             \
          Action.isApplicable())                                               \
        return Action.performChange();                                         \
      return true;                                                             \
  }
#include "swift/Refactoring/RefactoringKinds.def"
    case RefactoringKind::GlobalRename:
    case RefactoringKind::FindGlobalRenameRanges:
    case RefactoringKind::FindLocalRenameRanges:
      llvm_unreachable("not a valid refactoring kind");
    case RefactoringKind::None:
      llvm_unreachable("should not enter here.");
  }
  llvm_unreachable("unhandled kind");
}

static std::vector<ResolvedLoc>
resolveRenameLocations(ArrayRef<RenameLoc> RenameLocs, SourceFile &SF,
                       DiagnosticEngine &Diags) {
  SourceManager &SM = SF.getASTContext().SourceMgr;
  unsigned BufferID = SF.getBufferID().value();

  std::vector<UnresolvedLoc> UnresolvedLocs;
  for (const RenameLoc &RenameLoc : RenameLocs) {
    DeclNameViewer OldName(RenameLoc.OldName);
    SourceLoc Location = SM.getLocForLineCol(BufferID, RenameLoc.Line,
                                             RenameLoc.Column);

    if (!OldName.isValid()) {
      Diags.diagnose(Location, diag::invalid_name, RenameLoc.OldName);
      return {};
    }

    if (!RenameLoc.NewName.empty()) {
      DeclNameViewer NewName(RenameLoc.NewName);
      ArrayRef<StringRef> ParamNames = NewName.args();
      bool newOperator = Lexer::isOperator(NewName.base());
      bool NewNameIsValid = NewName.isValid() &&
        (Lexer::isIdentifier(NewName.base()) || newOperator) &&
        std::all_of(ParamNames.begin(), ParamNames.end(), [](StringRef Label) {
          return Label.empty() || Lexer::isIdentifier(Label);
        });

      if (!NewNameIsValid) {
        Diags.diagnose(Location, diag::invalid_name, RenameLoc.NewName);
        return {};
      }

      if (NewName.partsCount() != OldName.partsCount()) {
        Diags.diagnose(Location, diag::arity_mismatch, RenameLoc.NewName,
                       RenameLoc.OldName);
        return {};
      }

      if (RenameLoc.Usage == NameUsage::Call && !RenameLoc.IsFunctionLike) {
        Diags.diagnose(Location, diag::name_not_functionlike, RenameLoc.NewName);
        return {};
      }
    }

    bool isOperator = Lexer::isOperator(OldName.base());
    UnresolvedLocs.push_back({
      Location,
      (RenameLoc.Usage == NameUsage::Unknown ||
       (RenameLoc.Usage == NameUsage::Call && !isOperator))
    });
  }

  NameMatcher Resolver(SF);
  return Resolver.resolve(UnresolvedLocs, SF.getAllTokens());
}

int swift::ide::syntacticRename(SourceFile *SF, ArrayRef<RenameLoc> RenameLocs,
                                SourceEditConsumer &EditConsumer,
                                DiagnosticConsumer &DiagConsumer) {

  assert(SF && "null source file");

  SourceManager &SM = SF->getASTContext().SourceMgr;
  DiagnosticEngine DiagEngine(SM);
  DiagEngine.addConsumer(DiagConsumer);

  auto ResolvedLocs = resolveRenameLocations(RenameLocs, *SF, DiagEngine);
  if (ResolvedLocs.size() != RenameLocs.size())
    return true; // Already diagnosed.

  size_t index = 0;
  llvm::StringSet<> ReplaceTextContext;
  for(const RenameLoc &Rename: RenameLocs) {
    ResolvedLoc &Resolved = ResolvedLocs[index++];
    TextReplacementsRenamer Renamer(SM, Rename.OldName, Rename.NewName,
                                    ReplaceTextContext);
    RegionType Type = Renamer.addSyntacticRenameRanges(Resolved, Rename);
    if (Type == RegionType::Mismatch) {
      DiagEngine.diagnose(Resolved.Range.getStart(), diag::mismatched_rename,
                          Rename.NewName);
      EditConsumer.accept(SM, Type, llvm::None);
    } else {
      EditConsumer.accept(SM, Type, Renamer.getReplacements());
    }
  }

  return false;
}

int swift::ide::findSyntacticRenameRanges(
    SourceFile *SF, ArrayRef<RenameLoc> RenameLocs,
    FindRenameRangesConsumer &RenameConsumer,
    DiagnosticConsumer &DiagConsumer) {
  assert(SF && "null source file");

  SourceManager &SM = SF->getASTContext().SourceMgr;
  DiagnosticEngine DiagEngine(SM);
  DiagEngine.addConsumer(DiagConsumer);

  auto ResolvedLocs = resolveRenameLocations(RenameLocs, *SF, DiagEngine);
  if (ResolvedLocs.size() != RenameLocs.size())
    return true; // Already diagnosed.

  size_t index = 0;
  for (const RenameLoc &Rename : RenameLocs) {
    ResolvedLoc &Resolved = ResolvedLocs[index++];
    RenameRangeDetailCollector Renamer(SM, Rename.OldName);
    RegionType Type = Renamer.addSyntacticRenameRanges(Resolved, Rename);
    if (Type == RegionType::Mismatch) {
      DiagEngine.diagnose(Resolved.Range.getStart(), diag::mismatched_rename,
                          Rename.NewName);
      RenameConsumer.accept(SM, Type, llvm::None);
    } else {
      RenameConsumer.accept(SM, Type, Renamer.Ranges);
    }
  }

  return false;
}

int swift::ide::findLocalRenameRanges(
    SourceFile *SF, RangeConfig Range,
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
