//===--- IDETypeChecking.cpp ----------------------------------------------===//
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

#include "swift/Sema/IDETypeChecking.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTDemangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/IDE/IDERequests.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/IDETypeCheckingRequests.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

void swift::getTopLevelDeclsForDisplay(ModuleDecl *M,
                                       SmallVectorImpl<Decl *> &Results,
                                       bool Recursive) {
  auto getDisplayDeclsForModule =
      [Recursive](ModuleDecl *M, SmallVectorImpl<Decl *> &Results) {
        M->getDisplayDecls(Results, Recursive);
      };
  getTopLevelDeclsForDisplay(M, Results, std::move(getDisplayDeclsForModule));
}

void swift::getTopLevelDeclsForDisplay(
    ModuleDecl *M, SmallVectorImpl<Decl *> &Results,
    llvm::function_ref<void(ModuleDecl *, SmallVectorImpl<Decl *> &)>
        getDisplayDeclsForModule) {
  auto startingSize = Results.size();
  getDisplayDeclsForModule(M, Results);

  // Force Sendable on all public types, which might synthesize some extensions.
  // FIXME: We can remove this if @_nonSendable stops creating extensions.
  for (auto result : Results) {
    if (auto NTD = dyn_cast<NominalTypeDecl>(result)) {

      // Restrict this logic to public and package types. Non-public types
      // may refer to implementation details and fail at deserialization.
      auto accessScope = NTD->getFormalAccessScope();
      if (!M->isMainModule() && !accessScope.isPublic() &&
          !accessScope.isPackage())
        continue;

      auto proto = M->getASTContext().getProtocol(KnownProtocolKind::Sendable);
      if (proto)
        (void) lookupConformance(NTD->getDeclaredInterfaceType(), proto);
    }
  }

  // Remove what we fetched and fetch again, possibly now with additional
  // extensions.
  Results.resize(startingSize);
  getDisplayDeclsForModule(M, Results);
}

static bool shouldPrintAsFavorable(const Decl *D, const PrintOptions &Options) {
  if (!Options.TransformContext ||
      !isa<ExtensionDecl>(D->getDeclContext()) ||
      !Options.TransformContext->isPrintingSynthesizedExtension())
    return true;
  auto DC = Options.TransformContext->getDeclContext();
  auto BaseTy = Options.TransformContext->getBaseType();
  const auto *FD = dyn_cast<FuncDecl>(D);
  if (!FD)
    return true;
  // Don't check overload choices for accessor decls.
  if (isa<AccessorDecl>(FD))
    return true;
  ResolvedMemberResult Result =
      resolveValueMember(*DC, BaseTy, FD->getEffectiveFullName());
  return !(Result.hasBestOverload() && Result.getBestOverload() != D);
}

class ModulePrinterPrintableChecker: public ShouldPrintChecker {
  bool shouldPrint(const Decl *D, const PrintOptions &Options) override {
    if (!shouldPrintAsFavorable(D, Options))
      return false;
    return ShouldPrintChecker::shouldPrint(D, Options);
  }
};

PrintOptions PrintOptions::printModuleInterface(bool printFullConvention) {
  PrintOptions result = printInterface(printFullConvention);
  result.CurrentPrintabilityChecker.reset(new ModulePrinterPrintableChecker());
  return result;
}

PrintOptions PrintOptions::printTypeInterface(Type T,
                                              bool printFullConvention) {
  PrintOptions result = printModuleInterface(printFullConvention);
  result.PrintExtensionFromConformingProtocols = true;
  result.TransformContext = TypeTransformContext(T);
  result.printExtensionContentAsMembers = [T](const ExtensionDecl *ED) {
    return isExtensionApplied(
        T->getNominalOrBoundGenericNominal()->getDeclContext(), T, ED);
  };
  result.CurrentPrintabilityChecker.reset(new ModulePrinterPrintableChecker());
  return result;
}

PrintOptions PrintOptions::printDocInterface() {
  PrintOptions result =
      PrintOptions::printModuleInterface(/*printFullConvention*/ false);
  result.PrintAccess = false;
  result.SkipUnavailable = false;
  result.ExcludeAttrList.push_back(DeclAttrKind::Available);
  result.ArgAndParamPrinting =
      PrintOptions::ArgAndParamPrintingMode::BothAlways;
  result.PrintDocumentationComments = false;
  result.PrintFunctionRepresentationAttrs =
    PrintOptions::FunctionRepresentationMode::None;
  return result;
}

struct SynthesizedExtensionAnalyzer::Implementation {
  static bool isMemberFavored(const NominalTypeDecl* Target, const Decl* D) {
    DeclContext* DC = Target->getInnermostDeclContext();
    Type BaseTy = Target->getDeclaredTypeInContext();
    const auto *FD = dyn_cast<FuncDecl>(D);
    if (!FD)
      return true;
    ResolvedMemberResult Result = resolveValueMember(*DC, BaseTy,
                                                     FD->getEffectiveFullName());
    return !(Result.hasBestOverload() && Result.getBestOverload() != D);
  }

  static bool isExtensionFavored(const NominalTypeDecl* Target,
                                 const ExtensionDecl *ED) {
    return std::find_if(ED->getMembers().begin(), ED->getMembers().end(),
      [&](DeclIterator It) {
        return isMemberFavored(Target, *It);}) != ED->getMembers().end();
  }

  struct SynthesizedExtensionInfo {
    ExtensionDecl *Ext = nullptr;
    bool IsSynthesized;
    ExtensionDecl *EnablingExt = nullptr;
    operator bool() const { return Ext; }
    SynthesizedExtensionInfo(bool IsSynthesized = false,
                             ExtensionDecl *EnablingExt = nullptr)
        : IsSynthesized(IsSynthesized), EnablingExt(EnablingExt) {}
    bool operator< (const SynthesizedExtensionInfo& Rhs) const {

      // Synthesized are always after actual ones.
      if (IsSynthesized != Rhs.IsSynthesized)
        return !IsSynthesized;

      // If not from the same file, sort by file name.
      if (auto LFile = Ext->getSourceFileName()) {
        if (auto RFile = Rhs.Ext->getSourceFileName()) {
          int Result = LFile.value().compare(RFile.value());
          if (Result != 0)
            return Result < 0;
        }
      }

      // Otherwise, sort by source order.
      if (auto LeftOrder = Ext->getSourceOrder()) {
        if (auto RightOrder = Rhs.Ext->getSourceOrder()) {
          return LeftOrder.value() < RightOrder.value();
        }
      }

      return false;
    }
  };

  struct ExtensionMergeInfo {
    struct Requirement {
      swift::Requirement Req;

      bool operator<(const Requirement& Rhs) const {
        if (auto result = unsigned(Req.getKind()) - unsigned(Rhs.Req.getKind())) {
          return result < 0;
        } else if (!Req.getFirstType()->isEqual(Rhs.Req.getFirstType())) {
          return (Req.getFirstType()->getCanonicalType() <
                  Rhs.Req.getFirstType()->getCanonicalType());
        } else if (Req.getKind() != RequirementKind::Layout) {
          return (Req.getSecondType()->getCanonicalType() <
                  Rhs.Req.getSecondType()->getCanonicalType());
        }

        return false;
      }

      bool operator== (const Requirement& Rhs) const {
        return Req.getCanonical() == Rhs.Req.getCanonical();
      }
    };

    bool Unmergable;
    unsigned InheritsCount;
    std::set<Requirement> Requirements;
    void addRequirement(swift::Requirement Req) {
      Requirements.insert({Req});
    }
    bool operator== (const ExtensionMergeInfo& Another) const {
      // Trivially unmergeable.
      if (Unmergable || Another.Unmergable)
        return false;
      if (InheritsCount != 0 || Another.InheritsCount != 0)
        return false;
      return Requirements == Another.Requirements;
    }
    bool isMergeableWithTypeDef() {
      return !Unmergable && InheritsCount == 0 && Requirements.empty();
    }
  };

  using ExtensionInfoMap =
      llvm::MapVector<ExtensionDecl *, SynthesizedExtensionInfo>;
  using ExtensionMergeInfoMap =
      llvm::MapVector<ExtensionDecl *, ExtensionMergeInfo>;

  struct ExtensionMergeGroup {

    unsigned RequirementsCount;
    unsigned InheritanceCount;
    MergeGroupKind Kind;
    std::vector<SynthesizedExtensionInfo*> Members;

    ExtensionMergeGroup(SynthesizedExtensionInfo *Info,
                        unsigned RequirementsCount,
                        unsigned InheritanceCount,
                        bool MergeableWithType) :
    RequirementsCount(RequirementsCount),
    InheritanceCount(InheritanceCount),
    Kind(MergeableWithType ? MergeGroupKind::MergeableWithTypeDef :
         MergeGroupKind::UnmergeableWithTypeDef) {
      Members.push_back(Info);
    }

    void removeUnfavored(const NominalTypeDecl *Target) {
      Members.erase(std::remove_if(Members.begin(), Members.end(),
        [&](SynthesizedExtensionInfo *Info){
          return !isExtensionFavored(Target, Info->Ext);}), Members.end());
    }

    void sortMembers() {
      std::sort(Members.begin(), Members.end(),
                [](SynthesizedExtensionInfo *LHS, SynthesizedExtensionInfo *RHS) {
                  return (*LHS) < (*RHS);
                });
    }

    bool operator< (const ExtensionMergeGroup& Rhs) const {
      if (RequirementsCount == Rhs.RequirementsCount)
        return InheritanceCount < Rhs.InheritanceCount;
      return RequirementsCount < Rhs.RequirementsCount;
    }
  };

  using MergeGroupVector = std::vector<ExtensionMergeGroup>;

  NominalTypeDecl *Target;
  Type BaseType;
  DeclContext *DC;
  bool IncludeUnconditional;
  PrintOptions Options;
  MergeGroupVector AllGroups;
  ExtensionInfoMap InfoMap;

  Implementation(NominalTypeDecl *Target,
                 bool IncludeUnconditional,
                 PrintOptions Options):
  Target(Target),
  BaseType(Target->getDeclaredInterfaceType()),
  DC(Target),
  IncludeUnconditional(IncludeUnconditional),
  Options(Options), AllGroups(MergeGroupVector()),
  InfoMap(collectSynthesizedExtensionInfo(AllGroups)) {}

  unsigned countInherits(ExtensionDecl *ED) {
    SmallVector<InheritedEntry, 4> Results;
    getInheritedForPrinting(ED, Options, Results);
    return Results.size();
  }

  std::pair<SynthesizedExtensionInfo, ExtensionMergeInfo>
  isApplicable(ExtensionDecl *Ext, bool IsSynthesized,
               ExtensionDecl *EnablingExt, NormalProtocolConformance *Conf) {
    SynthesizedExtensionInfo Result(IsSynthesized, EnablingExt);
    ExtensionMergeInfo MergeInfo;
    MergeInfo.Unmergable =
        !Ext->getRawComment().isEmpty() ||             // With comments
        Ext->getAttrs().hasAttribute<AvailableAttr>(); // With @available
    MergeInfo.InheritsCount = countInherits(Ext);

    // There's (up to) two extensions here: the extension with the items that we
    // might be merging, plus the "enabling extension", which is the route
    // through which \c Ext itself applies, e.g. extension SomeProtocol {}
    // extension SomeType: SomeProtocol where T: SomeProtocol {}. The former is
    // Ext and the latter is EnablingExt/Conf. Either of these can be
    // conditional in ways that need to be considered when merging.
    auto conformanceIsConditional =
        Conf && !Conf->getConditionalRequirements().empty();
    if (!Ext->isConstrainedExtension() && !conformanceIsConditional) {
      if (IncludeUnconditional)
        Result.Ext = Ext;
      return {Result, MergeInfo};
    }

    auto handleRequirements = [&](SubstitutionMap subMap,
                                  ExtensionDecl *OwningExt,
                                  ArrayRef<Requirement> Reqs) {
      ProtocolDecl *BaseProto = OwningExt->getInnermostDeclContext()
        ->getSelfProtocolDecl();
      for (auto Req : Reqs) {
        // Skip protocol's Self : <Protocol> requirement.
        if (BaseProto &&
            Req.getKind() == RequirementKind::Conformance &&
            Req.getFirstType()->isEqual(BaseProto->getSelfInterfaceType()) &&
            Req.getProtocolDecl() == BaseProto)
          continue;

        if (!BaseType->isExistentialType()) {
          // Apply any substitutions we need to map the requirements from a
          // a protocol extension to an extension on the conforming type.
          Req = Req.subst(subMap);
          if (Req.hasError()) {
            // Substitution with interface type bases can only fail
            // if a concrete type fails to conform to a protocol.
            // In this case, just give up on the extension altogether.
            return true;
          }
        }

        assert(!Req.getFirstType()->hasArchetype());
        if (Req.getKind() != RequirementKind::Layout)
          assert(!Req.getSecondType()->hasArchetype());

        auto *env = Target->getGenericEnvironment();
        SmallVector<Requirement, 2> subReqs;
        subReqs.push_back(
          Req.subst(
            QueryInterfaceTypeSubstitutions(env),
            LookUpConformanceInModule(),
            SubstFlags::PreservePackExpansionLevel));

        while (!subReqs.empty()) {
          auto req = subReqs.pop_back_val();
          switch (req.checkRequirement(subReqs)) {
          case CheckRequirementResult::Success:
          case CheckRequirementResult::PackRequirement:
          case CheckRequirementResult::ConditionalConformance:
            break;

          case CheckRequirementResult::SubstitutionFailure:
            return true;

          case CheckRequirementResult::RequirementFailure:
            if (!req.canBeSatisfied())
              return true;

            MergeInfo.addRequirement(Req);
            break;
          }
        }
      }
      return false;
    };

    if (Ext->isConstrainedExtension()) {
      // Get the substitutions from the generic signature of
      // the extension to the interface types of the base type's
      // declaration.
      SubstitutionMap subMap;
      if (!BaseType->isExistentialType()) {
        if (auto *NTD = Ext->getExtendedNominal())
          subMap = BaseType->getContextSubstitutionMap(NTD);
      }

      assert(Ext->getGenericSignature() && "No generic signature.");
      auto GenericSig = Ext->getGenericSignature();
      if (handleRequirements(subMap, Ext, GenericSig.getRequirements()))
        return {Result, MergeInfo};
    }

    if (Conf) {
      SubstitutionMap subMap;
      if (!BaseType->isExistentialType()) {
        if (auto *NTD = EnablingExt->getExtendedNominal())
          subMap = BaseType->getContextSubstitutionMap(NTD);
      }
      if (handleRequirements(subMap,
                             EnablingExt,
                             Conf->getConditionalRequirements()))
        return {Result, MergeInfo};
    }

    Result.Ext = Ext;
    return {Result, MergeInfo};
  }

  void populateMergeGroup(ExtensionInfoMap &InfoMap,
                          ExtensionMergeInfoMap &MergeInfoMap,
                          MergeGroupVector &Results,
                          bool AllowMergeWithDefBody) {
    for (auto &Pair : InfoMap) {
      ExtensionDecl *ED = Pair.first;
      ExtensionMergeInfo &MergeInfo = MergeInfoMap[ED];
      SynthesizedExtensionInfo &ExtInfo = InfoMap[ED];
      auto Found = std::find_if(Results.begin(), Results.end(),
        [&](ExtensionMergeGroup &Group) {
          return MergeInfo == MergeInfoMap[Group.Members.front()->Ext];
        });
      if (Found == Results.end()) {
        Results.push_back({&ExtInfo,
          (unsigned)MergeInfo.Requirements.size(),
          MergeInfo.InheritsCount,
          AllowMergeWithDefBody && MergeInfo.isMergeableWithTypeDef()});
      } else {
        Found->Members.push_back(&ExtInfo);
      }
    }
  }

  ExtensionInfoMap
  collectSynthesizedExtensionInfoForProtocol(MergeGroupVector &AllGroups) {
    ExtensionInfoMap InfoMap;
    ExtensionMergeInfoMap MergeInfoMap;
    for (auto *E : Target->getExtensions()) {
      if (!Options.shouldPrint(E))
        continue;
      auto Pair = isApplicable(E, /*Synthesized*/ false,
                               /*EnablingExt*/ nullptr,
                               /*Conf*/ nullptr);
      if (Pair.first) {
        InfoMap.insert({E, Pair.first});
        MergeInfoMap.insert({E, Pair.second});
      }
    }
    populateMergeGroup(InfoMap, MergeInfoMap, AllGroups,
                       /*AllowMergeWithDefBody=*/false);
    std::sort(AllGroups.begin(), AllGroups.end());
    for (auto &Group : AllGroups) {
      Group.sortMembers();
    }
    return InfoMap;
  }

  static bool isEnumRawType(const Decl* D, TypeLoc TL) {
    assert (TL.getType());
    if (auto ED = dyn_cast<EnumDecl>(D)) {
      return ED->hasRawType() && ED->getRawType()->isEqual(TL.getType());
    }
    return false;
  }

  ExtensionInfoMap
  collectSynthesizedExtensionInfo(MergeGroupVector &AllGroups) {
    if (isa<ProtocolDecl>(Target)) {
      return collectSynthesizedExtensionInfoForProtocol(AllGroups);
    }

    ExtensionInfoMap InfoMap;
    ExtensionMergeInfoMap MergeInfoMap;
    std::vector<NominalTypeDecl*> Unhandled;

    auto handleExtension = [&](ExtensionDecl *E, bool Synthesized,
                               ExtensionDecl *EnablingE,
                               NormalProtocolConformance *Conf) {
      PrintOptions AdjustedOpts = Options;
      if (Synthesized) {
        // Members from underscored system protocols should still appear as
        // members of the target type, even if the protocols themselves are not
        // printed.
        AdjustedOpts.SkipUnderscoredSystemProtocols = false;
      }
      if (AdjustedOpts.shouldPrint(E)) {
        auto Pair = isApplicable(E, Synthesized, EnablingE, Conf);
        if (Pair.first) {
          InfoMap.insert({E, Pair.first});
          MergeInfoMap.insert({E, Pair.second});
        }
      }
    };

    // We want to visit the protocols of any normal conformances we see, but
    // we have to avoid doing this to self-conformances or we can end up with
    // a cycle.  Otherwise this is cycle-proof on valid code.
    // We also want to ignore inherited conformances. Members from these will
    // be included in the class they were inherited from.
    auto addConformance = [&](ProtocolConformance *Conf) {
      if (isa<InheritedProtocolConformance>(Conf))
        return;
      auto RootConf = Conf->getRootConformance();
      if (isa<NormalProtocolConformance>(RootConf))
        Unhandled.push_back(RootConf->getProtocol());
    };

    for (auto *Conf : Target->getLocalConformances()) {
      addConformance(Conf);
    }
    while (!Unhandled.empty()) {
      NominalTypeDecl* Back = Unhandled.back();
      Unhandled.pop_back();
      for (ExtensionDecl *E : Back->getExtensions()) {
        handleExtension(E, true, nullptr, nullptr);
      }
      for (auto *Conf : Back->getLocalConformances()) {
        addConformance(Conf);
      }
    }

    // Merge with actual extensions.
    for (auto *EnablingE : Target->getExtensions()) {
      handleExtension(EnablingE, false, nullptr, nullptr);
      for (auto *Conf : EnablingE->getLocalConformances()) {
        auto NormalConf =
          dyn_cast<NormalProtocolConformance>(Conf->getRootConformance());
        if (!NormalConf) continue;
        for (auto E : NormalConf->getProtocol()->getExtensions())
          handleExtension(E, true, EnablingE, NormalConf);
      }
    }

    populateMergeGroup(InfoMap, MergeInfoMap, AllGroups,
                       /*AllowMergeWithDefBody=*/true);

    std::sort(AllGroups.begin(), AllGroups.end());
    for (auto &Group : AllGroups) {
      Group.removeUnfavored(Target);
      Group.sortMembers();
    }
    AllGroups.erase(std::remove_if(AllGroups.begin(), AllGroups.end(),
      [](ExtensionMergeGroup &Group) { return Group.Members.empty(); }),
                    AllGroups.end());

    return InfoMap;
  }
};

SynthesizedExtensionAnalyzer::SynthesizedExtensionAnalyzer(
    NominalTypeDecl *Target, PrintOptions Options, bool IncludeUnconditional)
    : Impl(*(new Implementation(Target, IncludeUnconditional, Options))) {}

SynthesizedExtensionAnalyzer::~SynthesizedExtensionAnalyzer() {delete &Impl;}

bool SynthesizedExtensionAnalyzer::isInSynthesizedExtension(
    const ValueDecl *VD) {
  if (auto Ext = dyn_cast_or_null<ExtensionDecl>(VD->getDeclContext()->
                                                 getInnermostTypeContext())) {
    auto It = Impl.InfoMap.find(Ext);
    if (It != Impl.InfoMap.end() && It->second.IsSynthesized) {
      // A synthesized extension will only be created if the underlying type
      // is in the same module
      return VD->getModuleContext() == Impl.Target->getModuleContext();
    }
  }
  return false;
}

void SynthesizedExtensionAnalyzer::
forEachExtensionMergeGroup(MergeGroupKind Kind, ExtensionGroupOperation Fn) {
  for (auto &Group : Impl.AllGroups) {
    if (Kind != MergeGroupKind::All) {
      if (Kind != Group.Kind)
        continue;
    }
    std::vector<ExtensionInfo> GroupContent;
    for (auto &Member : Group.Members) {
      GroupContent.push_back(
          {Member->Ext, Member->EnablingExt, Member->IsSynthesized});
    }
    Fn(llvm::ArrayRef(GroupContent));
  }
}

bool SynthesizedExtensionAnalyzer::hasMergeGroup(MergeGroupKind Kind) {
  for (auto &Group : Impl.AllGroups) {
    if (Kind == MergeGroupKind::All)
      return true;
    if (Kind == Group.Kind)
      return true;
  }
  return false;
}

void swift::
collectDefaultImplementationForProtocolMembers(ProtocolDecl *PD,
                    llvm::SmallDenseMap<ValueDecl*, ValueDecl*> &DefaultMap) {
  auto HandleMembers = [&](DeclRange Members) {
    for (Decl *D : Members) {
      auto *VD = dyn_cast<ValueDecl>(D);

      // Skip non-value decl.
      if (!VD)
        continue;

      // Skip decls with empty names, e.g. setter/getters for properties.
      if (VD->getBaseName().empty())
        continue;

      for (auto *Default: PD->lookupDirect(VD->getName())) {
        if (Default->getDeclContext()->getExtendedProtocolDecl() == PD) {
          DefaultMap.insert({Default, VD});
        }
      }
    }
  };

  // Collect the default implementations for the members in this given protocol.
  HandleMembers(PD->getMembers());

  // Collect the default implementations for the members in the inherited
  // protocols.
  for (auto *IP : PD->getInheritedProtocols())
    HandleMembers(IP->getMembers());
}

/// This walker will traverse the AST and report types for every expression.
class ExpressionTypeCollector: public SourceEntityWalker {
  SourceManager &SM;
  unsigned int BufferId;
  std::vector<ExpressionTypeInfo> &Results;

  // This is to where we print all types.
  llvm::raw_ostream &OS;

  // Map from a printed type to the offset in OS where the type starts.
  llvm::StringMap<uint32_t> TypeOffsets;

  // This keeps track of whether we have a type reported for a given
  // [offset, length].
  llvm::DenseMap<unsigned, llvm::DenseSet<unsigned>> AllPrintedTypes;

  // When non empty, we only print expression types that conform to any of
  // these protocols.
  llvm::MapVector<ProtocolDecl*, StringRef> &InterestedProtocols;

  // Specified by the client whether we should print fully qualified types
  const bool FullyQualified;

  // Specified by the client whether we should canonicalize types before printing
  const bool CanonicalType;

  bool shouldReport(unsigned Offset, unsigned Length, Expr *E,
                    std::vector<StringRef> &Conformances) {
    assert(Conformances.empty());
    // We shouldn't report null types.
    if (E->getType().isNull())
      return false;

    // We should not report a type for implicit expressions, except for
    // - `OptionalEvaluationExpr` to show the correct type when there is optional chaining
    // - `DotSyntaxCallExpr` to report the method type without the metatype
    if (E->isImplicit() &&
        !isa<OptionalEvaluationExpr>(E) &&
        !isa<DotSyntaxCallExpr>(E)) {
      return false;
    }

    // If we have already reported types for this source range, we shouldn't
    // report again. This makes sure we always report the outtermost type of
    // several overlapping expressions.
    auto &Bucket = AllPrintedTypes[Offset];
    if (Bucket.find(Length) != Bucket.end())
      return false;

    // We print every expression if the interested protocols are empty.
    if (InterestedProtocols.empty())
      return true;

    // Collecting protocols conformed by this expressions that are in the list.
    for (auto Proto: InterestedProtocols) {
      if (checkConformance(E->getType(), Proto.first)) {
        Conformances.push_back(Proto.second);
      }
    }

    // We only print the type of the expression if it conforms to any of the
    // interested protocols.
    return !Conformances.empty();
  }

  // Find an existing offset in the type buffer otherwise print the type to
  // the buffer.
  std::pair<uint32_t, uint32_t> getTypeOffsets(StringRef PrintedType) {
    auto It = TypeOffsets.find(PrintedType);
    if (It == TypeOffsets.end()) {
      TypeOffsets[PrintedType] = OS.tell();
      OS << PrintedType << '\0';
    }
    return {TypeOffsets[PrintedType], PrintedType.size()};
  }


public:
  ExpressionTypeCollector(
      SourceFile &SF,
      llvm::MapVector<ProtocolDecl *, StringRef> &InterestedProtocols,
      std::vector<ExpressionTypeInfo> &Results, bool FullyQualified,
      bool CanonicalType, llvm::raw_ostream &OS)
      : SM(SF.getASTContext().SourceMgr),
        BufferId(SF.getBufferID()), Results(Results), OS(OS),
        InterestedProtocols(InterestedProtocols),
        FullyQualified(FullyQualified), CanonicalType(CanonicalType) {}
  bool walkToExprPre(Expr *E) override {
    if (E->getSourceRange().isInvalid())
      return true;
    CharSourceRange Range =
      Lexer::getCharSourceRangeFromSourceRange(SM, E->getSourceRange());
    unsigned Offset = SM.getLocOffsetInBuffer(Range.getStart(), BufferId);
    unsigned Length = Range.getByteLength();
    std::vector<StringRef> Conformances;
    if (!shouldReport(Offset, Length, E, Conformances))
      return true;
    // Print the type to a temporary buffer.
    SmallString<64> Buffer;
    {
      llvm::raw_svector_ostream OS(Buffer);
      auto Ty = E->getType()->getRValueType();
      PrintOptions printOptions = PrintOptions();
      printOptions.FullyQualifiedTypes = FullyQualified;
      if (CanonicalType) {
        Ty->getCanonicalType()->print(OS, printOptions);
      } else {
        Ty->reconstituteSugar(true)->print(OS, printOptions);
      }
    }
    auto Ty = getTypeOffsets(Buffer.str());
    // Add the type information to the result list.
    Results.push_back({Offset, Length, Ty.first, Ty.second, {}});

    // Adding all protocol names to the result.
    for(auto Con: Conformances) {
      auto Ty = getTypeOffsets(Con);
      Results.back().protocols.push_back({Ty.first, Ty.second});
    }

    // Keep track of that we have a type reported for this range.
    AllPrintedTypes[Offset].insert(Length);
    return true;
  }
};

ProtocolDecl* swift::resolveProtocolName(DeclContext *dc, StringRef name) {
  return evaluateOrDefault(dc->getASTContext().evaluator,
                           ResolveProtocolNameRequest(ProtocolNameOwner(dc, name)),
                           nullptr);
}

ArrayRef<ExpressionTypeInfo> swift::collectExpressionType(
    SourceFile &SF, ArrayRef<const char *> ExpectedProtocols,
    std::vector<ExpressionTypeInfo> &Scratch, bool FullyQualified,
    bool CanonicalType, llvm::raw_ostream &OS) {
  llvm::MapVector<ProtocolDecl*, StringRef> InterestedProtocols;
  for (auto Name: ExpectedProtocols) {
    if (auto *pd = resolveProtocolName(&SF, Name)) {
      InterestedProtocols.insert({pd, Name});
    } else {
      return {};
    }
  }
  ExpressionTypeCollector Walker(SF, InterestedProtocols, Scratch,
                                 FullyQualified, CanonicalType, OS);
  Walker.walk(SF);
  return Scratch;
}

/// This walker will traverse the AST and report types for every variable
/// declaration.
class VariableTypeCollector : public SourceEntityWalker {
private:
  const SourceManager &SM;
  unsigned int BufferId;

  /// The range in which variable types are to be collected.
  SourceRange TotalRange;

  // Specified by the client whether we should print fully qualified types
  const bool FullyQualified;

  /// The output vector for VariableTypeInfos emitted during traversal.
  std::vector<VariableTypeInfo> &Results;

  /// We print all types into a single output stream (e.g. into a string buffer)
  /// and provide offsets into this string buffer to describe individual types,
  /// i.e. \c OS builds a string that contains all null-terminated printed type
  /// strings. When referring to one of these types, we can use the offsets at
  /// which it starts in the \c OS.
  llvm::raw_ostream &OS;

  /// Map from a printed type to the offset in \c OS where the type starts.
  llvm::StringMap<uint32_t> TypeOffsets;

  /// Returns the start offset of this string in \c OS. If \c PrintedType
  /// hasn't been printed to \c OS yet, this function will do so.
  uint32_t getTypeOffset(StringRef PrintedType) {
    auto It = TypeOffsets.find(PrintedType);
    if (It == TypeOffsets.end()) {
      TypeOffsets[PrintedType] = OS.tell();
      OS << PrintedType << '\0';
    }
    return TypeOffsets[PrintedType];
  }

  /// Checks whether the given range overlaps the total range in which we
  /// collect variable types.
  bool overlapsTotalRange(SourceRange Range) {
    return TotalRange.isInvalid() || Range.overlaps(TotalRange);
  }

public:
  VariableTypeCollector(const SourceFile &SF, SourceRange Range,
                        bool FullyQualified,
                        std::vector<VariableTypeInfo> &Results,
                        llvm::raw_ostream &OS)
      : SM(SF.getASTContext().SourceMgr), BufferId(SF.getBufferID()),
        TotalRange(Range), FullyQualified(FullyQualified), Results(Results),
        OS(OS) {}

  bool walkToDeclPre(Decl *D, CharSourceRange DeclNameRange) override {
    if (DeclNameRange.isInvalid()) {
      return true;
    }
    // Skip this declaration and its subtree if outside the range
    if (!overlapsTotalRange(D->getSourceRange())) {
      return false;
    }
    if (auto VD = dyn_cast<VarDecl>(D)) {
      unsigned VarOffset =
          SM.getLocOffsetInBuffer(DeclNameRange.getStart(), BufferId);
      unsigned VarLength = DeclNameRange.getByteLength();
      // Print the type to a temporary buffer
      SmallString<64> Buffer;
      {
        llvm::raw_svector_ostream OS(Buffer);
        PrintOptions Options;
        Options.SynthesizeSugarOnTypes = true;
        Options.FullyQualifiedTypes = FullyQualified;
        auto Ty = VD->getInterfaceType();
        // Skip this declaration and its children if the type is an error type.
        if (Ty->is<ErrorType>()) {
          return false;
        }
        Ty->print(OS, Options);
      }
      // Transfer the type to `OS` if needed and get the offset of this string
      // in `OS`.
      auto TyOffset = getTypeOffset(Buffer.str());
      bool HasExplicitType =
          VD->getTypeReprOrParentPatternTypeRepr() != nullptr;
      // Add the type information to the result list.
      Results.emplace_back(VarOffset, VarLength, HasExplicitType, TyOffset);
    }
    return true;
  }

  bool walkToStmtPre(Stmt *S) override {
    // Skip this statement and its subtree if outside the range
    return overlapsTotalRange(S->getSourceRange());
  }

  bool walkToExprPre(Expr *E) override {
    // Skip this expression and its subtree if outside the range
    return overlapsTotalRange(E->getSourceRange());
  }

  bool walkToPatternPre(Pattern *P) override {
    // Skip this pattern and its subtree if outside the range
    return overlapsTotalRange(P->getSourceRange());
  }
};

VariableTypeInfo::VariableTypeInfo(uint32_t Offset, uint32_t Length,
                                   bool HasExplicitType, uint32_t TypeOffset)
    : Offset(Offset), Length(Length), HasExplicitType(HasExplicitType),
      TypeOffset(TypeOffset) {}

void swift::collectVariableType(
    SourceFile &SF, SourceRange Range, bool FullyQualified,
    std::vector<VariableTypeInfo> &VariableTypeInfos, llvm::raw_ostream &OS) {
  VariableTypeCollector Walker(SF, Range, FullyQualified, VariableTypeInfos,
                               OS);
  Walker.walk(SF);
}

ArrayRef<ValueDecl*> swift::
canDeclProvideDefaultImplementationFor(ValueDecl* VD) {
  return evaluateOrDefault(VD->getASTContext().evaluator,
                           ProvideDefaultImplForRequest(VD),
                           ArrayRef<ValueDecl*>());
}

ArrayRef<ValueDecl*> swift::
collectAllOverriddenDecls(ValueDecl *VD, bool IncludeProtocolRequirements,
                          bool Transitive) {
  return evaluateOrDefault(VD->getASTContext().evaluator,
    CollectOverriddenDeclsRequest(OverridenDeclsOwner(VD,
      IncludeProtocolRequirements, Transitive)), ArrayRef<ValueDecl*>());
}

bool swift::isExtensionApplied(const DeclContext *DC, Type BaseTy,
                               const ExtensionDecl *ED) {
  return evaluateOrDefault(DC->getASTContext().evaluator,
    IsDeclApplicableRequest(DeclApplicabilityOwner(DC, BaseTy, ED)), false);
}

bool swift::isMemberDeclApplied(const DeclContext *DC, Type BaseTy,
                                const ValueDecl *VD) {
  return evaluateOrDefault(DC->getASTContext().evaluator,
    IsDeclApplicableRequest(DeclApplicabilityOwner(DC, BaseTy, VD)), false);
}

Type swift::tryMergeBaseTypeForCompletionLookup(Type ty1, Type ty2,
                                                DeclContext *dc) {
  // Easy case, equivalent so just pick one.
  if (ty1->isEqual(ty2))
    return ty1;

  // Check to see if one is an optional of another. In that case, prefer the
  // optional since we can unwrap a single level when doing a lookup.
  {
    SmallVector<Type, 4> ty1Optionals;
    SmallVector<Type, 4> ty2Optionals;
    auto ty1Unwrapped = ty1->lookThroughAllOptionalTypes(ty1Optionals);
    auto ty2Unwrapped = ty2->lookThroughAllOptionalTypes(ty2Optionals);

    if (ty1Unwrapped->isEqual(ty2Unwrapped)) {
      // We currently only unwrap a single level of optional, so if the
      // difference is greater, don't merge.
      if (ty1Optionals.size() == 1 && ty2Optionals.empty())
        return ty1;
      if (ty2Optionals.size() == 1 && ty1Optionals.empty())
        return ty2;
    }
    // We don't want to consider subtyping for optional mismatches since
    // optional promotion is modelled as a subtype, which isn't useful for us
    // (i.e if we have T? and U, preferring U would miss members on T?).
    if (ty1Optionals.size() != ty2Optionals.size())
      return Type();
  }

  // In general we want to prefer a subtype over a supertype.
  if (isSubtypeOf(ty1, ty2, dc))
    return ty1;
  if (isSubtypeOf(ty2, ty1, dc))
    return ty2;

  // Incomparable, return null.
  return Type();
}

bool swift::isConvertibleTo(Type T1, Type T2, bool openArchetypes,
                            DeclContext &DC) {
  return evaluateOrDefault(DC.getASTContext().evaluator,
    TypeRelationCheckRequest(TypeRelationCheckInput(&DC, T1, T2,
      TypeRelation::ConvertTo, openArchetypes)), false);
}

bool swift::isSubtypeOf(Type T1, Type T2, DeclContext *DC) {
  return evaluateOrDefault(DC->getASTContext().evaluator,
    TypeRelationCheckRequest(TypeRelationCheckInput(DC, T1, T2,
      TypeRelation::SubtypeOf, /*openArchetypes*/ false)), false);
}

Type swift::getRootTypeOfKeypathDynamicMember(SubscriptDecl *SD) {
  return evaluateOrDefault(SD->getASTContext().evaluator,
    RootTypeOfKeypathDynamicMemberRequest{SD}, Type());
}

Type swift::getResultTypeOfKeypathDynamicMember(SubscriptDecl *SD) {
  return evaluateOrDefault(SD->getASTContext().evaluator,
    RootAndResultTypeOfKeypathDynamicMemberRequest{SD}, TypePair()).
      SecondTy;
}

SmallVector<std::pair<ValueDecl *, ValueDecl *>, 1>
swift::getShorthandShadows(CaptureListExpr *CaptureList, DeclContext *DC) {
  SmallVector<std::pair<ValueDecl *, ValueDecl *>, 1> Result;
  for (auto Capture : CaptureList->getCaptureList()) {
    if (Capture.PBD->getPatternList().size() != 1)
      continue;

    Expr *Init = Capture.PBD->getInit(0);
    if (!Init)
      continue;

    auto DeclaredVar = Capture.getVar();
    if (DeclaredVar->getLoc() != Init->getLoc()) {
      // We have a capture like `[foo]` if the declared var and the
      // reference share the same location.
      continue;
    }

    if (auto UDRE = dyn_cast<UnresolvedDeclRefExpr>(Init)) {
      if (DC)
        Init = resolveDeclRefExpr(UDRE, DC);
    }

    auto *ReferencedVar = Init->getReferencedDecl().getDecl();
    if (!ReferencedVar)
      continue;

    Result.emplace_back(std::make_pair(DeclaredVar, ReferencedVar));
  }
  return Result;
}

SmallVector<std::pair<ValueDecl *, ValueDecl *>, 1>
swift::getShorthandShadows(LabeledConditionalStmt *CondStmt, DeclContext *DC) {
  SmallVector<std::pair<ValueDecl *, ValueDecl *>, 1> Result;
  for (const StmtConditionElement &Cond : CondStmt->getCond()) {
    if (Cond.getKind() != StmtConditionElement::CK_PatternBinding)
      continue;

    Expr *Init = Cond.getInitializer();
    if (auto UDRE = dyn_cast<UnresolvedDeclRefExpr>(Init)) {
      if (DC)
        Init = resolveDeclRefExpr(UDRE, DC);
    }

    auto ReferencedVar = Init->getReferencedDecl().getDecl();
    if (!ReferencedVar)
      continue;

    Cond.getPattern()->forEachVariable([&](VarDecl *DeclaredVar) {
      if (DeclaredVar->getLoc() != Init->getLoc())
        return;
      Result.emplace_back(std::make_pair(DeclaredVar, ReferencedVar));
    });
  }
  return Result;
}
