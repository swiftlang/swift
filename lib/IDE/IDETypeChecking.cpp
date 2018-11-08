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

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Types.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;

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

PrintOptions PrintOptions::printModuleInterface() {
  PrintOptions result = printInterface();
  result.CurrentPrintabilityChecker.reset(new ModulePrinterPrintableChecker());
  return result;
}

PrintOptions PrintOptions::printTypeInterface(Type T) {
  PrintOptions result = printModuleInterface();
  result.PrintExtensionFromConformingProtocols = true;
  result.TransformContext = TypeTransformContext(T);
  result.printExtensionContentAsMembers = [T](const ExtensionDecl *ED) {
    return isExtensionApplied(*T->getNominalOrBoundGenericNominal()->
                              getDeclContext(), T, ED);
  };
  result.CurrentPrintabilityChecker.reset(new ModulePrinterPrintableChecker());
  return result;
}

PrintOptions PrintOptions::printDocInterface() {
  PrintOptions result = PrintOptions::printModuleInterface();
  result.PrintAccess = false;
  result.SkipUnavailable = false;
  result.ExcludeAttrList.push_back(DAK_Available);
  result.ArgAndParamPrinting =
      PrintOptions::ArgAndParamPrintingMode::BothAlways;
  result.PrintDocumentationComments = false;
  result.PrintRegularClangComments = false;
  result.PrintFunctionRepresentationAttrs = false;
  return result;
}

/// Erase any associated types within dependent member types, so we'll resolve
/// them again.
static Type eraseAssociatedTypes(Type type) {
  if (!type->hasTypeParameter()) return type;

  return type.transformRec([](TypeBase *type) -> Optional<Type> {
    if (auto depMemType = dyn_cast<DependentMemberType>(type)) {
      auto newBase = eraseAssociatedTypes(depMemType->getBase());
      if (newBase.getPointer() == depMemType->getBase().getPointer() &&
          !depMemType->getAssocType())
        return None;

      return Type(DependentMemberType::get(newBase, depMemType->getName()));
    }

    return None;
  });
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
          int Result = LFile.getValue().compare(RFile.getValue());
          if (Result != 0)
            return Result < 0;
        }
      }

      // Otherwise, sort by source order.
      if (auto LeftOrder = Ext->getSourceOrder()) {
        if (auto RightOrder = Rhs.Ext->getSourceOrder()) {
          return LeftOrder.getValue() < RightOrder.getValue();
        }
      }

      return false;
    }
  };

  struct ExtensionMergeInfo {
    struct Requirement {
      Type First;
      Type Second;
      RequirementKind Kind;
      CanType CanFirst;
      CanType CanSecond;

      bool operator< (const Requirement& Rhs) const {
        if (Kind != Rhs.Kind)
          return Kind < Rhs.Kind;
        else if (CanFirst != Rhs.CanFirst)
          return CanFirst < Rhs.CanFirst;
        else
          return CanSecond < Rhs.CanSecond;
      }
      bool operator== (const Requirement& Rhs) const {
        return (!(*this < Rhs)) && (!(Rhs < *this));
      }
    };

    bool HasDocComment;
    unsigned InheritsCount;
    std::set<Requirement> Requirements;
    void addRequirement(GenericSignature *GenericSig,
                        Type First, Type Second, RequirementKind Kind) {
      CanType CanFirst =
        GenericSig->getCanonicalTypeInContext(eraseAssociatedTypes(First));
      CanType CanSecond;
      if (Second) CanSecond =
        GenericSig->getCanonicalTypeInContext(eraseAssociatedTypes(Second));

      Requirements.insert({First, Second, Kind, CanFirst, CanSecond});
    }
    bool operator== (const ExtensionMergeInfo& Another) const {
      // Trivially unmergeable.
      if (HasDocComment || Another.HasDocComment)
        return false;
      if (InheritsCount != 0 || Another.InheritsCount != 0)
        return false;
      return Requirements == Another.Requirements;
    }
    bool isMergeableWithTypeDef() {
      return !HasDocComment && InheritsCount == 0 && Requirements.empty();
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
  std::unique_ptr<ExtensionInfoMap> InfoMap;

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
    SmallVector<TypeLoc, 4> Results;
    getInheritedForPrinting(
        ED, [&](const Decl *D) { return Options.shouldPrint(D); }, Results);
    return Results.size();
  }

  std::pair<SynthesizedExtensionInfo, ExtensionMergeInfo>
  isApplicable(ExtensionDecl *Ext, bool IsSynthesized,
               ExtensionDecl *EnablingExt, NormalProtocolConformance *Conf) {
    SynthesizedExtensionInfo Result(IsSynthesized, EnablingExt);
    ExtensionMergeInfo MergeInfo;
    MergeInfo.HasDocComment = !Ext->getRawComment().isEmpty();
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
                                  GenericSignature *GenericSig,
                                  ArrayRef<Requirement> Reqs) {
      for (auto Req : Reqs) {
        auto Kind = Req.getKind();

        // FIXME: Could do something here
        if (Kind == RequirementKind::Layout)
          continue;

        auto First = Req.getFirstType();
        auto Second = Req.getSecondType();
        if (!BaseType->isExistentialType()) {
          First = First.subst(subMap);
          Second = Second.subst(subMap);

          if (!First || !Second) {
            // Substitution with interface type bases can only fail
            // if a concrete type fails to conform to a protocol.
            // In this case, just give up on the extension altogether.
            return true;
          }
        }

        switch (Kind) {
        case RequirementKind::Conformance:
        case RequirementKind::Superclass:
          // FIXME: This could be more accurate; check
          // conformance instead of subtyping
          if (!canPossiblyConvertTo(First, Second, *DC))
            return true;
          else if (!isConvertibleTo(First, Second, *DC))
            MergeInfo.addRequirement(GenericSig, First, Second, Kind);
          break;

        case RequirementKind::SameType:
          if (!canPossiblyEqual(First, Second, *DC)) {
            return true;
          } else if (!First->isEqual(Second)) {
            MergeInfo.addRequirement(GenericSig, First, Second, Kind);
          }
          break;

        case RequirementKind::Layout:
          llvm_unreachable("Handled above");
        }
      }
      return false;
    };

    auto *M = DC->getParentModule();
    if (Ext->isConstrainedExtension()) {
      // Get the substitutions from the generic signature of
      // the extension to the interface types of the base type's
      // declaration.
      SubstitutionMap subMap;
      if (!BaseType->isExistentialType())
        subMap = BaseType->getContextSubstitutionMap(M, Ext);

      assert(Ext->getGenericSignature() && "No generic signature.");
      auto GenericSig = Ext->getGenericSignature();
      if (handleRequirements(subMap, GenericSig, GenericSig->getRequirements()))
        return {Result, MergeInfo};
    }

    if (Conf && handleRequirements(Conf->getSubstitutions(M),
                                   Conf->getGenericSignature(),
                                   Conf->getConditionalRequirements()))
      return {Result, MergeInfo};

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

  std::unique_ptr<ExtensionInfoMap>
  collectSynthesizedExtensionInfoForProtocol(MergeGroupVector &AllGroups) {
    std::unique_ptr<ExtensionInfoMap> InfoMap(new ExtensionInfoMap());
    ExtensionMergeInfoMap MergeInfoMap;
    for (auto *E : Target->getExtensions()) {
      if (!Options.shouldPrint(E))
        continue;
      auto Pair = isApplicable(E, /*Synthesized*/ false,
                               /*EnablingExt*/ nullptr,
                               /*Conf*/ nullptr);
      if (Pair.first) {
        InfoMap->insert({E, Pair.first});
        MergeInfoMap.insert({E, Pair.second});
      }
    }
    populateMergeGroup(*InfoMap, MergeInfoMap, AllGroups,
                       /*AllowMergeWithDefBody*/false);
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

  std::unique_ptr<ExtensionInfoMap>
  collectSynthesizedExtensionInfo(MergeGroupVector &AllGroups) {
    if (isa<ProtocolDecl>(Target)) {
      return collectSynthesizedExtensionInfoForProtocol(AllGroups);
    }
    std::unique_ptr<ExtensionInfoMap> InfoMap(new ExtensionInfoMap());
    ExtensionMergeInfoMap MergeInfoMap;
    std::vector<NominalTypeDecl*> Unhandled;

    auto handleExtension = [&](ExtensionDecl *E, bool Synthesized,
                               ExtensionDecl *EnablingE,
                               NormalProtocolConformance *Conf) {
      if (Options.shouldPrint(E)) {
        auto Pair = isApplicable(E, Synthesized, EnablingE, Conf);
        if (Pair.first) {
          InfoMap->insert({E, Pair.first});
          MergeInfoMap.insert({E, Pair.second});
        }
      }
    };

    for (auto *Conf : Target->getLocalConformances()) {
      Unhandled.push_back(Conf->getProtocol());
    }
    if (auto *CD = dyn_cast<ClassDecl>(Target)) {
      if (auto Super = CD->getSuperclassDecl())
        Unhandled.push_back(Super);
    }
    while (!Unhandled.empty()) {
      NominalTypeDecl* Back = Unhandled.back();
      Unhandled.pop_back();
      for (ExtensionDecl *E : Back->getExtensions()) {
        handleExtension(E, true, nullptr, nullptr);
      }
      for (auto *Conf : Back->getLocalConformances()) {
        Unhandled.push_back(Conf->getProtocol());
      }
      if (auto *CD = dyn_cast<ClassDecl>(Back)) {
        if (auto Super = CD->getSuperclass())
          Unhandled.push_back(Super->getAnyNominal());
      }
    }

    // Merge with actual extensions.
    for (auto *EnablingE : Target->getExtensions()) {
      handleExtension(EnablingE, false, nullptr, nullptr);
      for (auto *Conf : EnablingE->getLocalConformances()) {
        for (auto E : Conf->getProtocol()->getExtensions())
          handleExtension(E, true, EnablingE, Conf->getRootNormalConformance());
      }
    }

    populateMergeGroup(*InfoMap, MergeInfoMap, AllGroups,
                       /*AllowMergeWithDefBody*/true);

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

SynthesizedExtensionAnalyzer::
SynthesizedExtensionAnalyzer(NominalTypeDecl *Target,
                             PrintOptions Options,
                             bool IncludeUnconditional):
Impl(*(new Implementation(Target, IncludeUnconditional, Options))) {}

SynthesizedExtensionAnalyzer::~SynthesizedExtensionAnalyzer() {delete &Impl;}

bool SynthesizedExtensionAnalyzer::
isInSynthesizedExtension(const ValueDecl *VD) {
  if (auto Ext = dyn_cast_or_null<ExtensionDecl>(VD->getDeclContext()->
                                                 getInnermostTypeContext())) {
    return Impl.InfoMap->count(Ext) != 0 &&
    Impl.InfoMap->find(Ext)->second.IsSynthesized;
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
    Fn(llvm::makeArrayRef(GroupContent));
  }
}

bool SynthesizedExtensionAnalyzer::
hasMergeGroup(MergeGroupKind Kind) {
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

      for (auto *Default: PD->lookupDirect(VD->getFullName())) {
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
