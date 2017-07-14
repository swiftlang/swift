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
#include "swift/AST/Types.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;

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
    operator bool() const { return Ext; }
    SynthesizedExtensionInfo(bool IsSynthesized = true) :
    IsSynthesized(IsSynthesized) {}
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
      bool operator< (const Requirement& Rhs) const {
        if (Kind != Rhs.Kind)
          return Kind < Rhs.Kind;
        else if (First.getPointer() != Rhs.First.getPointer())
          return First.getPointer() < Rhs.First.getPointer();
        else
          return Second.getPointer() < Rhs.Second.getPointer();
      }
      bool operator== (const Requirement& Rhs) const {
        return (!(*this < Rhs)) && (!(Rhs < *this));
      }
    };

    bool HasDocComment;
    unsigned InheritsCount;
    std::set<Requirement> Requirements;
    void addRequirement(Type First, Type Second, RequirementKind Kind) {
      Requirements.insert({First, Second, Kind});
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

  typedef llvm::MapVector<ExtensionDecl*, SynthesizedExtensionInfo> ExtensionInfoMap;
  typedef llvm::MapVector<ExtensionDecl*, ExtensionMergeInfo> ExtensionMergeInfoMap;

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

  typedef std::vector<ExtensionMergeGroup> MergeGroupVector;

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
    unsigned Count = 0;
    for (auto TL : ED->getInherited()) {
      auto *nominal = TL.getType()->getAnyNominal();
      if (nominal && shouldPrint(nominal, Options))
        Count ++;
    }
    return Count;
  }

  std::pair<SynthesizedExtensionInfo, ExtensionMergeInfo>
  isApplicable(ExtensionDecl *Ext, bool IsSynthesized) {
    SynthesizedExtensionInfo Result(IsSynthesized);
    ExtensionMergeInfo MergeInfo;
    MergeInfo.HasDocComment = !Ext->getRawComment().isEmpty();
    MergeInfo.InheritsCount = countInherits(Ext);
    if (!Ext->isConstrainedExtension()) {
      if (IncludeUnconditional)
        Result.Ext = Ext;
      return {Result, MergeInfo};
    }

    // Get the substitutions from the generic signature of
    // the extension to the interface types of the base type's
    // declaration.
    auto *M = DC->getParentModule();
    SubstitutionMap subMap;
    if (!BaseType->isExistentialType())
      subMap = BaseType->getContextSubstitutionMap(M, Ext);

    assert(Ext->getGenericSignature() && "No generic signature.");
    for (auto Req : Ext->getGenericSignature()->getRequirements()) {
      auto Kind = Req.getKind();

      auto First = Req.getFirstType();
      auto Second = Req.getSecondType();
      if (!BaseType->isExistentialType()) {
        First = First.subst(subMap);
        Second = Second.subst(subMap);

        if (!First || !Second) {
          // Substitution with interface type bases can only fail
          // if a concrete type fails to conform to a protocol.
          // In this case, just give up on the extension altogether.
          return {Result, MergeInfo};
        }
      }

      switch (Kind) {
        case RequirementKind::Conformance:
        case RequirementKind::Layout:
        case RequirementKind::Superclass:
          if (!canPossiblyConvertTo(First, Second, *DC))
            return {Result, MergeInfo};
          else if (!isConvertibleTo(First, Second, *DC))
            MergeInfo.addRequirement(First, Second, Kind);
          break;

        case RequirementKind::SameType:
          if (!canPossiblyEqual(First, Second, *DC)) {
            return {Result, MergeInfo};
          } else if (!First->isEqual(Second)) {
            MergeInfo.addRequirement(First, Second, Kind);
          }
          break;
      }
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

  std::unique_ptr<ExtensionInfoMap>
  collectSynthesizedExtensionInfoForProtocol(MergeGroupVector &AllGroups) {
    std::unique_ptr<ExtensionInfoMap> InfoMap(new ExtensionInfoMap());
    ExtensionMergeInfoMap MergeInfoMap;
    for (auto *E : Target->getExtensions()) {
      if (!shouldPrint(E, Options))
        continue;
      auto Pair = isApplicable(E, /*Synthesized*/false);
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
    auto addTypeLocNominal = [&](TypeLoc TL) {
      if (TL.getType()) {
        if (auto D = TL.getType()->getAnyNominal()) {
          Unhandled.push_back(D);
        }
      }
    };

    auto handleExtension = [&](ExtensionDecl *E, bool Synthesized) {
      if (shouldPrint(E, Options)) {
        auto Pair = isApplicable(E, Synthesized);
        if (Pair.first) {
          InfoMap->insert({E, Pair.first});
          MergeInfoMap.insert({E, Pair.second});
        }
      }
    };

    for (auto TL : Target->getInherited()) {
      if (!isEnumRawType(Target, TL))
        addTypeLocNominal(TL);
    }
    while (!Unhandled.empty()) {
      NominalTypeDecl* Back = Unhandled.back();
      Unhandled.pop_back();
      for (ExtensionDecl *E : Back->getExtensions()) {
        handleExtension(E, true);
        for (auto TL : Back->getInherited()) {
          if (!isEnumRawType(Target, TL))
            addTypeLocNominal(TL);
        }
      }
    }

    // Merge with actual extensions.
    for (auto *E : Target->getExtensions()) {
      handleExtension(E, false);
      for (auto *Conf : E->getLocalConformances()) {
        for (auto E : Conf->getProtocol()->getExtensions())
          handleExtension(E, true);
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
    std::vector<ExtensionAndIsSynthesized> GroupContent;
    for (auto &Member : Group.Members) {
      GroupContent.push_back({Member->Ext, Member->IsSynthesized});
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
  Type BaseTy = PD->getDeclaredInterfaceType();
  DeclContext *DC = PD->getInnermostDeclContext();
  auto HandleMembers = [&](DeclRange Members) {
    for (Decl *D : Members) {
      auto *VD = dyn_cast<ValueDecl>(D);

      // Skip non-value decl.
      if (!VD)
        continue;

      // Skip decls with empty names, e.g. setter/getters for properties.
      if (VD->getBaseName().empty())
        continue;

      ResolvedMemberResult Result = resolveValueMember(*DC, BaseTy,
                                                       VD->getFullName());
      assert(Result);
      for (auto *Default : Result.getMemberDecls(InterestedMemberKind::All)) {
        if (PD == Default->getDeclContext()->getAsProtocolExtensionContext()) {
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
