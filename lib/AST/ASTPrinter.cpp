//===--- ASTPrinter.cpp - Swift Language AST Printer ----------------------===//
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
//  This file implements printing for the Swift ASTs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Config.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Strings.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <queue>

using namespace swift;

struct SynthesizedExtensionAnalyzer::Implementation {
  static bool isMemberFavored(const NominalTypeDecl* Target, const Decl* D) {
    DeclContext* DC = Target->getInnermostDeclContext();
    Type BaseTy = Target->getDeclaredTypeInContext();
    const FuncDecl *FD = dyn_cast<FuncDecl>(D);
    if (!FD)
      return true;
    ResolvedMemberResult Result = resolveValueMember(*DC, BaseTy,
                                                    FD->getEffectiveFullName());
    return !(Result.hasBestOverload() && Result.getBestOverload() != D);
  }

  static bool isExtensionFavored(const NominalTypeDecl* Target,
                                 const ExtensionDecl *ED) {
    return std::find_if(ED->getMembers().begin(), ED->getMembers().end(),
      [&](DeclIterator It) { return isMemberFavored(Target, *It);}) !=
        ED->getMembers().end();
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
      if (shouldPrint(TL.getType()->getAnyNominal(), Options))
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
    for (auto TL : Target->getInherited()) {
      if (!isEnumRawType(Target, TL))
        addTypeLocNominal(TL);
    }
    while (!Unhandled.empty()) {
      NominalTypeDecl* Back = Unhandled.back();
      Unhandled.pop_back();
      for (ExtensionDecl *E : Back->getExtensions()) {
        if (!shouldPrint(E, Options))
          continue;
        auto Pair = isApplicable(E, /*Synthesized*/true);
        if (Pair.first) {
          InfoMap->insert({E, Pair.first});
          MergeInfoMap.insert({E, Pair.second});
        }
        for (auto TL : Back->getInherited()) {
          if (!isEnumRawType(Target, TL))
            addTypeLocNominal(TL);
        }
      }
    }

    // Merge with actual extensions.
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

PrintOptions PrintOptions::printTypeInterface(Type T) {
  PrintOptions result = printInterface();
  result.PrintExtensionFromConformingProtocols = true;
  result.TransformContext = TypeTransformContext(T);
  result.printExtensionContentAsMembers = [T](const ExtensionDecl *ED) {
    return isExtensionApplied(*T->getNominalOrBoundGenericNominal()->
                              getDeclContext(), T, ED);
  };
  return result;
}

void PrintOptions::setBaseType(Type T) {
  TransformContext = TypeTransformContext(T);
}

void PrintOptions::initForSynthesizedExtension(NominalTypeDecl *D) {
  TransformContext = TypeTransformContext(D);
}

void PrintOptions::clearSynthesizedExtension() {
  TransformContext.reset();
}

TypeTransformContext::TypeTransformContext(Type T)
    : BaseType(T.getPointer()) {
  assert(T->mayHaveMembers());
}

TypeTransformContext::TypeTransformContext(NominalTypeDecl *NTD)
    : BaseType(NTD->getDeclaredTypeInContext().getPointer()), Nominal(NTD) {}

NominalTypeDecl *TypeTransformContext::getNominal() const {
  return Nominal;
}

Type TypeTransformContext::getBaseType() const {
  return Type(BaseType);
}

bool TypeTransformContext::isPrintingSynthesizedExtension() const {
  return Nominal != nullptr;
}

std::string ASTPrinter::sanitizeUtf8(StringRef Text) {
  llvm::SmallString<256> Builder;
  Builder.reserve(Text.size());
  const llvm::UTF8* Data = reinterpret_cast<const llvm::UTF8*>(Text.begin());
  const llvm::UTF8* End = reinterpret_cast<const llvm::UTF8*>(Text.end());
  StringRef Replacement = u8"\ufffd";
  while (Data < End) {
    auto Step = llvm::getNumBytesForUTF8(*Data);
    if (Data + Step > End) {
      Builder.append(Replacement);
      break;
    }

    if (llvm::isLegalUTF8Sequence(Data, Data + Step)) {
      Builder.append(Data, Data + Step);
    } else {

      // If malformed, add replacement characters.
      Builder.append(Replacement);
    }
    Data += Step;
  }
  return Builder.str();
}

ValueDecl* ASTPrinter::findConformancesWithDocComment(ValueDecl *VD) {
  assert(VD->getRawComment().isEmpty());
  std::queue<ValueDecl*> AllConformances;
  AllConformances.push(VD);
  while (!AllConformances.empty()) {
    auto *VD = AllConformances.front();
    AllConformances.pop();
    if (VD->getRawComment().isEmpty()) {
      for (auto *Req : VD->getSatisfiedProtocolRequirements()) {
        AllConformances.push(Req);
      }
    } else {
      return VD;
    }
  }
  return nullptr;
}

void ASTPrinter::anchor() {}

void ASTPrinter::printIndent() {
  llvm::SmallString<16> Str;
  for (unsigned i = 0; i != CurrentIndentation; ++i)
    Str += ' ';

  printText(Str);
}

void ASTPrinter::printTextImpl(StringRef Text) {
  forceNewlines();
  printText(Text);
}

void ASTPrinter::printTypeRef(Type T, const TypeDecl *RefTo, Identifier Name) {
  PrintNameContext Context = PrintNameContext::Normal;
  if (isa<GenericTypeParamDecl>(RefTo)) {
    Context = PrintNameContext::GenericParameter;
  } else if (T && T->is<DynamicSelfType>()) {
    assert(T->castTo<DynamicSelfType>()->getSelfType()->getAnyNominal() &&
           "protocol Self handled as GenericTypeParamDecl");
    Context = PrintNameContext::ClassDynamicSelf;
  }

  printName(Name, Context);
}

void ASTPrinter::printModuleRef(ModuleEntity Mod, Identifier Name) {
  printName(Name);
}

void ASTPrinter::callPrintDeclPre(const Decl *D,
                                  Optional<BracketOptions> Bracket) {
  forceNewlines();

  if (SynthesizeTarget && isa<ExtensionDecl>(D))
    printSynthesizedExtensionPre(cast<ExtensionDecl>(D), SynthesizeTarget, Bracket);
  else
    printDeclPre(D, Bracket);
}

ASTPrinter &ASTPrinter::operator<<(unsigned long long N) {
  llvm::SmallString<32> Str;
  llvm::raw_svector_ostream OS(Str);
  OS << N;
  printTextImpl(OS.str());
  return *this;
}

ASTPrinter &ASTPrinter::operator<<(UUID UU) {
  llvm::SmallString<UUID::StringBufferSize> Str;
  UU.toString(Str);
  printTextImpl(Str);
  return *this;
}

ASTPrinter &ASTPrinter::operator<<(DeclName name) {
  llvm::SmallString<32> str;
  llvm::raw_svector_ostream os(str);
  name.print(os);
  printTextImpl(os.str());
  return *this;
}

llvm::raw_ostream &swift::
operator<<(llvm::raw_ostream &OS, tok keyword) {
  switch (keyword) {
#define KEYWORD(KW) case tok::kw_##KW: OS << #KW; break;
#define POUND_KEYWORD(KW) case tok::pound_##KW: OS << "#"#KW; break;
#define PUNCTUATOR(PUN, TEXT) case tok::PUN: OS << TEXT; break;
#include "swift/Syntax/TokenKinds.def"
  default:
    llvm_unreachable("unexpected keyword or punctuator kind");
  }
  return OS;
}

uint8_t swift::getKeywordLen(tok keyword) {
  switch (keyword) {
#define KEYWORD(KW) case tok::kw_##KW: return StringRef(#KW).size();
#define POUND_KEYWORD(KW) case tok::pound_##KW: return StringRef("#"#KW).size();
#define PUNCTUATOR(PUN, TEXT) case tok::PUN: return StringRef(TEXT).size();
#include "swift/Syntax/TokenKinds.def"
  default:
    llvm_unreachable("unexpected keyword or punctuator kind");
  }
}

StringRef swift::getCodePlaceholder() { return "<#code#>"; }

void swift::
printEnumElmentsAsCases(llvm::DenseSet<EnumElementDecl*> &UnhandledElements,
                        llvm::raw_ostream &OS) {
  // Sort the missing elements to a vector because set does not guarantee orders.
  SmallVector<EnumElementDecl*, 4> SortedElements;
  SortedElements.insert(SortedElements.begin(), UnhandledElements.begin(),
                        UnhandledElements.end());
  std::sort(SortedElements.begin(),SortedElements.end(),
            [](EnumElementDecl *LHS, EnumElementDecl *RHS) {
              return LHS->getNameStr().compare(RHS->getNameStr()) < 0;
            });

  auto printPayloads = [](EnumElementDecl *EE, llvm::raw_ostream &OS) {
    // If the enum element has no payloads, return.
    auto TL = EE->getArgumentTypeLoc();
    if (TL.isNull())
      return;
    TypeRepr* TR = EE->getArgumentTypeLoc().getTypeRepr();
    if (auto *TTR = dyn_cast<TupleTypeRepr>(TR)) {
      SmallVector<Identifier, 4> NameBuffer;
      ArrayRef<Identifier> Names;
      if (TTR->hasElementNames()) {
        // Get the name from the tuple repr, if exist.
        Names = TTR->getElementNames();
      } else {
        // Create same amount of empty names to the elements.
        NameBuffer.resize(TTR->getNumElements());
        Names = llvm::makeArrayRef(NameBuffer);
      }
      OS << "(";
      // Print each element in the pattern match.
      for (unsigned I = 0, N = Names.size(); I < N; I ++) {
        auto Id = Names[I];
        if (Id.empty())
          OS << "_";
        else
          OS << tok::kw_let << " " << Id.str();
        if (I + 1 != N) {
          OS << ", ";
        }
      }
      OS << ")";
    }
  };

  // Print each enum element name.
  std::for_each(SortedElements.begin(), SortedElements.end(),
                [&](EnumElementDecl *EE) {
                  OS << tok::kw_case << " ." << EE->getNameStr();
                  printPayloads(EE, OS);
                  OS <<": " << getCodePlaceholder() << "\n";
                });
}

ASTPrinter &operator<<(ASTPrinter &printer, tok keyword) {
  SmallString<16> Buffer;
  llvm::raw_svector_ostream OS(Buffer);
  OS << keyword;
  printer.printKeyword(Buffer.str());
  return printer;
}

/// Determine whether to escape the given keyword in the given context.
static bool escapeKeywordInContext(StringRef keyword, PrintNameContext context){
  switch (context) {
  case PrintNameContext::Normal:
  case PrintNameContext::Attribute:
    return true;
  case PrintNameContext::Keyword:
    return false;

  case PrintNameContext::ClassDynamicSelf:
  case PrintNameContext::GenericParameter:
    return keyword != "Self";

  case PrintNameContext::FunctionParameterExternal:
  case PrintNameContext::FunctionParameterLocal:
  case PrintNameContext::TupleElement:
    return !canBeArgumentLabel(keyword);
  }

  llvm_unreachable("Unhandled PrintNameContext in switch.");
}

void ASTPrinter::printName(Identifier Name, PrintNameContext Context) {
  callPrintNamePre(Context);

  if (Name.empty()) {
    *this << "_";
    printNamePost(Context);
    return;
  }
  bool IsKeyword = llvm::StringSwitch<bool>(Name.str())
#define KEYWORD(KW) \
      .Case(#KW, true)
#include "swift/Syntax/TokenKinds.def"
      .Default(false);

  if (IsKeyword)
    IsKeyword = escapeKeywordInContext(Name.str(), Context);

  if (IsKeyword)
    *this << "`";
  *this << Name.str();
  if (IsKeyword)
    *this << "`";

  printNamePost(Context);
}

void StreamPrinter::printText(StringRef Text) {
  OS << Text;
}

/// Whether we will be printing a TypeLoc by using the TypeRepr printer
static bool willUseTypeReprPrinting(TypeLoc tyLoc,
                                    Type currentType,
                                    PrintOptions options) {
  // Special case for when transforming archetypes
  if (currentType && tyLoc.getType())
    return false;

  return ((options.PreferTypeRepr && tyLoc.hasLocation()) ||
          (tyLoc.getType().isNull() && tyLoc.getTypeRepr()));
}

namespace {
/// \brief AST pretty-printer.
class PrintAST : public ASTVisitor<PrintAST> {
  ASTPrinter &Printer;
  PrintOptions Options;
  unsigned IndentLevel = 0;
  Decl *Current = nullptr;
  Type CurrentType;

  friend DeclVisitor<PrintAST>;

  /// \brief RAII object that increases the indentation level.
  class IndentRAII {
    PrintAST &Self;
    bool DoIndent;

  public:
    IndentRAII(PrintAST &self, bool DoIndent = true)
        : Self(self), DoIndent(DoIndent) {
      if (DoIndent)
        Self.IndentLevel += Self.Options.Indent;
    }

    ~IndentRAII() {
      if (DoIndent)
        Self.IndentLevel -= Self.Options.Indent;
    }
  };

  /// \brief Indent the current number of indentation spaces.
  void indent() {
    Printer.setIndent(IndentLevel);
  }

  /// \brief Record the location of this declaration, which is about to
  /// be printed, marking the name and signature end locations.
  template<typename FnTy>
  void recordDeclLoc(Decl *decl, const FnTy &NameFn,
                     llvm::function_ref<void()> ParamFn = []{}) {
    Printer.callPrintDeclLoc(decl);
    NameFn();
    Printer.printDeclNameEndLoc(decl);
    ParamFn();
    Printer.printDeclNameOrSignatureEndLoc(decl);
  }

  void printSourceRange(CharSourceRange Range, ASTContext &Ctx) {
    Printer << Ctx.SourceMgr.extractText(Range);
  }

  void printClangDocumentationComment(const clang::Decl *D) {
    const auto &ClangContext = D->getASTContext();
    const clang::RawComment *RC = ClangContext.getRawCommentForAnyRedecl(D);
    if (!RC)
      return;

    bool Invalid;
    unsigned StartLocCol =
        ClangContext.getSourceManager().getSpellingColumnNumber(
            RC->getLocStart(), &Invalid);
    if (Invalid)
      StartLocCol = 0;

    unsigned WhitespaceToTrim = StartLocCol ? StartLocCol - 1 : 0;

    SmallVector<StringRef, 8> Lines;

    StringRef RawText =
        RC->getRawText(ClangContext.getSourceManager()).rtrim("\n\r");
    trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

    for (auto Line : Lines) {
      Printer << ASTPrinter::sanitizeUtf8(Line);
      Printer.printNewline();
    }
  }

  void printRawComment(RawComment RC) {
    indent();

    SmallVector<StringRef, 8> Lines;
    for (const auto &SRC : RC.Comments) {
      Lines.clear();

      StringRef RawText = SRC.RawText.rtrim("\n\r");
      unsigned WhitespaceToTrim = SRC.StartColumn - 1;
      trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

      for (auto Line : Lines) {
        Printer << Line;
        Printer.printNewline();
      }
    }
  }

  void printSwiftDocumentationComment(const Decl *D) {
    auto RC = D->getRawComment();
    if (RC.isEmpty() && !Options.ElevateDocCommentFromConformance)
      return;

    if (RC.isEmpty()) {
      if (auto *VD = dyn_cast<ValueDecl>(D)) {
        if (auto *Req = ASTPrinter::findConformancesWithDocComment(
            const_cast<ValueDecl*>(VD))) {
          printRawComment(Req->getRawComment());
        }
      }
    } else {
      printRawComment(RC);
    }
  }

  void printDocumentationComment(const Decl *D) {
    if (!Options.PrintDocumentationComments)
      return;

    // Try to print a comment from Clang.
    auto MaybeClangNode = D->getClangNode();
    if (MaybeClangNode) {
      if (auto *CD = MaybeClangNode.getAsDecl())
        printClangDocumentationComment(CD);
      return;
    }

    printSwiftDocumentationComment(D);
  }

  void printStaticKeyword(StaticSpellingKind StaticSpelling) {
    switch (StaticSpelling) {
    case StaticSpellingKind::None:
      llvm_unreachable("should not be called for non-static decls");
    case StaticSpellingKind::KeywordStatic:
      Printer << tok::kw_static << " ";
      break;
    case StaticSpellingKind::KeywordClass:
      Printer << tok::kw_class << " ";
      break;
    }
  }

  void printAccessibility(Accessibility access, StringRef suffix = "") {
    switch (access) {
    case Accessibility::Private:
      Printer << tok::kw_private;
      break;
    case Accessibility::FilePrivate:
      Printer << tok::kw_fileprivate;
      break;
    case Accessibility::Internal:
      if (!Options.PrintInternalAccessibilityKeyword)
        return;
      Printer << tok::kw_internal;
      break;
    case Accessibility::Public:
      Printer << tok::kw_public;
      break;
    case Accessibility::Open:
      Printer.printKeyword("open");
      break;
    }
    Printer << suffix << " ";
  }

  void printAccessibility(const ValueDecl *D) {
    if (!Options.PrintAccessibility || !D->hasAccessibility() ||
        D->getAttrs().hasAttribute<AccessibilityAttr>())
      return;

    printAccessibility(D->getFormalAccess());

    if (auto storageDecl = dyn_cast<AbstractStorageDecl>(D)) {
      if (auto setter = storageDecl->getSetter()) {
        Accessibility setterAccess = setter->getFormalAccess();
        if (setterAccess != D->getFormalAccess())
          printAccessibility(setterAccess, "(set)");
      }
    }
  }

  void printType(Type T) {
    if (Options.TransformContext) {
      // FIXME: it's not clear exactly what we want to keep from the existing
      // options, and what we want to discard.
      PrintOptions FreshOptions;
      FreshOptions.ExcludeAttrList = Options.ExcludeAttrList;
      FreshOptions.ExclusiveAttrList = Options.ExclusiveAttrList;
      T.print(Printer, FreshOptions);
      return;
    }

    T.print(Printer, Options);
  }

  void printTransformedType(Type T) {
    if (CurrentType) {
      if (T->hasArchetype()) {
        // Get the interface type, since TypeLocs still have
        // contextual types in them.
        T = Current->getInnermostDeclContext()->mapTypeOutOfContext(T);
      }

      auto *M = Current->getDeclContext()->getParentModule();
      SubstitutionMap subMap;

      if (auto *NTD = dyn_cast<NominalTypeDecl>(Current))
        subMap = CurrentType->getContextSubstitutionMap(M, NTD);
      else if (auto *ED = dyn_cast<ExtensionDecl>(Current))
        subMap = CurrentType->getContextSubstitutionMap(M, ED);
      else {
        subMap = CurrentType->getMemberSubstitutionMap(
          M, cast<ValueDecl>(Current));
      }

      T = T.subst(subMap, SubstFlags::DesugarMemberTypes);
    }

    printType(T);
  }

  void printTypeLoc(const TypeLoc &TL) {
    if (CurrentType && TL.getType()) {
      printTransformedType(TL.getType());
      return;
    }

    // Print a TypeRepr if instructed to do so by options, or if the type
    // is null.
    if (willUseTypeReprPrinting(TL, CurrentType, Options)) {
      if (auto repr = TL.getTypeRepr()) {
        llvm::SaveAndRestore<bool> SPTA(Options.SkipParameterTypeAttributes,
                                        true);
        repr->print(Printer, Options);
      }
      return;
    }

    TL.getType().print(Printer, Options);
  }

  void printAttributes(const Decl *D);
  void printTypedPattern(const TypedPattern *TP);

public:
  void printPattern(const Pattern *pattern);

  enum GenericSignatureFlags {
    PrintParams = 1,
    PrintRequirements = 2,
    InnermostOnly = 4,
    SkipSelfRequirement = 8,
    SwapSelfAndDependentMemberType = 16,
  };

  void printWhereClauseFromRequirementSignature(ProtocolDecl *proto,
                                                Decl *attachingTo);
  void printTrailingWhereClause(TrailingWhereClause *whereClause);

  void printGenericSignature(const GenericSignature *genericSig,
                             unsigned flags);
  void
  printGenericSignature(const GenericSignature *genericSig, unsigned flags,
                        llvm::function_ref<bool(const Requirement &)> filter);
  void printSingleDepthOfGenericSignature(
      ArrayRef<GenericTypeParamType *> genericParams,
      ArrayRef<Requirement> requirements, unsigned flags,
      llvm::function_ref<bool(const Requirement &)> filter);
  void printRequirement(const Requirement &req);

private:
  bool shouldPrint(const Decl *D, bool Notify = false);
  bool shouldPrintPattern(const Pattern *P);
  void printPatternType(const Pattern *P);
  void printAccessors(AbstractStorageDecl *ASD);
  void printMembersOfDecl(Decl * NTD, bool needComma = false,
                          bool openBracket = true, bool closeBracket = true);
  void printMembers(ArrayRef<Decl *> members, bool needComma = false,
                    bool openBracket = true, bool closeBracket = true);
  void printNominalDeclGenericParams(NominalTypeDecl *decl);
  void printNominalDeclGenericRequirements(NominalTypeDecl *decl);
  void printInherited(const Decl *decl,
                      ArrayRef<TypeLoc> inherited,
                      ArrayRef<ProtocolDecl *> protos,
                      Type superclass = {},
                      bool explicitClass = false,
                      bool PrintAsProtocolComposition = false);

  void printInherited(const NominalTypeDecl *decl,
                      bool explicitClass = false);
  void printInherited(const EnumDecl *D);
  void printInherited(const ExtensionDecl *decl);
  void printInherited(const GenericTypeParamDecl *D);

  void printEnumElement(EnumElementDecl *elt);

  /// \returns true if anything was printed.
  bool printASTNodes(const ArrayRef<ASTNode> &Elements, bool NeedIndent = true);

  void printOneParameter(const ParamDecl *param, ParameterTypeFlags paramFlags,
                         bool Curried, bool ArgNameIsAPIByDefault);

  void printParameterList(ParameterList *PL, Type paramListTy, bool isCurried,
                          std::function<bool()> isAPINameByDefault);

  /// \brief Print the function parameters in curried or selector style,
  /// to match the original function declaration.
  void printFunctionParameters(AbstractFunctionDecl *AFD);

#define DECL(Name,Parent) void visit##Name##Decl(Name##Decl *decl);
#define ABSTRACT_DECL(Name, Parent)
#define DECL_RANGE(Name,Start,End)
#include "swift/AST/DeclNodes.def"

#define STMT(Name, Parent) void visit##Name##Stmt(Name##Stmt *stmt);
#include "swift/AST/StmtNodes.def"

  void printSynthesizedExtension(NominalTypeDecl* Decl,
                                 ExtensionDecl* ExtDecl);

  void printExtension(ExtensionDecl* ExtDecl);

public:
  PrintAST(ASTPrinter &Printer, const PrintOptions &Options)
      : Printer(Printer), Options(Options) {
    if (Options.TransformContext)
      CurrentType = Options.TransformContext->getBaseType();
  }

  using ASTVisitor::visit;

  bool visit(Decl *D) {
    if (!shouldPrint(D, true))
      return false;

    Decl *Old = Current;
    Current = D;
    SWIFT_DEFER { Current = Old; };

    Type OldType = CurrentType;
    if (CurrentType && (Old != nullptr || Options.PrintAsMember)) {
      if (auto *NTD = dyn_cast<NominalTypeDecl>(D)) {
        auto Subs = CurrentType->getContextSubstitutionMap(
          Options.CurrentModule, NTD->getDeclContext());
        CurrentType = NTD->getDeclaredInterfaceType().subst(Subs);
      }
    }

    SWIFT_DEFER { CurrentType = OldType; };

    bool Synthesize =
        Options.TransformContext &&
        Options.TransformContext->isPrintingSynthesizedExtension() &&
        isa<ExtensionDecl>(D);
    if (Synthesize)
      Printer.setSynthesizedTarget(Options.TransformContext->getNominal());

    // We want to print a newline before doc comments.  Swift code already
    // handles this, but we need to insert it for clang doc comments when not
    // printing other clang comments. Do it now so the printDeclPre callback
    // happens after the newline.
    if (Options.PrintDocumentationComments &&
        !Options.PrintRegularClangComments &&
        D->hasClangNode()) {
      auto clangNode = D->getClangNode();
      auto clangDecl = clangNode.getAsDecl();
      if (clangDecl &&
          clangDecl->getASTContext().getRawCommentForAnyRedecl(clangDecl)) {
        Printer.printNewline();
        indent();
      }
    }

    Printer.callPrintDeclPre(D, Options.BracketOptions);

    ASTVisitor::visit(D);

    if (Synthesize) {
      Printer.setSynthesizedTarget(nullptr);
      Printer.printSynthesizedExtensionPost(
          cast<ExtensionDecl>(D), Options.TransformContext->getNominal(),
          Options.BracketOptions);
    } else {
      Printer.callPrintDeclPost(D, Options.BracketOptions);
    }

    return true;
  }

};
} // unnamed namespace

static StaticSpellingKind getCorrectStaticSpelling(const Decl *D) {
  if (auto *VD = dyn_cast<VarDecl>(D)) {
    return VD->getCorrectStaticSpelling();
  } else if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    return PBD->getCorrectStaticSpelling();
  } else if (auto *FD = dyn_cast<FuncDecl>(D)) {
    return FD->getCorrectStaticSpelling();
  } else {
    return StaticSpellingKind::None;
  }
}

void PrintAST::printAttributes(const Decl *D) {
  if (Options.SkipAttributes)
    return;

  // Don't print a redundant 'final' if we are printing a 'static' decl.
  unsigned originalExcludeAttrCount = Options.ExcludeAttrList.size();
  if (Options.PrintImplicitAttrs &&
      D->getDeclContext()->getAsClassOrClassExtensionContext() &&
      getCorrectStaticSpelling(D) == StaticSpellingKind::KeywordStatic) {
    Options.ExcludeAttrList.push_back(DAK_Final);
  }

  D->getAttrs().print(Printer, Options, D);

  Options.ExcludeAttrList.resize(originalExcludeAttrCount);
}

void PrintAST::printTypedPattern(const TypedPattern *TP) {
  printPattern(TP->getSubPattern());
  Printer << ": ";
  printTypeLoc(TP->getTypeLoc());
}

void PrintAST::printPattern(const Pattern *pattern) {
  switch (pattern->getKind()) {
  case PatternKind::Any:
    Printer << "_";
    break;

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);
    recordDeclLoc(named->getDecl(), [&]{
        Printer.printName(named->getBoundName());
      });
    break;
  }

  case PatternKind::Paren:
    Printer << "(";
    printPattern(cast<ParenPattern>(pattern)->getSubPattern());
    Printer << ")";
    break;

  case PatternKind::Tuple: {
    Printer << "(";
    auto TP = cast<TuplePattern>(pattern);
    auto Fields = TP->getElements();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      const auto &Elt = Fields[i];
      if (i != 0)
        Printer << ", ";

      printPattern(Elt.getPattern());
    }
    Printer << ")";
    break;
  }

  case PatternKind::Typed:
    printTypedPattern(cast<TypedPattern>(pattern));
    break;

  case PatternKind::Is: {
    auto isa = cast<IsPattern>(pattern);
    Printer << tok::kw_is << " ";
    isa->getCastTypeLoc().getType().print(Printer, Options);
    break;
  }

  case PatternKind::EnumElement: {
    auto elt = cast<EnumElementPattern>(pattern);
    // FIXME: Print element expr.
    if (elt->hasSubPattern())
      printPattern(elt->getSubPattern());
    break;
  }

  case PatternKind::OptionalSome:
    printPattern(cast<OptionalSomePattern>(pattern)->getSubPattern());
    Printer << '?';
    break;

  case PatternKind::Bool:
    Printer << (cast<BoolPattern>(pattern)->getValue() ? tok::kw_true
                                                       : tok::kw_false);
    break;

  case PatternKind::Expr:
    // FIXME: Print expr.
    break;

  case PatternKind::Var:
    if (!Options.SkipIntroducerKeywords)
      Printer << (cast<VarPattern>(pattern)->isLet() ? tok::kw_let
                                                     : tok::kw_var)
              << " ";
    printPattern(cast<VarPattern>(pattern)->getSubPattern());
  }
}

/// If we can't find the depth of a type, return ErrorDepth.
const unsigned ErrorDepth = ~0U;
/// A helper function to return the depth of a type.
static unsigned getDepthOfType(Type ty) {
  if (auto paramTy = ty->getAs<GenericTypeParamType>())
    return paramTy->getDepth();

  if (auto depMemTy = dyn_cast<DependentMemberType>(ty->getCanonicalType())) {
    CanType rootTy;
    do {
      rootTy = depMemTy.getBase();
    } while ((depMemTy = dyn_cast<DependentMemberType>(rootTy)));
    if (auto rootParamTy = dyn_cast<GenericTypeParamType>(rootTy))
      return rootParamTy->getDepth();
    return ErrorDepth;
  }

  return ErrorDepth;
}

namespace {
struct RequirementPrintLocation {
  /// The Decl where the requirement should be attached (whether inherited or in
  /// a where clause)
  Decl *AttachedTo;
  /// Whether the requirement needs to be in a where clause.
  bool InWhereClause;
};
};

/// Heuristically work out a good place for \c req to be printed inside \c
/// proto.
///
/// This depends only on the protocol so that we make the same decisions for all
/// requirements in all associated types, guaranteeing that all of them will be
/// printed somewhere. That is, taking an AssociatedTypeDecl as an argument and
/// asking "should this requirement be printed on this ATD?" seems more likely
/// to result in inconsistencies in what is printed where, versus what this
/// function does: asking "where should this requirement be printed?" and then
/// callers check if the location is the ATD.
static RequirementPrintLocation
bestRequirementPrintLocation(ProtocolDecl *proto, const Requirement &req) {
  auto protoSelf = proto->getProtocolSelfType();
  // Returns the most relevant decl within proto connected to outerType (or null
  // if one doesn't exist), and whether the type is an "direct use",
  // i.e. outerType itself is Self or Self.T, but not, say, Self.T.U, or
  // Array<Self.T>. (The first's decl will be proto, while the other three will
  // be Self.T.)
  auto findRelevantDeclAndDirectUse = [&](Type outerType) {
    TypeDecl *relevantDecl = nullptr;
    Type foundType;
    (void)outerType.findIf([&](Type t) {
      if (t->isEqual(protoSelf)) {
        relevantDecl = proto;
        foundType = t;
        return true;
      } else if (auto DMT = t->getAs<DependentMemberType>()) {
        auto assocType = DMT->getAssocType();
        if (assocType && assocType->getProtocol() == proto) {
          relevantDecl = assocType;
          foundType = t;
          return true;
        }
      }

      // not here, so let's keep looking.
      return false;
    });

    // If we didn't find anything, relevantDecl and foundType will be null, as
    // desired.
    auto directUse = foundType && outerType->isEqual(foundType);
    return std::make_pair(relevantDecl, directUse);
  };

  Decl *bestDecl;
  bool inWhereClause;

  switch (req.getKind()) {
  case RequirementKind::Conformance:
  case RequirementKind::Superclass:
  case RequirementKind::Layout: {
    auto subject = req.getFirstType();
    auto result = findRelevantDeclAndDirectUse(subject);

    bestDecl = result.first;

    // A requirement like Self : Protocol or Self.T : Class might be from an
    // inheritance, or might be a where clause.
    if (req.getKind() != RequirementKind::Layout && result.second) {
      auto inherited = req.getSecondType();
      inWhereClause =
          none_of(result.first->getInherited(), [&](const TypeLoc &loc) {
            return loc.getType()->isEqual(inherited);
          });
    } else {
      inWhereClause = true;
    }
    break;
  }
  case RequirementKind::SameType: {
    auto lhs = req.getFirstType();
    auto rhs = req.getSecondType();

    auto lhsResult = findRelevantDeclAndDirectUse(lhs);
    auto rhsResult = findRelevantDeclAndDirectUse(rhs);

    // Default to using the left type's decl.
    bestDecl = lhsResult.first;

    // But maybe the right type's one is "obviously" better!
    // e.g. Int == Self.T
    auto lhsDoesntExist = !lhsResult.first;
    // e.g. Self.T.U == Self.V should go on V (first two conditions), but
    // Self.T.U == Self should go on T (third condition).
    auto rhsBetterDirect =
        !lhsResult.second && rhsResult.second && rhsResult.first != proto;
    auto rhsOfSelfToAssoc = lhsResult.first == proto && rhsResult.first;
    // e.g. Self == Self.T.U
    if (lhsDoesntExist || rhsBetterDirect || rhsOfSelfToAssoc)
      bestDecl = rhsResult.first;

    // Same-type requirements can only occur in where clauses
    inWhereClause = true;
    break;
  }
  }
  // Didn't find anything that we think is relevant, so let's default to a where
  // clause on the protocol.
  if (!bestDecl) {
    bestDecl = proto;
    inWhereClause = true;
  }

  return {/*AttachedTo=*/bestDecl, inWhereClause};
}

void PrintAST::printWhereClauseFromRequirementSignature(ProtocolDecl *proto,
                                                        Decl *attachingTo) {
  assert(proto->isRequirementSignatureComputed());
  unsigned flags = PrintRequirements;
  if (isa<AssociatedTypeDecl>(attachingTo))
    flags |= SwapSelfAndDependentMemberType;
  printGenericSignature(
      proto->getRequirementSignature(), flags,
      [&](const Requirement &req) {
        auto location = bestRequirementPrintLocation(proto, req);
        return location.AttachedTo == attachingTo && location.InWhereClause;
      });
}

void PrintAST::printTrailingWhereClause(TrailingWhereClause *whereClause) {
  Printer << " " << tok::kw_where << " ";
  interleave(
      whereClause->getRequirements(),
      [&](const RequirementRepr &req) {
        Printer.callPrintStructurePre(PrintStructureKind::GenericRequirement);
        req.print(Printer);
        Printer.printStructurePost(PrintStructureKind::GenericRequirement);
      },
      [&] { Printer << ", "; });
}

/// A helper function to return the depth of a requirement.
static unsigned getDepthOfRequirement(const Requirement &req) {
  switch (req.getKind()) {
  case RequirementKind::Conformance:
  case RequirementKind::Layout:
  case RequirementKind::Superclass:
    return getDepthOfType(req.getFirstType());

  case RequirementKind::SameType: {
    // Return the max valid depth of firstType and secondType.
    unsigned firstDepth = getDepthOfType(req.getFirstType());
    unsigned secondDepth = getDepthOfType(req.getSecondType());

    unsigned maxDepth;
    if (firstDepth == ErrorDepth && secondDepth != ErrorDepth)
      maxDepth = secondDepth;
    else if (firstDepth != ErrorDepth && secondDepth == ErrorDepth)
      maxDepth = firstDepth;
    else
      maxDepth = std::max(firstDepth, secondDepth);

    return maxDepth;
  }
  }
  llvm_unreachable("bad RequirementKind");
}

static void getRequirementsAtDepth(const GenericSignature *genericSig,
                                   unsigned depth,
                                   SmallVectorImpl<Requirement> &result) {
  for (auto reqt : genericSig->getRequirements()) {
    unsigned currentDepth = getDepthOfRequirement(reqt);
    assert(currentDepth != ErrorDepth);
    if (currentDepth == depth)
      result.push_back(reqt);
  }
}

void PrintAST::printGenericSignature(const GenericSignature *genericSig,
                                     unsigned flags) {
  printGenericSignature(genericSig, flags,
                        // print everything
                        [&](const Requirement &) { return true; });
}
void PrintAST::printGenericSignature(
    const GenericSignature *genericSig, unsigned flags,
    llvm::function_ref<bool(const Requirement &)> filter) {
  if (flags & InnermostOnly) {
    auto genericParams = genericSig->getInnermostGenericParams();
    unsigned depth = genericParams[0]->getDepth();
    SmallVector<Requirement, 2> requirementsAtDepth;
    getRequirementsAtDepth(genericSig, depth, requirementsAtDepth);

    printSingleDepthOfGenericSignature(genericParams, requirementsAtDepth,
                                       flags, filter);
    return;
  }

  auto genericParams = genericSig->getGenericParams();
  auto requirements = genericSig->getRequirements();

  if (!Options.PrintInSILBody) {
    printSingleDepthOfGenericSignature(genericParams, requirements, flags,
                                       filter);
    return;
  }

  // In order to recover the nested GenericParamLists, we divide genericParams
  // and requirements according to depth.
  unsigned paramIdx = 0, numParam = genericParams.size();
  while (paramIdx < numParam) {
    unsigned depth = genericParams[paramIdx]->getDepth();

    // Move index to genericParams.
    unsigned lastParamIdx = paramIdx;
    do {
      lastParamIdx++;
    } while (lastParamIdx < numParam &&
             genericParams[lastParamIdx]->getDepth() == depth);

    // Collect requirements for this level.
    SmallVector<Requirement, 2> requirementsAtDepth;
    getRequirementsAtDepth(genericSig, depth, requirementsAtDepth);

    printSingleDepthOfGenericSignature(
        genericParams.slice(paramIdx, lastParamIdx - paramIdx),
        requirementsAtDepth, flags, filter);

    paramIdx = lastParamIdx;
  }
}

void PrintAST::printSingleDepthOfGenericSignature(
    ArrayRef<GenericTypeParamType *> genericParams,
    ArrayRef<Requirement> requirements, unsigned flags,
    llvm::function_ref<bool(const Requirement &)> filter) {
  bool printParams = (flags & PrintParams);
  bool printRequirements = (flags & PrintRequirements);
  bool swapSelfAndDependentMemberType =
    (flags & SwapSelfAndDependentMemberType);

  SubstitutionMap subMap;
  if (CurrentType) {
    if (!CurrentType->isExistentialType()) {
      auto *DC = Current->getInnermostDeclContext()->getInnermostTypeContext();
      auto *M = DC->getParentModule();
      subMap = CurrentType->getContextSubstitutionMap(M, DC);
    }
  }

  auto substParam = [&](Type param) -> Type {
    return param.subst(subMap);
  };

  if (printParams) {
    // Print the generic parameters.
    Printer << "<";
    interleave(genericParams,
               [&](GenericTypeParamType *param) {
                 if (!subMap.empty()) {
                   if (auto argTy = substParam(param))
                     printType(argTy);
                   else
                     printType(param);
                 } else if (auto *GP = param->getDecl()) {
                   Printer.callPrintStructurePre(
                       PrintStructureKind::GenericParameter, GP);
                   Printer.printName(GP->getName(),
                                     PrintNameContext::GenericParameter);
                   Printer.printStructurePost(
                       PrintStructureKind::GenericParameter, GP);
                 } else {
                   printType(param);
                 }
               },
               [&] { Printer << ", "; });
  }

  if (printRequirements) {
    bool isFirstReq = true;
    for (const auto &req : requirements) {
      if (!filter(req))
        continue;

      auto first = req.getFirstType();
      Type second;

      if (req.getKind() != RequirementKind::Layout)
        second = req.getSecondType();

      if ((flags & SkipSelfRequirement) &&
          req.getKind() == RequirementKind::Conformance) {
        auto proto = cast<ProtocolDecl>(second->getAnyNominal());
        if (first->isEqual(proto->getSelfInterfaceType()))
          continue;
      }

      if (!subMap.empty()) {
        if (Type subFirst = substParam(first))
          first = subFirst;
        if (second) {
          if (Type subSecond = substParam(second))
            second = subSecond;
          if (!(first->is<ArchetypeType>() || first->isTypeParameter()) &&
              !(second->is<ArchetypeType>() || second->isTypeParameter()))
            continue;
        }
      }

      if (isFirstReq) {
        Printer << " " << tok::kw_where << " ";
        isFirstReq = false;
      } else {
        Printer << ", ";
      }

      // Swap the order of Self == Self.A requirements if requested.
      if (swapSelfAndDependentMemberType &&
          req.getKind() == RequirementKind::SameType &&
          first->is<GenericTypeParamType>() &&
          second->is<DependentMemberType>())
        std::swap(first, second);

      Printer.callPrintStructurePre(PrintStructureKind::GenericRequirement);
      if (second) {
        Requirement substReq(req.getKind(), first, second);
        printRequirement(substReq);
      } else {
        Requirement substReq(req.getKind(), first, req.getLayoutConstraint());
        printRequirement(substReq);
      }
      Printer.printStructurePost(PrintStructureKind::GenericRequirement);
    }
  }

  if (printParams)
    Printer << ">";
}

void PrintAST::printRequirement(const Requirement &req) {
  printType(req.getFirstType());
  switch (req.getKind()) {
  case RequirementKind::Layout:
    Printer << " : ";
    req.getLayoutConstraint()->print(Printer, Options);
    return;
  case RequirementKind::Conformance:
  case RequirementKind::Superclass:
    Printer << " : ";
    break;
  case RequirementKind::SameType:
    Printer << " == ";
    break;
  }
  printType(req.getSecondType());
}

bool swift::shouldPrintPattern(const Pattern *P, PrintOptions &Options) {
  bool ShouldPrint = false;
  P->forEachVariable([&](VarDecl *VD) {
    ShouldPrint |= shouldPrint(VD, Options);
  });
  return ShouldPrint;
}

bool PrintAST::shouldPrintPattern(const Pattern *P) {
  return swift::shouldPrintPattern(P, Options);
}

void PrintAST::printPatternType(const Pattern *P) {
  if (P->hasType()) {
    Printer << ": ";
    printType(P->getType());
  }
}

static bool shouldPrintAsFavorable(const Decl *D, PrintOptions &Options) {
  if (!Options.TransformContext || !D->getDeclContext()->isExtensionContext() ||
      !Options.TransformContext->isPrintingSynthesizedExtension())
    return true;
  NominalTypeDecl *Target = Options.TransformContext->getNominal();
  Type BaseTy = Target->getDeclaredTypeInContext();
  const FuncDecl *FD = dyn_cast<FuncDecl>(D);
  if (!FD)
    return true;
  ResolvedMemberResult Result = resolveValueMember(*Target->getDeclContext(),
                                                  BaseTy,
                                                  FD->getEffectiveFullName());
  return !(Result.hasBestOverload() && Result.getBestOverload() != D);
}

bool swift::shouldPrint(const Decl *D, PrintOptions &Options) {
  if (!shouldPrintAsFavorable(D, Options))
    return false;
  if (auto *ED= dyn_cast<ExtensionDecl>(D)) {
    if (Options.printExtensionContentAsMembers(ED))
      return false;
  }

  if (Options.SkipDeinit && isa<DestructorDecl>(D)) {
    return false;
  }

  if (Options.SkipImports && isa<ImportDecl>(D)) {
    return false;
  }

  if (Options.SkipImplicit && D->isImplicit())
    return false;

  if (Options.SkipUnavailable &&
      D->getAttrs().isUnavailable(D->getASTContext()))
    return false;

  if (Options.ExplodeEnumCaseDecls) {
    if (isa<EnumElementDecl>(D))
      return true;
    if (isa<EnumCaseDecl>(D))
      return false;
  } else if (auto *EED = dyn_cast<EnumElementDecl>(D)) {
    // Enum elements are printed as part of the EnumCaseDecl, unless they were
    // imported without source info.
    return !EED->getSourceRange().isValid();
  }

  // Skip declarations that are not accessible.
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    if (Options.AccessibilityFilter > Accessibility::Private &&
        VD->hasAccessibility() &&
        VD->getFormalAccess() < Options.AccessibilityFilter)
      return false;
  }

  if (Options.SkipPrivateStdlibDecls &&
      D->isPrivateStdlibDecl(
                /*whitelistProtocols=*/!Options.SkipUnderscoredStdlibProtocols))
    return false;

  if (Options.SkipEmptyExtensionDecls && isa<ExtensionDecl>(D)) {
    auto Ext = cast<ExtensionDecl>(D);
    // If the extension doesn't add protocols or has no members that we should
    // print then skip printing it.
    if (Ext->getLocalProtocols().empty()) {
      bool HasMemberToPrint = false;
      for (auto Member : Ext->getMembers()) {
        if (shouldPrint(Member, Options)) {
          HasMemberToPrint = true;
          break;
        }
      }
      if (!HasMemberToPrint)
        return false;
    }
  }

  // If asked to skip overrides and witnesses, do so.
  if (Options.SkipOverrides) {
    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      if (VD->getOverriddenDecl()) return false;
      if (!VD->getSatisfiedProtocolRequirements().empty()) return false;

      if (auto clangDecl = VD->getClangDecl()) {
        // If the Clang declaration is from a protocol but was mirrored into
        // class or extension thereof, treat it as an override.
        if (isa<clang::ObjCProtocolDecl>(clangDecl->getDeclContext()) &&
            VD->getDeclContext()->getAsClassOrClassExtensionContext())
          return false;

        // Check whether Clang considers it an override.
        if (auto objcMethod = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
          SmallVector<const clang::ObjCMethodDecl *, 4> overriddenMethods;
          objcMethod->getOverriddenMethods(overriddenMethods);
          if (!overriddenMethods.empty()) return false;
        } else if (auto objcProperty
                     = dyn_cast<clang::ObjCPropertyDecl>(clangDecl)) {
          if (auto getter = objcProperty->getGetterMethodDecl()) {
            SmallVector<const clang::ObjCMethodDecl *, 4> overriddenMethods;
            getter->getOverriddenMethods(overriddenMethods);
            if (!overriddenMethods.empty()) return false;
          }
        }
      }
    }
  }

  // We need to handle PatternBindingDecl as a special case here because its
  // attributes can only be retrieved from the inside VarDecls.
  if (auto *PD = dyn_cast<PatternBindingDecl>(D)) {
    auto ShouldPrint = false;
    for (auto entry : PD->getPatternList()) {
      ShouldPrint |= shouldPrintPattern(entry.getPattern(), Options);
      if (ShouldPrint)
        return true;
    }
    return false;
  }
  return true;
}

bool PrintAST::shouldPrint(const Decl *D, bool Notify) {
  auto Result = swift::shouldPrint(D, Options);
  if (!Result && Notify)
    Printer.callAvoidPrintDeclPost(D);
  return Result;
}

static bool isAccessorAssumedNonMutating(FuncDecl *accessor) {
  switch (accessor->getAccessorKind()) {
  case AccessorKind::IsGetter:
  case AccessorKind::IsAddressor:
    return true;

  case AccessorKind::IsSetter:
  case AccessorKind::IsWillSet:
  case AccessorKind::IsDidSet:
  case AccessorKind::IsMaterializeForSet:
  case AccessorKind::IsMutableAddressor:
    return false;

  case AccessorKind::NotAccessor:
    llvm_unreachable("not an addressor!");
  }
  llvm_unreachable("bad addressor kind");
}

static StringRef getAddressorLabel(FuncDecl *addressor) {
  switch (addressor->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("addressor claims not to be an addressor");
  case AddressorKind::Unsafe:
    return "unsafeAddress";
  case AddressorKind::Owning:
    return "addressWithOwner";
  case AddressorKind::NativeOwning:
    return "addressWithNativeOwner";
  case AddressorKind::NativePinning:
    return "addressWithPinnedNativeOwner";
  }
  llvm_unreachable("bad addressor kind");
}

static StringRef getMutableAddressorLabel(FuncDecl *addressor) {
  switch (addressor->getAddressorKind()) {
  case AddressorKind::NotAddressor:
    llvm_unreachable("addressor claims not to be an addressor");
  case AddressorKind::Unsafe:
    return "unsafeMutableAddress";
  case AddressorKind::Owning:
    return "mutableAddressWithOwner";
  case AddressorKind::NativeOwning:
    return "mutableAddressWithNativeOwner";
  case AddressorKind::NativePinning:
    return "mutableAddressWithPinnedNativeOwner";
  }
  llvm_unreachable("bad addressor kind");
}

void PrintAST::printAccessors(AbstractStorageDecl *ASD) {
  if (isa<VarDecl>(ASD) && !Options.PrintPropertyAccessors)
    return;

  auto storageKind = ASD->getStorageKind();

  // Never print anything for stored properties.
  if (storageKind == AbstractStorageDecl::Stored)
    return;

  // Treat StoredWithTrivialAccessors the same as Stored unless
  // we're printing for SIL, in which case we want to distinguish it
  // from a pure stored property.
  if (storageKind == AbstractStorageDecl::StoredWithTrivialAccessors) {
    if (!Options.PrintForSIL) return;

    // Don't print an accessor for a let; the parser can't handle it.
    if (isa<VarDecl>(ASD) && cast<VarDecl>(ASD)->isLet())
      return;
  }

  // We sometimes want to print the accessors abstractly
  // instead of listing out how they're actually implemented.
  bool inProtocol = isa<ProtocolDecl>(ASD->getDeclContext());
  if (!Options.FunctionBody &&
      (inProtocol ||
        (Options.AbstractAccessors && !Options.FunctionDefinitions))) {
    bool mutatingGetter = ASD->getGetter() && ASD->isGetterMutating();
    bool settable = ASD->isSettable(nullptr);
    bool nonmutatingSetter = false;
    if (settable && ASD->isSetterNonMutating() && ASD->isInstanceMember() &&
        !ASD->getDeclContext()->getDeclaredTypeInContext()
            ->hasReferenceSemantics())
      nonmutatingSetter = true;

    // We're about to print something like this:
    //   { mutating? get (nonmutating? set)? }
    // But don't print "{ get set }" if we don't have to.
    if (!inProtocol && !Options.PrintGetSetOnRWProperties &&
        settable && !mutatingGetter && !nonmutatingSetter) {
      return;
    }

    Printer << " {";
    if (mutatingGetter) {
      Printer << " ";
      Printer.printKeyword("mutating");
    }
    Printer << " ";
    Printer.printKeyword("get");
    if (settable) {
      if (nonmutatingSetter) {
        Printer << " ";
        Printer.printKeyword("nonmutating");
      }
      Printer << " ";
      Printer.printKeyword("set");
    }
    Printer << " }";
    return;
  }

  // Honor !Options.PrintGetSetOnRWProperties in the only remaining
  // case where we could end up printing { get set }.
  if (storageKind == AbstractStorageDecl::StoredWithTrivialAccessors ||
      storageKind == AbstractStorageDecl::Computed) {
    if (!Options.PrintGetSetOnRWProperties &&
        !Options.FunctionDefinitions &&
        ASD->getSetter() &&
        !ASD->getGetter()->isMutating() &&
        !ASD->getSetter()->isExplicitNonMutating()) {
      return;
    }
  }

  // Otherwise, print all the concrete defining accessors.

  bool PrintAccessorBody = Options.FunctionDefinitions || Options.FunctionBody;

  auto PrintAccessor = [&](FuncDecl *Accessor, StringRef Label) {
    if (!Accessor)
      return;
    if (!PrintAccessorBody) {
      if (isAccessorAssumedNonMutating(Accessor)) {
        if (Accessor->isMutating()) {
          Printer << " ";
          Printer.printKeyword("mutating");
        }
      } else {
        if (Accessor->isExplicitNonMutating()) {
          Printer << " ";
          Printer.printKeyword("nonmutating");
        }
      }
      Printer << " ";
      Printer.printKeyword(Label); // Contextual keyword get, set, ...
    } else {
      Printer.printNewline();
      IndentRAII IndentMore(*this);
      indent();
      visit(Accessor);
    }
  };

  auto PrintAddressor = [&](FuncDecl *accessor) {
    if (!accessor) return;
    PrintAccessor(accessor, getAddressorLabel(accessor));
  };

  auto PrintMutableAddressor = [&](FuncDecl *accessor) {
    if (!accessor) return;
    PrintAccessor(accessor, getMutableAddressorLabel(accessor));
  };

  Printer << " {";
  switch (storageKind) {
  case AbstractStorageDecl::Stored:
    llvm_unreachable("filtered out above!");

  case AbstractStorageDecl::StoredWithTrivialAccessors:
  case AbstractStorageDecl::Computed:
    if (ASD->getGetter() && !ASD->getSetter() && PrintAccessorBody &&
          !Options.FunctionDefinitions) {
      // Omit the 'get' keyword. Directly print getter
      if (auto BodyFunc = Options.FunctionBody) {
        Printer.printNewline();
        IndentRAII IndentBody(*this);
        indent();
        Printer << BodyFunc(ASD->getGetter());
      }
    } else {
      PrintAccessor(ASD->getGetter(), "get");
      PrintAccessor(ASD->getSetter(), "set");
    }
    break;

  case AbstractStorageDecl::StoredWithObservers:
  case AbstractStorageDecl::InheritedWithObservers:
    PrintAccessor(ASD->getWillSetFunc(), "willSet");
    PrintAccessor(ASD->getDidSetFunc(), "didSet");
    break;

  case AbstractStorageDecl::Addressed:
  case AbstractStorageDecl::AddressedWithTrivialAccessors:
  case AbstractStorageDecl::AddressedWithObservers:
    PrintAddressor(ASD->getAddressor());
    PrintMutableAddressor(ASD->getMutableAddressor());
    if (ASD->hasObservers()) {
      PrintAccessor(ASD->getWillSetFunc(), "willSet");
      PrintAccessor(ASD->getDidSetFunc(), "didSet");
    }
    break;

  case AbstractStorageDecl::ComputedWithMutableAddress:
    PrintAccessor(ASD->getGetter(), "get");
    PrintMutableAddressor(ASD->getMutableAddressor());
    break;
  }
  if (PrintAccessorBody) {
    Printer.printNewline();
    indent();
  } else
    Printer << " ";
  Printer << "}";
}

void PrintAST::printMembersOfDecl(Decl *D, bool needComma,
                                  bool openBracket,
                                  bool closeBracket) {
  llvm::SmallVector<Decl *, 3> Members;
  auto AddDeclFunc = [&](DeclRange Range) {
    for (auto RD : Range)
      Members.push_back(RD);
  };

  if (auto Ext = dyn_cast<ExtensionDecl>(D)) {
    AddDeclFunc(Ext->getMembers());
  } else if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
    AddDeclFunc(NTD->getMembers());
    for (auto Ext : NTD->getExtensions()) {
      if (Options.printExtensionContentAsMembers(Ext))
        AddDeclFunc(Ext->getMembers());
    }
    if (Options.PrintExtensionFromConformingProtocols) {
      for (auto Conf : NTD->getAllConformances()) {
        for (auto Ext : Conf->getProtocol()->getExtensions()) {
          if (Options.printExtensionContentAsMembers(Ext))
            AddDeclFunc(Ext->getMembers());
        }
      }
    }
  }
  printMembers(Members, needComma, openBracket, closeBracket);
}

void PrintAST::printMembers(ArrayRef<Decl *> members, bool needComma,
                            bool openBracket, bool closeBracket) {
  if (openBracket) {
    Printer << " {";
    Printer.printNewline();
  }
  {
    IndentRAII indentMore(*this);
    for (auto i = members.begin(), iEnd = members.end(); i != iEnd; ++i) {
      auto member = *i;

      if (!shouldPrint(member, true))
        continue;

      if (!member->shouldPrintInContext(Options))
        continue;

      if (Options.EmptyLineBetweenMembers)
        Printer.printNewline();
      indent();
      visit(member);
      if (needComma && std::next(i) != iEnd)
        Printer << ",";
      Printer.printNewline();
    }
  }
  indent();
  if (closeBracket)
    Printer << "}";
}

void PrintAST::printNominalDeclGenericParams(NominalTypeDecl *decl) {
  if (decl->getGenericParams())
    if (auto GenericSig = decl->getGenericSignature())
      printGenericSignature(GenericSig, PrintParams | InnermostOnly);
}

void PrintAST::printNominalDeclGenericRequirements(NominalTypeDecl *decl) {
  if (decl->getGenericParams())
    if (auto GenericSig = decl->getGenericSignature())
      printGenericSignature(GenericSig, PrintRequirements | InnermostOnly);
}

void PrintAST::printInherited(const Decl *decl,
                              ArrayRef<TypeLoc> inherited,
                              ArrayRef<ProtocolDecl *> protos,
                              Type superclass,
                              bool explicitClass,
                              bool PrintAsProtocolComposition) {
  if (inherited.empty() && superclass.isNull() && !explicitClass) {
    if (protos.empty())
      return;
    // If only conforms to AnyObject protocol, nothing to print.
    if (protos.size() == 1) {
      if (protos.front()->isSpecificProtocol(KnownProtocolKind::AnyObject))
        return;
    }
  }

  if (inherited.empty()) {
    bool PrintedColon = false;
    bool PrintedInherited = false;

    if (explicitClass) {
      Printer << " : " << tok::kw_class;
      PrintedInherited = true;
    } else if (superclass) {
      bool ShouldPrintSuper = true;
      if (auto NTD = superclass->getAnyNominal()) {
        ShouldPrintSuper = shouldPrint(NTD);
      }
      if (ShouldPrintSuper) {
        Printer << " : ";
        superclass.print(Printer, Options);
        PrintedInherited = true;
      }
    }

    bool UseProtocolCompositionSyntax =
        PrintAsProtocolComposition && protos.size() > 1;
    if (UseProtocolCompositionSyntax) {
      Printer << " : " << tok::kw_protocol << "<";
      PrintedColon = true;
    }
    for (auto Proto : protos) {
      if (!shouldPrint(Proto))
        continue;
      if (Proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
        continue;
      if (auto Enum = dyn_cast<EnumDecl>(decl)) {
        // Conformance to RawRepresentable is implied by having a raw type.
        if (Enum->hasRawType()
            && Proto->isSpecificProtocol(KnownProtocolKind::RawRepresentable))
          continue;
        // Conformance to Equatable and Hashable is implied by being a "simple"
        // no-payload enum.
        if (Enum->hasOnlyCasesWithoutAssociatedValues()
            && (Proto->isSpecificProtocol(KnownProtocolKind::Equatable)
                || Proto->isSpecificProtocol(KnownProtocolKind::Hashable)))
          continue;
      }

      if (PrintedInherited)
        Printer << ", ";
      else if (!PrintedColon)
        Printer << " : ";
      Proto->getDeclaredType()->print(Printer, Options);
      PrintedInherited = true;
      PrintedColon = true;
    }
    if (UseProtocolCompositionSyntax)
      Printer << ">";
  } else {
    SmallVector<TypeLoc, 6> TypesToPrint;
    for (auto TL : inherited) {
      if (auto Ty = TL.getType()) {
        if (auto NTD = Ty->getAnyNominal())
          if (!shouldPrint(NTD))
            continue;
      }
      TypesToPrint.push_back(TL);
    }
    if (TypesToPrint.empty())
      return;

    Printer << " : ";

    if (explicitClass)
      Printer << " " << tok::kw_class << ", ";

    interleave(TypesToPrint, [&](TypeLoc TL) {
      printTypeLoc(TL);
    }, [&]() {
      Printer << ", ";
    });
  }
}

void PrintAST::printInherited(const NominalTypeDecl *decl,
                              bool explicitClass) {
  printInherited(decl, decl->getInherited(), { }, nullptr, explicitClass);
}

void PrintAST::printInherited(const EnumDecl *decl) {
  printInherited(decl, decl->getInherited(), { });
}

void PrintAST::printInherited(const ExtensionDecl *decl) {
  printInherited(decl, decl->getInherited(), { });
}

void PrintAST::printInherited(const GenericTypeParamDecl *D) {
  printInherited(D, D->getInherited(), { });
}

static void getModuleEntities(const clang::Module *ClangMod,
                              SmallVectorImpl<ModuleEntity> &ModuleEnts) {
  if (!ClangMod)
    return;

  getModuleEntities(ClangMod->Parent, ModuleEnts);
  ModuleEnts.push_back(ClangMod);
}

static void getModuleEntities(ImportDecl *Import,
                              SmallVectorImpl<ModuleEntity> &ModuleEnts) {
  if (auto *ClangMod = Import->getClangModule()) {
    getModuleEntities(ClangMod, ModuleEnts);
    return;
  }

  auto Mod = Import->getModule();
  if (!Mod)
    return;

  if (auto *ClangMod = Mod->findUnderlyingClangModule()) {
    getModuleEntities(ClangMod, ModuleEnts);
  } else {
    ModuleEnts.push_back(Mod);
  }
}

void PrintAST::visitImportDecl(ImportDecl *decl) {
  printAttributes(decl);
  Printer << tok::kw_import << " ";

  switch (decl->getImportKind()) {
  case ImportKind::Module:
    break;
  case ImportKind::Type:
    Printer << tok::kw_typealias << " ";
    break;
  case ImportKind::Struct:
    Printer << tok::kw_struct << " ";
    break;
  case ImportKind::Class:
    Printer << tok::kw_class << " ";
    break;
  case ImportKind::Enum:
    Printer << tok::kw_enum << " ";
    break;
  case ImportKind::Protocol:
    Printer << tok::kw_protocol << " ";
    break;
  case ImportKind::Var:
    Printer << tok::kw_var << " ";
    break;
  case ImportKind::Func:
    Printer << tok::kw_func << " ";
    break;
  }

  SmallVector<ModuleEntity, 4> ModuleEnts;
  getModuleEntities(decl, ModuleEnts);

  ArrayRef<ModuleEntity> Mods = ModuleEnts;
  interleave(decl->getFullAccessPath(),
             [&](const ImportDecl::AccessPathElement &Elem) {
               if (!Mods.empty()) {
                 Printer.printModuleRef(Mods.front(), Elem.first);
                 Mods = Mods.slice(1);
               } else {
                 Printer << Elem.first.str();
               }
             },
             [&] { Printer << "."; });
}

static void printExtendedTypeName(Type ExtendedType, ASTPrinter &Printer,
                                  PrintOptions Options) {
  auto Nominal = ExtendedType->getAnyNominal();
  assert(Nominal && "extension of non-nominal type");
  if (auto ct = ExtendedType->getAs<ClassType>()) {
    if (auto ParentType = ct->getParent()) {
      ParentType.print(Printer, Options);
      Printer << ".";
    }
  }
  if (auto st = ExtendedType->getAs<StructType>()) {
    if (auto ParentType = st->getParent()) {
      ParentType.print(Printer, Options);
      Printer << ".";
    }
  }

  // Respect alias type.
  if (isa<NameAliasType>(ExtendedType.getPointer())) {
    ExtendedType.print(Printer, Options);
    return;
  }

  Printer.printTypeRef(ExtendedType, Nominal, Nominal->getName());
}

void PrintAST::
printSynthesizedExtension(NominalTypeDecl* Decl, ExtensionDecl *ExtDecl) {
  if (Options.BracketOptions.shouldOpenExtension(ExtDecl)) {
    printDocumentationComment(ExtDecl);
    printAttributes(ExtDecl);
    Printer << tok::kw_extension << " ";

    printExtendedTypeName(Decl->getDeclaredType(), Printer, Options);
    printInherited(ExtDecl);

    if (ExtDecl->getGenericParams())
      if (auto *GenericSig = ExtDecl->getGenericSignature())
        printGenericSignature(GenericSig, PrintRequirements | InnermostOnly);
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(ExtDecl, false,
                       Options.BracketOptions.shouldOpenExtension(ExtDecl),
                       Options.BracketOptions.shouldCloseExtension(ExtDecl));
  }
}

void PrintAST::printExtension(ExtensionDecl *decl) {
  if (Options.BracketOptions.shouldOpenExtension(decl)) {
    printDocumentationComment(decl);
    printAttributes(decl);
    Printer << "extension ";
    recordDeclLoc(decl, [&]{
      // We cannot extend sugared types.
      Type extendedType = decl->getExtendedType();
      NominalTypeDecl *nominal = extendedType ? extendedType->getAnyNominal() : nullptr;
      if (!nominal) {
        // Fallback to TypeRepr.
        printTypeLoc(decl->getExtendedTypeLoc());
        return;
      }
      printExtendedTypeName(extendedType, Printer, Options);
    });
    printInherited(decl);

    if (decl->getGenericParams())
      if (auto *genericSig = decl->getGenericSignature()) {
        // For protocol extensions, don't print the 'Self : ...' requirement.
        unsigned flags = PrintRequirements | InnermostOnly;
        if (decl->getAsProtocolExtensionContext())
          flags |= SkipSelfRequirement;
        printGenericSignature(genericSig, flags);
      }
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false,
                       Options.BracketOptions.shouldOpenExtension(decl),
                       Options.BracketOptions.shouldCloseExtension(decl));
  }
}

void PrintAST::visitExtensionDecl(ExtensionDecl *decl) {
  if (Options.TransformContext &&
      Options.TransformContext->isPrintingSynthesizedExtension())
    printSynthesizedExtension(Options.TransformContext->getNominal(), decl);
  else
    printExtension(decl);
}

void PrintAST::visitPatternBindingDecl(PatternBindingDecl *decl) {
  // FIXME: We're not printing proper "{ get set }" annotations in pattern
  // binding decls.  As a hack, scan the decl to find out if any of the
  // variables are immutable, and if so, we print as 'let'.  This allows us to
  // handle the 'let x = 4' case properly at least.
  const VarDecl *anyVar = nullptr;
  for (auto entry : decl->getPatternList()) {
    entry.getPattern()->forEachVariable([&](VarDecl *V) {
      anyVar = V;
    });
    if (anyVar) break;
  }

  if (anyVar)
    printDocumentationComment(anyVar);
  if (decl->isStatic())
    printStaticKeyword(decl->getCorrectStaticSpelling());

  // FIXME: PatternBindingDecls don't have attributes themselves, so just assume
  // the variables all have the same attributes. This isn't exactly true
  // after type-checking, but it's close enough for now.
  if (anyVar) {
    printAttributes(anyVar);
    printAccessibility(anyVar);
    Printer << (anyVar->isSettable(anyVar->getDeclContext()) ? "var " : "let ");
  } else {
    Printer << "let ";
  }

  bool isFirst = true;
  for (auto entry : decl->getPatternList()) {
    if (!shouldPrintPattern(entry.getPattern()))
      continue;
    if (isFirst)
      isFirst = false;
    else
      Printer << ", ";

    printPattern(entry.getPattern());

    // We also try to print type for named patterns, e.g. var Field = 10;
    // and tuple patterns, e.g. var (T1, T2) = (10, 10)
    if (isa<NamedPattern>(entry.getPattern()) ||
        isa<TuplePattern>(entry.getPattern())) {
      printPatternType(entry.getPattern());
    }

    if (Options.VarInitializers) {
      // FIXME: Implement once we can pretty-print expressions.
    }
  }
}

void PrintAST::visitTopLevelCodeDecl(TopLevelCodeDecl *decl) {
  printASTNodes(decl->getBody()->getElements(), /*NeedIndent=*/false);
}

void PrintAST::visitIfConfigDecl(IfConfigDecl *ICD) {
  // FIXME: Pretty print #if decls
}

void PrintAST::visitTypeAliasDecl(TypeAliasDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << tok::kw_typealias << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    }, [&]{ // Signature
      if (decl->getGenericParams())
        if (auto *genericSig = decl->getGenericSignature())
          printGenericSignature(genericSig, PrintParams | InnermostOnly);
    });
  bool ShouldPrint = true;
  Type Ty = decl->getUnderlyingTypeLoc().getType();

  // If the underlying type is private, don't print it.
  if (Options.SkipPrivateStdlibDecls && Ty && Ty.isPrivateStdlibType())
    ShouldPrint = false;

  if (ShouldPrint) {
    Printer << " = ";
    printTypeLoc(decl->getUnderlyingTypeLoc());
  }
}

void PrintAST::visitGenericTypeParamDecl(GenericTypeParamDecl *decl) {
  recordDeclLoc(decl, [&] {
    Printer.printName(decl->getName(), PrintNameContext::GenericParameter);
  });

  printInherited(decl, decl->getInherited(), { });
}

void PrintAST::visitAssociatedTypeDecl(AssociatedTypeDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << tok::kw_associatedtype << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });

  printInherited(decl, decl->getInherited(), { });

  if (!decl->getDefaultDefinitionLoc().isNull()) {
    Printer << " = ";
    decl->getDefaultDefinitionLoc().getType().print(Printer, Options);
  }

  auto proto = decl->getProtocol();
  // As with protocol's trailing where clauses, use the requirement signature
  // when available.
  if (proto->isRequirementSignatureComputed()) {
    printWhereClauseFromRequirementSignature(proto, decl);
  } else {
    if (auto trailingWhere = decl->getTrailingWhereClause()) {
      printTrailingWhereClause(trailingWhere);
    }
  }
}

void PrintAST::visitEnumDecl(EnumDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords)
      Printer << tok::kw_enum << " ";
    recordDeclLoc(decl,
      [&]{
        Printer.printName(decl->getName());
      }, [&]{ // Signature
        printNominalDeclGenericParams(decl);
      });
    printInherited(decl);
    printNominalDeclGenericRequirements(decl);
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}

void PrintAST::visitStructDecl(StructDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords)
      Printer << tok::kw_struct << " ";
    recordDeclLoc(decl,
      [&]{
        Printer.printName(decl->getName());
      }, [&]{ // Signature
        printNominalDeclGenericParams(decl);
      });
    printInherited(decl);
    printNominalDeclGenericRequirements(decl);
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}

void PrintAST::visitClassDecl(ClassDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords)
      Printer << tok::kw_class << " ";
    recordDeclLoc(decl,
      [&]{
        Printer.printName(decl->getName());
      }, [&]{ // Signature
        printNominalDeclGenericParams(decl);
      });

    printInherited(decl);
    printNominalDeclGenericRequirements(decl);
  }

  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}

void PrintAST::visitProtocolDecl(ProtocolDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);

  if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
    ASTContext &Ctx = decl->getASTContext();
    printSourceRange(CharSourceRange(Ctx.SourceMgr, decl->getStartLoc(),
                              decl->getBraces().Start.getAdvancedLoc(-1)), Ctx);
  } else {
    if (!Options.SkipIntroducerKeywords)
      Printer << tok::kw_protocol << " ";
    recordDeclLoc(decl,
      [&]{
        Printer.printName(decl->getName());
      });

    // Figure out whether we need an explicit 'class' in the inheritance.
    bool explicitClass = false;
    if (decl->requiresClass() && !decl->isObjC()) {
      bool inheritsRequiresClass = false;
      for (auto proto : decl->getLocalProtocols(
                          ConformanceLookupKind::OnlyExplicit)) {
        if (proto->requiresClass()) {
          inheritsRequiresClass = true;
          break;
        }
      }

      if (!inheritsRequiresClass)
        explicitClass = true;
    }

    printInherited(decl, explicitClass);

    // The trailing where clause is a syntactic thing, which isn't serialized
    // (etc.) and thus isn't available for printing things out of
    // already-compiled SIL modules. The requirement signature is available in
    // such cases, so let's go with that when we can.
    if (decl->isRequirementSignatureComputed()) {
      printWhereClauseFromRequirementSignature(decl, decl);
    } else {
      if (auto trailingWhere = decl->getTrailingWhereClause()) {
        printTrailingWhereClause(trailingWhere);
      }
    }
  }
  if (Options.TypeDefinitions) {
    printMembersOfDecl(decl, false, true,
                       Options.BracketOptions.shouldCloseNominal(decl));
  }
}

static bool isStructOrClassContext(DeclContext *dc) {
  if (auto ctx = dc->getDeclaredTypeInContext())
    return ctx->getClassOrBoundGenericClass() ||
           ctx->getStructOrBoundGenericStruct();
  return false;
}

static void printParameterFlags(ASTPrinter &printer, PrintOptions options,
                                ParameterTypeFlags flags) {
  if (!options.excludeAttrKind(TAK_autoclosure) && flags.isAutoClosure())
    printer << "@autoclosure ";
  if (!options.excludeAttrKind(TAK_escaping) && flags.isEscaping())
    printer << "@escaping ";
}

void PrintAST::visitVarDecl(VarDecl *decl) {
  printDocumentationComment(decl);
  // Print @sil_stored when the attribute is not already
  // on, decl has storage and it is on a class.
  if (Options.PrintForSIL && decl->hasStorage() &&
      isStructOrClassContext(decl->getDeclContext()) &&
      !decl->getAttrs().hasAttribute<SILStoredAttr>())
    Printer << "@sil_stored ";
  printAttributes(decl);
  printAccessibility(decl);
  if (!Options.SkipIntroducerKeywords) {
    if (decl->isStatic())
      printStaticKeyword(decl->getCorrectStaticSpelling());
    Printer << (decl->isLet() ? tok::kw_let : tok::kw_var) << " ";
  }
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  if (decl->hasInterfaceType()) {
    Printer << ": ";
    auto tyLoc = decl->getTypeLoc();
    if (!tyLoc.getTypeRepr())
      tyLoc = TypeLoc::withoutLoc(decl->getInterfaceType());
    printTypeLoc(tyLoc);
  }

  printAccessors(decl);
}

void PrintAST::visitParamDecl(ParamDecl *decl) {
  visitVarDecl(decl);
}

void PrintAST::printOneParameter(const ParamDecl *param,
                                 ParameterTypeFlags paramFlags, bool Curried,
                                 bool ArgNameIsAPIByDefault) {
  Printer.callPrintStructurePre(PrintStructureKind::FunctionParameter, param);
  SWIFT_DEFER {
    Printer.printStructurePost(PrintStructureKind::FunctionParameter, param);
  };

  auto printArgName = [&]() {
    // Print argument name.
    auto ArgName = param->getArgumentName();
    auto BodyName = param->getName();
    switch (Options.ArgAndParamPrinting) {
    case PrintOptions::ArgAndParamPrintingMode::ArgumentOnly:
      Printer.printName(ArgName, PrintNameContext::FunctionParameterExternal);

      if (!ArgNameIsAPIByDefault && !ArgName.empty())
        Printer << " _";
      break;
    case PrintOptions::ArgAndParamPrintingMode::MatchSource:
      if (ArgName == BodyName && ArgNameIsAPIByDefault) {
        Printer.printName(ArgName, PrintNameContext::FunctionParameterExternal);
        break;
      }
      if (ArgName.empty() && !ArgNameIsAPIByDefault) {
        Printer.printName(BodyName, PrintNameContext::FunctionParameterLocal);
        break;
      }
      LLVM_FALLTHROUGH;
    case PrintOptions::ArgAndParamPrintingMode::BothAlways:
      Printer.printName(ArgName, PrintNameContext::FunctionParameterExternal);
      Printer << " ";
      Printer.printName(BodyName, PrintNameContext::FunctionParameterLocal);
      break;
    }
    Printer << ": ";
  };

  auto TheTypeLoc = param->getTypeLoc();

  printArgName();

  if (!TheTypeLoc.getTypeRepr() && param->hasInterfaceType())
    TheTypeLoc = TypeLoc::withoutLoc(param->getInterfaceType());

  // If the parameter is variadic, we will print the "..." after it, but we have
  // to strip off the added array type.
  if (param->isVariadic() && TheTypeLoc.getType()) {
    if (auto *BGT = TheTypeLoc.getType()->getAs<BoundGenericType>())
      TheTypeLoc.setType(BGT->getGenericArgs()[0]);
  }

  // FIXME: don't do if will be using type repr printing
  printParameterFlags(Printer, Options, paramFlags);

  // Special case, if we're not going to use the type repr printing, peek
  // through the paren types so that we don't print excessive @escapings.
  unsigned numParens = 0;
  if (!willUseTypeReprPrinting(TheTypeLoc, CurrentType, Options)) {
    while (auto parenTy =
                dyn_cast<ParenType>(TheTypeLoc.getType().getPointer())) {
      ++numParens;
      TheTypeLoc = TypeLoc::withoutLoc(parenTy->getUnderlyingType());
    }
  }

  for (unsigned i = 0; i < numParens; ++i)
    Printer << "(";
  printTypeLoc(TheTypeLoc);
  for (unsigned i = 0; i < numParens; ++i)
    Printer << ")";

  if (param->isVariadic())
    Printer << "...";

  if (param->isDefaultArgument()) {
    auto defaultArgStr
      = getDefaultArgumentSpelling(param->getDefaultArgumentKind());
    if (defaultArgStr.empty()) {
      if (Options.PrintDefaultParameterPlaceholder)
        Printer << " = " << tok::kw_default;
    } else {
      Printer << " = ";

      switch (param->getDefaultArgumentKind()) {
      case DefaultArgumentKind::File:
      case DefaultArgumentKind::Line:
      case DefaultArgumentKind::Column:
      case DefaultArgumentKind::Function:
      case DefaultArgumentKind::DSOHandle:
        Printer.printKeyword(defaultArgStr);
        break;
      default:
        Printer << defaultArgStr;
        break;
      }
    }
  }
}

void PrintAST::printParameterList(ParameterList *PL, Type paramListTy,
                                  bool isCurried,
                                  std::function<bool()> isAPINameByDefault) {
  SmallVector<ParameterTypeFlags, 4> paramFlags;
  if (paramListTy && !paramListTy->hasError()) {
    if (auto parenTy = dyn_cast<ParenType>(paramListTy.getPointer())) {
      paramFlags.push_back(parenTy->getParameterFlags());
    } else if (auto tupleTy = paramListTy->getAs<TupleType>()) {
      for (auto elt : tupleTy->getElements())
        paramFlags.push_back(elt.getParameterFlags());
    } else {
      paramFlags.push_back({});
    }
  } else {
    // Malformed AST, just use default flags
    paramFlags.resize(PL->size());
  }

  Printer << "(";
  for (unsigned i = 0, e = PL->size(); i != e; ++i) {
    if (i > 0)
      Printer << ", ";

    printOneParameter(PL->get(i), paramFlags[i], isCurried,
                      isAPINameByDefault());
  }
  Printer << ")";
}

void PrintAST::printFunctionParameters(AbstractFunctionDecl *AFD) {
  auto BodyParams = AFD->getParameterLists();
  auto curTy = AFD->hasInterfaceType() ? AFD->getInterfaceType() : nullptr;

  // Skip over the implicit 'self'.
  if (AFD->getImplicitSelfDecl()) {
    BodyParams = BodyParams.slice(1);
    if (curTy)
      if (auto funTy = curTy->getAs<AnyFunctionType>())
        curTy = funTy->getResult();
  }

  SmallVector<Type, 4> parameterListTypes;
  for (unsigned i = 0; i < BodyParams.size(); ++i) {
    if (curTy) {
      if (auto funTy = curTy->getAs<AnyFunctionType>()) {
        parameterListTypes.push_back(funTy->getInput());
        if (i < BodyParams.size() - 1)
          curTy = funTy->getResult();
      } else {
        parameterListTypes.push_back(curTy);
      }
    }
  }

  for (unsigned CurrPattern = 0, NumPatterns = BodyParams.size();
       CurrPattern != NumPatterns; ++CurrPattern) {
    // Be extra careful in the event of printing mal-formed ASTs
    auto paramListType = CurrPattern < parameterListTypes.size()
                             ? parameterListTypes[CurrPattern]
                             : nullptr;
    printParameterList(BodyParams[CurrPattern], paramListType,
                       /*isCurried=*/CurrPattern > 0,
                       [&]()->bool {
      return CurrPattern > 0 || AFD->argumentNameIsAPIByDefault();
    });
  }

  if (AFD->hasThrows()) {
    if (AFD->getAttrs().hasAttribute<RethrowsAttr>())
      Printer << " " << tok::kw_rethrows;
    else
      Printer << " " << tok::kw_throws;
  }
}

bool PrintAST::printASTNodes(const ArrayRef<ASTNode> &Elements,
                             bool NeedIndent) {
  IndentRAII IndentMore(*this, NeedIndent);
  bool PrintedSomething = false;
  for (auto element : Elements) {
    PrintedSomething = true;
    Printer.printNewline();
    indent();
    if (auto decl = element.dyn_cast<Decl*>()) {
      if (decl->shouldPrintInContext(Options))
        visit(decl);
    } else if (auto stmt = element.dyn_cast<Stmt*>()) {
      visit(stmt);
    } else {
      // FIXME: print expression
      // visit(element.get<Expr*>());
    }
  }
  return PrintedSomething;
}

void PrintAST::visitFuncDecl(FuncDecl *decl) {
  if (decl->isAccessor()) {
    printDocumentationComment(decl);
    printAttributes(decl);
    switch (auto kind = decl->getAccessorKind()) {
    case AccessorKind::NotAccessor: break;
    case AccessorKind::IsGetter:
    case AccessorKind::IsAddressor:
      recordDeclLoc(decl,
        [&]{
          Printer << (kind == AccessorKind::IsGetter
                        ? "get" : getAddressorLabel(decl));
        });
      Printer << " {";
      break;
    case AccessorKind::IsDidSet:
    case AccessorKind::IsMaterializeForSet:
    case AccessorKind::IsMutableAddressor:
      recordDeclLoc(decl,
        [&]{
          Printer << (kind == AccessorKind::IsDidSet ? "didSet" :
                      kind == AccessorKind::IsMaterializeForSet
                        ? "materializeForSet"
                        : getMutableAddressorLabel(decl));
        });
      Printer << " {";
      break;
    case AccessorKind::IsSetter:
    case AccessorKind::IsWillSet:
      recordDeclLoc(decl,
        [&]{
          Printer << (decl->isSetter() ? "set" : "willSet");

          auto params = decl->getParameterLists().back();
          if (params->size() != 0 && !params->get(0)->isImplicit()) {
            auto Name = params->get(0)->getName();
            if (!Name.empty()) {
              Printer << "(";
              Printer.printName(Name);
              Printer << ")";
            }
          }
        });
      Printer << " {";
    }
    if (auto BodyFunc = Options.FunctionBody) {
      {
        IndentRAII IndentBody(*this);
        indent();
        Printer.printNewline();
        Printer << BodyFunc(decl);
      }
      indent();
      Printer.printNewline();
    } else if (Options.FunctionDefinitions && decl->getBody()) {
      if (printASTNodes(decl->getBody()->getElements())) {
        Printer.printNewline();
        indent();
      }
    }
    Printer << "}";
  } else {
    printDocumentationComment(decl);
    printAttributes(decl);
    printAccessibility(decl);

    if (Options.PrintOriginalSourceText && decl->getStartLoc().isValid()) {
      ASTContext &Ctx = decl->getASTContext();
      SourceLoc StartLoc = decl->getStartLoc();
      SourceLoc EndLoc;
      if (!decl->getBodyResultTypeLoc().isNull()) {
        EndLoc = decl->getBodyResultTypeLoc().getSourceRange().End;
      } else {
        EndLoc = decl->getSignatureSourceRange().End;
      }
      CharSourceRange Range =
        Lexer::getCharSourceRangeFromSourceRange(Ctx.SourceMgr,
                                                 SourceRange(StartLoc, EndLoc));
      printSourceRange(Range, Ctx);
    } else {
      if (!Options.SkipIntroducerKeywords) {
        if (decl->isStatic())
          printStaticKeyword(decl->getCorrectStaticSpelling());
        if (decl->isMutating() && !decl->getAttrs().hasAttribute<MutatingAttr>()) {
          Printer.printKeyword("mutating");
          Printer << " ";
        }
        Printer << tok::kw_func << " ";
      }
      recordDeclLoc(decl,
        [&]{ // Name
          if (!decl->hasName())
            Printer << "<anonymous>";
          else
            Printer.printName(decl->getName());
        }, [&] { // Parameters
          if (decl->isGeneric())
            if (auto *genericSig = decl->getGenericSignature())
              printGenericSignature(genericSig, PrintParams | InnermostOnly);

          printFunctionParameters(decl);
        });

      Type ResultTy = decl->getResultInterfaceType();
      if (ResultTy && !ResultTy->isVoid()) {
        TypeLoc ResultTyLoc = decl->getBodyResultTypeLoc();
        if (!ResultTyLoc.getTypeRepr())
          ResultTyLoc = TypeLoc::withoutLoc(ResultTy);
        // FIXME: Hacky way to workaround the fact that 'Self' as return
        // TypeRepr is not getting 'typechecked'. See
        // \c resolveTopLevelIdentTypeComponent function in TypeCheckType.cpp.
        if (auto *simId = dyn_cast_or_null<SimpleIdentTypeRepr>(ResultTyLoc.getTypeRepr())) {
          if (simId->getIdentifier().str() == "Self")
            ResultTyLoc = TypeLoc::withoutLoc(ResultTy);
        }
        Printer << " -> ";
        Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
        printTypeLoc(ResultTyLoc);
        Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
      }
      if (decl->isGeneric())
        if (auto *genericSig = decl->getGenericSignature())
          printGenericSignature(genericSig, PrintRequirements | InnermostOnly);
    }

    if (auto BodyFunc = Options.FunctionBody) {
      Printer << " {";
      Printer.printNewline();
      {
        IndentRAII IndentBody(*this);
        indent();
        Printer << BodyFunc(decl);
      }
      indent();
      Printer.printNewline();
      Printer << "}";

    } else if (Options.FunctionDefinitions && decl->getBody()) {
      Printer << " ";
      visit(decl->getBody());
    }
  }
}

void PrintAST::printEnumElement(EnumElementDecl *elt) {
  recordDeclLoc(elt,
    [&]{
      Printer.printName(elt->getName());
    });

  if (auto argTy = elt->getArgumentInterfaceType()) {
    if (!Options.SkipPrivateStdlibDecls || !argTy.isPrivateStdlibType())
      argTy.print(Printer, Options);
  }

  auto *raw = elt->getRawValueExpr();
  if (!Options.EnumRawValues || !raw || raw->isImplicit())
    return;

  // Print the explicit raw value expression.
  Printer << " = ";
  switch (raw->getKind()) {
  case ExprKind::IntegerLiteral:
  case ExprKind::FloatLiteral: {
    auto *numLiteral = cast<NumberLiteralExpr>(raw);
    Printer.callPrintStructurePre(PrintStructureKind::NumberLiteral);
    if (numLiteral->isNegative())
      Printer << "-";
    Printer << numLiteral->getDigitsText();
    Printer.printStructurePost(PrintStructureKind::NumberLiteral);
    break;
  }
  case ExprKind::StringLiteral:
    Printer.callPrintStructurePre(PrintStructureKind::StringLiteral);
    Printer << "\"" << cast<StringLiteralExpr>(raw)->getValue() << "\"";
    Printer.printStructurePost(PrintStructureKind::StringLiteral);
    break;
  default:
    break; // Incorrect raw value; skip it for error recovery.
  }
}

void PrintAST::visitEnumCaseDecl(EnumCaseDecl *decl) {
  auto elems = decl->getElements();
  if (!elems.empty()) {
    // Documentation comments over the case are attached to the enum elements.
    printDocumentationComment(elems[0]);
  }
  printAttributes(decl);
  Printer << tok::kw_case << " ";

  interleave(elems.begin(), elems.end(),
    [&](EnumElementDecl *elt) {
      printEnumElement(elt);
    },
    [&] { Printer << ", "; });
}

void PrintAST::visitEnumElementDecl(EnumElementDecl *decl) {
  printDocumentationComment(decl);
  // In cases where there is no parent EnumCaseDecl (such as imported or
  // deserialized elements), print the element independently.
  printAttributes(decl);
  Printer << tok::kw_case << " ";
  printEnumElement(decl);
}

void PrintAST::visitSubscriptDecl(SubscriptDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);
  recordDeclLoc(decl, [&]{
    Printer << "subscript";
  }, [&] { // Parameters
    printParameterList(decl->getIndices(), decl->getIndicesInterfaceType(),
                       /*isCurried=*/false,
                       /*isAPINameByDefault*/[]()->bool{return false;});
  });
  Printer << " -> ";

  Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
  TypeLoc elementTy = decl->getElementTypeLoc();
  if (!elementTy.getTypeRepr())
    elementTy = TypeLoc::withoutLoc(decl->getElementInterfaceType());
  printTypeLoc(elementTy);
  Printer.printStructurePost(PrintStructureKind::FunctionReturnType);

  printAccessors(decl);
}

void PrintAST::visitConstructorDecl(ConstructorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printAccessibility(decl);

  if ((decl->getInitKind() == CtorInitializerKind::Convenience ||
       decl->getInitKind() == CtorInitializerKind::ConvenienceFactory) &&
      !decl->getAttrs().hasAttribute<ConvenienceAttr>()) {
    Printer.printKeyword("convenience");
    Printer << " ";
  } else if (decl->getInitKind() == CtorInitializerKind::Factory) {
      Printer << "/*not inherited*/ ";
  }

  recordDeclLoc(decl,
    [&]{
      Printer << "init";
    }, [&] { // Signature
      switch (decl->getFailability()) {
      case OTK_None:
        break;

      case OTK_Optional:
        Printer << "?";
        break;

      case OTK_ImplicitlyUnwrappedOptional:
        Printer << "!";
        break;
      }

      if (decl->isGeneric())
        if (auto *genericSig = decl->getGenericSignature())
          printGenericSignature(genericSig, PrintParams | InnermostOnly);

      printFunctionParameters(decl);
    });

  if (decl->isGeneric())
    if (auto *genericSig = decl->getGenericSignature())
      printGenericSignature(genericSig, PrintRequirements | InnermostOnly);

  if (auto BodyFunc = Options.FunctionBody) {
    Printer << " {";
    {
      Printer.printNewline();
      IndentRAII IndentBody(*this);
      indent();
      Printer << BodyFunc(decl);
    }
    indent();
    Printer.printNewline();
    Printer << "}";
  } else if (Options.FunctionDefinitions && decl->getBody()) {
    Printer << " ";
    visit(decl->getBody());
  }
}

void PrintAST::visitDestructorDecl(DestructorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  recordDeclLoc(decl,
    [&]{
      Printer << "deinit";
    });

  if (!Options.FunctionDefinitions || !decl->getBody()) {
    return;
  }

  Printer << " ";
  visit(decl->getBody());
}

void PrintAST::visitInfixOperatorDecl(InfixOperatorDecl *decl) {
  Printer.printKeyword("infix");
  Printer << " " << tok::kw_operator << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  if (!decl->getPrecedenceGroupName().empty()) {
    Printer << " : " << decl->getPrecedenceGroupName();
  }
}

void PrintAST::visitPrecedenceGroupDecl(PrecedenceGroupDecl *decl) {
  Printer << tok::kw_precedencegroup << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
  Printer << " {";
  Printer.printNewline();
  {
    IndentRAII indentMore(*this);
    if (!decl->isAssociativityImplicit() ||
        !decl->isNonAssociative()) {
      indent();
      Printer.printKeyword("associativity");
      Printer << ": ";
      switch (decl->getAssociativity()) {
      case Associativity::None:
        Printer.printKeyword("none");
        break;
      case Associativity::Left:
        Printer.printKeyword("left");
        break;
      case Associativity::Right:
        Printer.printKeyword("right");
        break;
      }
      Printer.printNewline();
    }
    if (!decl->isAssignmentImplicit() ||
        decl->isAssignment()) {
      indent();
      Printer.printKeyword("assignment");
      Printer << ": ";
      Printer.printKeyword(decl->isAssignment() ? "true" : "false");
      Printer.printNewline();
    }
    if (!decl->getHigherThan().empty()) {
      indent();
      Printer.printKeyword("higherThan");
      Printer << ": ";
      if (!decl->getHigherThan().empty()) {
        Printer << decl->getHigherThan()[0].Name;
        for (auto &rel : decl->getHigherThan().slice(1))
          Printer << ", " << rel.Name;
      }
      Printer.printNewline();
    }
    if (!decl->getLowerThan().empty()) {
      indent();
      Printer.printKeyword("lowerThan");
      Printer << ": ";
      if (!decl->getLowerThan().empty()) {
        Printer << decl->getLowerThan()[0].Name;
        for (auto &rel : decl->getLowerThan().slice(1))
          Printer << ", " << rel.Name;
      }
      Printer.printNewline();
    }
  }
  indent();
  Printer << "}";
}

void PrintAST::visitPrefixOperatorDecl(PrefixOperatorDecl *decl) {
  Printer.printKeyword("prefix");
  Printer << " " << tok::kw_operator << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
}

void PrintAST::visitPostfixOperatorDecl(PostfixOperatorDecl *decl) {
  Printer.printKeyword("postfix");
  Printer << " " << tok::kw_operator << " ";
  recordDeclLoc(decl,
    [&]{
      Printer.printName(decl->getName());
    });
}

void PrintAST::visitModuleDecl(ModuleDecl *decl) { }

void PrintAST::visitBraceStmt(BraceStmt *stmt) {
  Printer << "{";
  printASTNodes(stmt->getElements());
  Printer.printNewline();
  indent();
  Printer << "}";
}

void PrintAST::visitReturnStmt(ReturnStmt *stmt) {
  Printer << tok::kw_return;
  if (stmt->hasResult()) {
    Printer << " ";
    // FIXME: print expression.
  }
}

void PrintAST::visitThrowStmt(ThrowStmt *stmt) {
  Printer << tok::kw_throw << " ";
  // FIXME: print expression.
}

void PrintAST::visitDeferStmt(DeferStmt *stmt) {
  Printer << tok::kw_defer << " ";
  visit(stmt->getBodyAsWritten());
}

void PrintAST::visitIfStmt(IfStmt *stmt) {
  Printer << tok::kw_if << " ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getThenStmt());
  if (auto elseStmt = stmt->getElseStmt()) {
    Printer << " " << tok::kw_else << " ";
    visit(elseStmt);
  }
}
void PrintAST::visitGuardStmt(GuardStmt *stmt) {
  Printer << tok::kw_guard << " ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitIfConfigStmt(IfConfigStmt *stmt) {
  if (!Options.PrintIfConfig)
    return;

  for (auto &Clause : stmt->getClauses()) {
    if (&Clause == &*stmt->getClauses().begin())
      Printer << tok::pound_if << " "; // FIXME: print condition
    else if (Clause.Cond)
      Printer << tok::pound_elseif << ""; // FIXME: print condition
    else
      Printer << tok::pound_else;
    Printer.printNewline();
    if (printASTNodes(Clause.Elements)) {
      Printer.printNewline();
      indent();
    }
  }
  Printer.printNewline();
  Printer << tok::pound_endif;
}

void PrintAST::visitWhileStmt(WhileStmt *stmt) {
  Printer << tok::kw_while << " ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitRepeatWhileStmt(RepeatWhileStmt *stmt) {
  Printer << tok::kw_do << " ";
  visit(stmt->getBody());
  Printer << " " << tok::kw_while << " ";
  // FIXME: print condition
}

void PrintAST::visitDoStmt(DoStmt *stmt) {
  Printer << tok::kw_do << " ";
  visit(stmt->getBody());
}

void PrintAST::visitDoCatchStmt(DoCatchStmt *stmt) {
  Printer << tok::kw_do << " ";
  visit(stmt->getBody());
  for (auto clause : stmt->getCatches()) {
    visitCatchStmt(clause);
  }
}

void PrintAST::visitCatchStmt(CatchStmt *stmt) {
  Printer << tok::kw_catch << " ";
  printPattern(stmt->getErrorPattern());
  if (auto guard = stmt->getGuardExpr()) {
    Printer << " " << tok::kw_where << " ";
    // FIXME: print guard expression
    (void) guard;
  }
  Printer << ' ';
  visit(stmt->getBody());
}

void PrintAST::visitForStmt(ForStmt *stmt) {
  Printer << tok::kw_for << " (";
  // FIXME: print initializer
  Printer << "; ";
  if (stmt->getCond().isNonNull()) {
    // FIXME: print cond
  }
  Printer << "; ";
  // FIXME: print increment
  Printer << ") ";
  visit(stmt->getBody());
}

void PrintAST::visitForEachStmt(ForEachStmt *stmt) {
  Printer << tok::kw_for << " ";
  printPattern(stmt->getPattern());
  Printer << " " << tok::kw_in << " ";
  // FIXME: print container
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitBreakStmt(BreakStmt *stmt) {
  Printer << tok::kw_break;
}

void PrintAST::visitContinueStmt(ContinueStmt *stmt) {
  Printer << tok::kw_continue;
}

void PrintAST::visitFallthroughStmt(FallthroughStmt *stmt) {
  Printer << tok::kw_fallthrough;
}

void PrintAST::visitSwitchStmt(SwitchStmt *stmt) {
  Printer << tok::kw_switch << " ";
  // FIXME: print subject
  Printer << "{";
  Printer.printNewline();
  for (CaseStmt *C : stmt->getCases()) {
    visit(C);
  }
  Printer.printNewline();
  indent();
  Printer << "}";
}

void PrintAST::visitCaseStmt(CaseStmt *CS) {
  if (CS->isDefault()) {
    Printer << tok::kw_default;
  } else {
    auto PrintCaseLabelItem = [&](const CaseLabelItem &CLI) {
      if (auto *P = CLI.getPattern())
        printPattern(P);
      if (CLI.getGuardExpr()) {
        Printer << " " << tok::kw_where << " ";
        // FIXME: print guard expr
      }
    };
    Printer << tok::kw_case << " ";
    interleave(CS->getCaseLabelItems(), PrintCaseLabelItem,
               [&] { Printer << ", "; });
  }
  Printer << ":";
  Printer.printNewline();

  printASTNodes((cast<BraceStmt>(CS->getBody())->getElements()));
}

void PrintAST::visitFailStmt(FailStmt *stmt) {
  Printer << tok::kw_return << " " << tok::kw_nil;
}

void Decl::print(raw_ostream &os) const {
  PrintOptions options;
  options.FunctionDefinitions = true;
  options.TypeDefinitions = true;
  options.VarInitializers = true;
  // FIXME: Move all places where SIL printing is happening to explicit options.
  // For example, see \c ProjectionPath::print.
  options.PreferTypeRepr = false;

  print(os, options);
}

void Decl::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

bool Decl::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  PrintAST printer(Printer, Opts);
  return printer.visit(const_cast<Decl *>(this));
}

bool Decl::shouldPrintInContext(const PrintOptions &PO) const {
  // Skip getters/setters. They are part of the variable or subscript.
  if (isa<FuncDecl>(this) && cast<FuncDecl>(this)->isAccessor())
    return false;

  if (PO.ExplodePatternBindingDecls) {
    if (isa<VarDecl>(this))
      return true;
    if (isa<PatternBindingDecl>(this))
      return false;
  } else {
    // Try to preserve the PatternBindingDecl structure.

    // Skip stored variables, unless they came from a Clang module.
    // Stored variables in Swift source will be picked up by the
    // PatternBindingDecl.
    if (auto *VD = dyn_cast<VarDecl>(this)) {
      if (!VD->hasClangNode() && VD->hasStorage() &&
          VD->getStorageKind() != VarDecl::StoredWithObservers)
        return false;
    }

    // Skip pattern bindings that consist of just one computed variable.
    if (auto pbd = dyn_cast<PatternBindingDecl>(this)) {
      if (pbd->getPatternList().size() == 1) {
        auto pattern =
          pbd->getPatternList()[0].getPattern()->getSemanticsProvidingPattern();
        if (auto named = dyn_cast<NamedPattern>(pattern)) {
          auto StorageKind = named->getDecl()->getStorageKind();
          if (StorageKind == VarDecl::Computed ||
              StorageKind == VarDecl::StoredWithObservers)
            return false;
        }
      }
    }
  }

  if (isa<IfConfigDecl>(this)) {
    return PO.PrintIfConfig;
  }

  // Print everything else.
  return true;
}

void Pattern::print(llvm::raw_ostream &OS, const PrintOptions &Options) const {
  StreamPrinter StreamPrinter(OS);
  PrintAST Printer(StreamPrinter, Options);
  Printer.printPattern(this);
}

//===----------------------------------------------------------------------===//
//  Type Printing
//===----------------------------------------------------------------------===//

namespace {
class TypePrinter : public TypeVisitor<TypePrinter> {
  using super = TypeVisitor;

  ASTPrinter &Printer;
  const PrintOptions &Options;

  void printGenericArgs(ArrayRef<Type> Args) {
    if (Args.empty())
      return;

    Printer << "<";
    interleave(Args, [&](Type Arg) { visit(Arg); }, [&] { Printer << ", "; });
    Printer << ">";
  }

  static bool isSimple(Type type) {
    switch (type->getKind()) {
    case TypeKind::Function:
    case TypeKind::GenericFunction:
      return false;

    case TypeKind::Metatype:
    case TypeKind::ExistentialMetatype:
      return !cast<AnyMetatypeType>(type.getPointer())->hasRepresentation();

    case TypeKind::Archetype: {
      auto arch = type->getAs<ArchetypeType>();
      return !arch->isOpenedExistential();
    }

    case TypeKind::ProtocolComposition: {
      // 'Any' and single protocol compositions are simple
      auto composition = type->getAs<ProtocolCompositionType>();
      return composition->getProtocols().size() <= 1;
    }

    default:
      return true;
    }
  }

  /// Helper function for printing a type that is embedded within a larger type.
  ///
  /// This is necessary whenever the inner type may not normally be represented
  /// as a 'type-simple' production in the type grammar.
  void printWithParensIfNotSimple(Type T) {
    if (T.isNull()) {
      visit(T);
      return;
    }

    if (!isSimple(T)) {
      Printer << "(";
      visit(T);
      Printer << ")";
    } else {
      visit(T);
    }
  }

  template <typename T>
  void printModuleContext(T *Ty) {
    ModuleDecl *Mod = Ty->getDecl()->getModuleContext();
    Printer.printModuleRef(Mod, Mod->getName());
    Printer << ".";
  }

  template <typename T>
  void printTypeDeclName(T *Ty) {
    TypeDecl *TD = Ty->getDecl();
    Printer.printTypeRef(Ty, TD, TD->getName());
  }

  // FIXME: we should have a callback that would tell us
  // whether it's kosher to print a module name or not
  bool isLLDBExpressionModule(ModuleDecl *M) {
    if (!M)
      return false;
    return M->getName().str().startswith(LLDB_EXPRESSIONS_MODULE_NAME_PREFIX);
  }

  bool shouldPrintFullyQualified(TypeBase *T) {
    if (Options.FullyQualifiedTypes)
      return true;

    if (!Options.FullyQualifiedTypesIfAmbiguous)
      return false;

    Decl *D = T->getAnyGeneric();

    // If we cannot find the declaration, be extra careful and print
    // the type qualified.
    if (!D)
      return true;

    ModuleDecl *M = D->getDeclContext()->getParentModule();

    if (Options.CurrentModule && M == Options.CurrentModule) {
      return false;
    }

    // Don't print qualifiers for types from the standard library.
    if (M->isStdlibModule() ||
        M->getName() == M->getASTContext().Id_ObjectiveC ||
        M->isSystemModule() ||
        isLLDBExpressionModule(M))
      return false;

    // Don't print qualifiers for imported types.
    for (auto File : M->getFiles()) {
      if (File->getKind() == FileUnitKind::ClangModule)
        return false;
    }

    return true;
  }

public:
  TypePrinter(ASTPrinter &Printer, const PrintOptions &PO)
      : Printer(Printer), Options(PO) {}

  void visit(Type T) {
    Printer.printTypePre(TypeLoc::withoutLoc(T));
    SWIFT_DEFER { Printer.printTypePost(TypeLoc::withoutLoc(T)); };

    super::visit(T);
  }

  void visitErrorType(ErrorType *T) {
    if (auto originalType = T->getOriginalType())
      visit(originalType);
    else
      Printer << "<<error type>>";
  }

  void visitUnresolvedType(UnresolvedType *T) {
    if (T->getASTContext().LangOpts.DebugConstraintSolver)
      Printer << "<<unresolvedtype>>";
    else
      Printer << "_";
  }

  void visitBuiltinRawPointerType(BuiltinRawPointerType *T) {
    Printer << "Builtin.RawPointer";
  }

  void visitBuiltinNativeObjectType(BuiltinNativeObjectType *T) {
    Printer << "Builtin.NativeObject";
  }

  void visitBuiltinUnknownObjectType(BuiltinUnknownObjectType *T) {
    Printer << "Builtin.UnknownObject";
  }

  void visitBuiltinBridgeObjectType(BuiltinBridgeObjectType *T) {
    Printer << "Builtin.BridgeObject";
  }

  void visitBuiltinUnsafeValueBufferType(BuiltinUnsafeValueBufferType *T) {
    Printer << "Builtin.UnsafeValueBuffer";
  }

  void visitBuiltinVectorType(BuiltinVectorType *T) {
    llvm::SmallString<32> UnderlyingStrVec;
    StringRef UnderlyingStr;
    {
      // FIXME: Ugly hack: remove the .Builtin from the element type.
      {
        llvm::raw_svector_ostream UnderlyingOS(UnderlyingStrVec);
        T->getElementType().print(UnderlyingOS);
      }
      if (UnderlyingStrVec.startswith("Builtin."))
        UnderlyingStr = UnderlyingStrVec.substr(8);
      else
        UnderlyingStr = UnderlyingStrVec;
    }

    Printer << "Builtin.Vec" << T->getNumElements() << "x" << UnderlyingStr;
  }

  void visitBuiltinIntegerType(BuiltinIntegerType *T) {
    auto width = T->getWidth();
    if (width.isFixedWidth()) {
      Printer << "Builtin.Int" << width.getFixedWidth();
    } else if (width.isPointerWidth()) {
      Printer << "Builtin.Word";
    } else {
      llvm_unreachable("impossible bit width");
    }
  }

  void visitBuiltinFloatType(BuiltinFloatType *T) {
    switch (T->getFPKind()) {
    case BuiltinFloatType::IEEE16:  Printer << "Builtin.FPIEEE16"; return;
    case BuiltinFloatType::IEEE32:  Printer << "Builtin.FPIEEE32"; return;
    case BuiltinFloatType::IEEE64:  Printer << "Builtin.FPIEEE64"; return;
    case BuiltinFloatType::IEEE80:  Printer << "Builtin.FPIEEE80"; return;
    case BuiltinFloatType::IEEE128: Printer << "Builtin.FPIEEE128"; return;
    case BuiltinFloatType::PPC128:  Printer << "Builtin.FPPPC128"; return;
    }
  }

  void visitNameAliasType(NameAliasType *T) {
    if (Options.PrintForSIL || Options.PrintNameAliasUnderlyingType) {
      visit(T->getSinglyDesugaredType());
      return;
    }

    auto ParentDC = T->getDecl()->getDeclContext();
    auto ParentNominal = ParentDC ?
      ParentDC->getAsNominalTypeOrNominalTypeExtensionContext() : nullptr;

    if (ParentNominal) {
      visit(ParentNominal->getDeclaredType());
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitParenType(ParenType *T) {
    Printer << "(";
    printParameterFlags(Printer, Options, T->getParameterFlags());
    visit(T->getUnderlyingType());
    Printer << ")";
  }

  void visitTupleType(TupleType *T) {
    Printer.callPrintStructurePre(PrintStructureKind::TupleType);
    SWIFT_DEFER { Printer.printStructurePost(PrintStructureKind::TupleType); };

    Printer << "(";

    auto Fields = T->getElements();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      const TupleTypeElt &TD = Fields[i];
      Type EltType = TD.getType();

      Printer.callPrintStructurePre(PrintStructureKind::TupleElement);
      SWIFT_DEFER {
        Printer.printStructurePost(PrintStructureKind::TupleElement);
      };

      if (TD.hasName()) {
        Printer.printName(TD.getName(), PrintNameContext::TupleElement);
        Printer << ": ";
      }
      if (TD.isVararg()) {
        visit(TD.getVarargBaseTy());
        Printer << "...";
      } else {
        printParameterFlags(Printer, Options, TD.getParameterFlags());
        visit(EltType);
      }
    }
    Printer << ")";
  }

  void visitUnboundGenericType(UnboundGenericType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }
    printTypeDeclName(T);
  }

  void visitBoundGenericType(BoundGenericType *T) {
    if (Options.SynthesizeSugarOnTypes) {
      auto *NT = T->getDecl();
      auto &Ctx = T->getASTContext();
      if (NT == Ctx.getArrayDecl()) {
        Printer << "[";
        visit(T->getGenericArgs()[0]);
        Printer << "]";
        return;
      }
      if (NT == Ctx.getDictionaryDecl()) {
        Printer << "[";
        visit(T->getGenericArgs()[0]);
        Printer << " : ";
        visit(T->getGenericArgs()[1]);
        Printer << "]";
        return;
      }
      if (NT == Ctx.getOptionalDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "?";
        return;
      }
      if (NT == Ctx.getImplicitlyUnwrappedOptionalDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "!";
        return;
      }
    }
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
    printGenericArgs(T->getGenericArgs());
  }

  void visitParentType(Type T) {
    PrintOptions innerOptions = Options;
    innerOptions.SynthesizeSugarOnTypes = false;

    if (auto sugarType = dyn_cast<SyntaxSugarType>(T.getPointer()))
      T = sugarType->getImplementationType();
    else if (auto dictType = dyn_cast<DictionaryType>(T.getPointer()))
      T = dictType->getImplementationType();

    TypePrinter(Printer, innerOptions).visit(T);
  }

  void visitEnumType(EnumType *T) {
    if (auto ParentType = T->getParent()) {
      visitParentType(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitStructType(StructType *T) {
    if (auto ParentType = T->getParent()) {
      visitParentType(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitClassType(ClassType *T) {
    if (auto ParentType = T->getParent()) {
      visitParentType(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitAnyMetatypeType(AnyMetatypeType *T) {
    if (T->hasRepresentation()) {
      switch (T->getRepresentation()) {
      case MetatypeRepresentation::Thin:  Printer << "@thin ";  break;
      case MetatypeRepresentation::Thick: Printer << "@thick "; break;
      case MetatypeRepresentation::ObjC:  Printer << "@objc_metatype "; break;
      }
    }
    printWithParensIfNotSimple(T->getInstanceType());

    // We spell normal metatypes of existential types as .Protocol.
    if (isa<MetatypeType>(T) &&
        T->getInstanceType()->isAnyExistentialType()) {
      Printer << ".Protocol";
    } else {
      Printer << ".Type";
    }
  }

  void visitModuleType(ModuleType *T) {
    Printer << "module<";
    Printer.printModuleRef(T->getModule(), T->getModule()->getName());
    Printer << ">";
  }

  void visitDynamicSelfType(DynamicSelfType *T) {
    if (Options.PrintInSILBody) {
      Printer << "@dynamic_self ";
      visit(T->getSelfType());
      return;
    }

    // Try to print as a reference to the static type so that we will get a USR,
    // in cursor info.
    auto staticSelfT = T->getSelfType();

    if (auto *NTD = staticSelfT->getAnyNominal()) {
      if (isa<ClassDecl>(NTD)) {
        auto Name = T->getASTContext().Id_Self;
        Printer.printTypeRef(T, NTD, Name);
        return;
      }
    }

    visit(staticSelfT);
  }

  void printFunctionExtInfo(AnyFunctionType::ExtInfo info) {
    if (Options.SkipAttributes)
      return;


    if (Options.PrintFunctionRepresentationAttrs &&
        !Options.excludeAttrKind(TAK_convention) &&
        info.getSILRepresentation() != SILFunctionType::Representation::Thick) {
      Printer.callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
      Printer.printAttrName("@convention");
      Printer << "(";
      // TODO: coalesce into a single convention attribute.
      switch (info.getSILRepresentation()) {
      case SILFunctionType::Representation::Thick:
        llvm_unreachable("thick is not printed");
      case SILFunctionType::Representation::Thin:
        Printer << "thin";
        break;
      case SILFunctionType::Representation::Block:
        Printer << "block";
        break;
      case SILFunctionType::Representation::CFunctionPointer:
        Printer << "c";
        break;
      case SILFunctionType::Representation::Method:
        Printer << "method";
        break;
      case SILFunctionType::Representation::ObjCMethod:
        Printer << "objc_method";
        break;
      case SILFunctionType::Representation::WitnessMethod:
        Printer << "witness_method";
        break;
      case SILFunctionType::Representation::Closure:
        Printer << "closure";
        break;
      }
      Printer << ")";
      Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
      Printer << " ";
    }
  }

  void printFunctionExtInfo(SILFunctionType::ExtInfo info) {
    if (Options.SkipAttributes)
      return;

    if (Options.PrintFunctionRepresentationAttrs &&
        !Options.excludeAttrKind(TAK_convention) &&
        info.getRepresentation() != SILFunctionType::Representation::Thick) {
      Printer.callPrintStructurePre(PrintStructureKind::BuiltinAttribute);
      Printer.printAttrName("@convention");
      Printer << "(";
      switch (info.getRepresentation()) {
      case SILFunctionType::Representation::Thick:
        llvm_unreachable("thick is not printed");
      case SILFunctionType::Representation::Thin:
        Printer << "thin";
        break;
      case SILFunctionType::Representation::Block:
        Printer << "block";
        break;
      case SILFunctionType::Representation::CFunctionPointer:
        Printer << "c";
        break;
      case SILFunctionType::Representation::Method:
        Printer << "method";
        break;
      case SILFunctionType::Representation::ObjCMethod:
        Printer << "objc_method";
        break;
      case SILFunctionType::Representation::WitnessMethod:
        Printer << "witness_method";
        break;
      case SILFunctionType::Representation::Closure:
        Printer << "closure";
        break;
      }
      Printer << ")";
      Printer.printStructurePost(PrintStructureKind::BuiltinAttribute);
      Printer << " ";
    }

    if (info.isPseudogeneric()) {
      Printer.printSimpleAttr("@pseudogeneric") << " ";
    }
  }

  void visitFunctionType(FunctionType *T) {
    Printer.callPrintStructurePre(PrintStructureKind::FunctionType);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::FunctionType);
    };

    printFunctionExtInfo(T->getExtInfo());
    
    // If we're stripping argument labels from types, do it when printing.
    Type inputType = T->getInput();
    if (auto tupleTy = dyn_cast<TupleType>(inputType.getPointer())) {
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(tupleTy->getNumElements());
      for (const auto &elt : tupleTy->getElements())
        elements.push_back(elt.getWithoutName());
      inputType = TupleType::get(elements, inputType->getASTContext());
    }

    bool needsParens =
      !isa<ParenType>(inputType.getPointer()) &&
      !inputType->is<TupleType>();
    
    if (needsParens)
      Printer << "(";

    visit(inputType);
    
    if (needsParens)
      Printer << ")";
    
    if (T->throws())
      Printer << " " << tok::kw_throws;

    Printer << " -> ";

    Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
    T->getResult().print(Printer, Options);
    Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  }

  void printGenericSignature(const GenericSignature *genericSig,
                             unsigned flags) {
    PrintAST(Printer, Options).printGenericSignature(genericSig, flags);
  }

  void visitGenericFunctionType(GenericFunctionType *T) {
    Printer.callPrintStructurePre(PrintStructureKind::FunctionType);
    SWIFT_DEFER {
      Printer.printStructurePost(PrintStructureKind::FunctionType);
    };

    printFunctionExtInfo(T->getExtInfo());
    printGenericSignature(T->getGenericSignature(),
                          PrintAST::PrintParams |
                          PrintAST::PrintRequirements);
    Printer << " ";

    bool needsParens =
      !isa<ParenType>(T->getInput().getPointer()) &&
      !T->getInput()->is<TupleType>();
      
    if (needsParens)
      Printer << "(";

    visit(T->getInput());

    if (needsParens)
      Printer << ")";

    if (T->throws())
      Printer << " " << tok::kw_throws;

    Printer << " -> ";
    Printer.callPrintStructurePre(PrintStructureKind::FunctionReturnType);
    T->getResult().print(Printer, Options);
    Printer.printStructurePost(PrintStructureKind::FunctionReturnType);
  }

  void printCalleeConvention(ParameterConvention conv) {
    switch (conv) {
    case ParameterConvention::Direct_Unowned:
      return;
    case ParameterConvention::Direct_Owned:
      Printer << "@callee_owned ";
      return;
    case ParameterConvention::Direct_Guaranteed:
      Printer << "@callee_guaranteed ";
      return;
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_InoutAliasable:
    case ParameterConvention::Indirect_In_Guaranteed:
      llvm_unreachable("callee convention cannot be indirect");
    }
    llvm_unreachable("bad convention");
  }

  void visitSILFunctionType(SILFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printCalleeConvention(T->getCalleeConvention());
    if (auto sig = T->getGenericSignature()) {
      printGenericSignature(sig,
                            PrintAST::PrintParams |
                            PrintAST::PrintRequirements);
      Printer << " ";
    }

    Printer << "(";
    bool first = true;
    for (auto param : T->getParameters()) {
      Printer.printSeparator(first, ", ");
      param.print(Printer, Options);
    }
    Printer << ") -> ";

    unsigned totalResults = T->getNumResults() + unsigned(T->hasErrorResult());

    if (totalResults != 1) Printer << "(";

    first = true;
    for (auto result : T->getResults()) {
      Printer.printSeparator(first, ", ");
      result.print(Printer, Options);
    }

    if (T->hasErrorResult()) {
      // The error result is implicitly @owned; don't print that.
      assert(T->getErrorResult().getConvention() == ResultConvention::Owned);
      Printer.printSeparator(first, ", ");
      Printer << "@error ";
      T->getErrorResult().getType().print(Printer, Options);
    }

    if (totalResults != 1) Printer << ")";
  }

  void visitSILBlockStorageType(SILBlockStorageType *T) {
    Printer << "@block_storage ";
    printWithParensIfNotSimple(T->getCaptureType());
  }

  void visitSILBoxType(SILBoxType *T) {
    {
      // A box layout has its own independent generic environment. Don't try
      // to print it with the environment's generic params.
      PrintOptions subOptions = Options;
      subOptions.GenericEnv = nullptr;
      TypePrinter sub(Printer, subOptions);
      
      // Capture list used here to ensure we don't print anything using `this`
      // printer, but only the sub-Printer.
      [&sub, T]{
        if (auto sig = T->getLayout()->getGenericSignature()) {
          sub.printGenericSignature(sig,
                            PrintAST::PrintParams | PrintAST::PrintRequirements);
          sub.Printer << " ";
        }
        sub.Printer << "{";
        interleave(T->getLayout()->getFields(),
                   [&](const SILField &field) {
                     sub.Printer <<
                       (field.isMutable() ? " var " : " let ");
                     sub.visit(field.getLoweredType());
                   },
                   [&]{
                     sub.Printer << ",";
                   });
        sub.Printer << " }";
      }();
    }
    
    // The arguments to the layout, if any, do come from the outer environment.
    if (!T->getGenericArgs().empty()) {
      Printer << " <";
      interleave(T->getGenericArgs(),
                 [&](const Substitution &arg) {
                   visit(arg.getReplacement());
                 }, [&]{
                   Printer << ", ";
                 });
      Printer << ">";
    }
  }

  void visitArraySliceType(ArraySliceType *T) {
    Printer << "[";
    visit(T->getBaseType());
    Printer << "]";
  }

  void visitDictionaryType(DictionaryType *T) {
    Printer << "[";
    visit(T->getKeyType());
    Printer << " : ";
    visit(T->getValueType());
    Printer << "]";
  }

  void visitOptionalType(OptionalType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    Printer << "?";
  }

  void visitImplicitlyUnwrappedOptionalType(ImplicitlyUnwrappedOptionalType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    Printer <<  "!";
  }

  void visitProtocolType(ProtocolType *T) {
    printTypeDeclName(T);
  }

  void visitProtocolCompositionType(ProtocolCompositionType *T) {
    if (T->getProtocols().empty()) {
      Printer << "Any";
    } else {
      interleave(T->getProtocols(), [&](Type Proto) { visit(Proto); },
                 [&] { Printer << " & "; });
    }
  }

  void visitLValueType(LValueType *T) {
    Printer << "@lvalue ";
    visit(T->getObjectType());
  }

  void visitInOutType(InOutType *T) {
    Printer << tok::kw_inout << " ";
    visit(T->getObjectType());
  }

  void visitArchetypeType(ArchetypeType *T) {
    if (auto existentialTy = T->getOpenedExistentialType()) {
      if (Options.PrintForSIL)
        Printer << "@opened(\"" << T->getOpenedExistentialID() << "\") ";
      visit(existentialTy);
    } else {
      if (auto parent = T->getParent()) {
        visit(parent);
        Printer << ".";
      }

      if (Options.AlternativeTypeNames) {
        auto found = Options.AlternativeTypeNames->find(T->getCanonicalType());
        if (found != Options.AlternativeTypeNames->end()) {
          Printer << found->second.str();
          return;
        }
      }

      auto Name = T->getName();
      if (Name.empty())
        Printer << "<anonymous>";
      else {
        PrintNameContext context = PrintNameContext::Normal;
        if (Name == T->getASTContext().Id_Self)
          context = PrintNameContext::GenericParameter;
        Printer.printName(Name, context);
      }
    }
  }

  void visitGenericTypeParamType(GenericTypeParamType *T) {
    if (T->getDecl() == nullptr) {
      // If we have an alternate name for this type, use it.
      if (Options.AlternativeTypeNames) {
        auto found = Options.AlternativeTypeNames->find(T->getCanonicalType());
        if (found != Options.AlternativeTypeNames->end()) {
          Printer << found->second.str();
          return;
        }
      }

      // When printing SIL types, use a generic environment to map them from
      // canonical types to sugared types.
      if (Options.GenericEnv)
        T = Options.GenericEnv->getSugaredType(T);
    }

    auto Name = T->getName();
    if (Name.empty())
      Printer << "<anonymous>";
    else {
      if (T->getDecl() &&
          T->getDecl()->getDeclContext()->getAsProtocolOrProtocolExtensionContext()) {
        Printer.printTypeRef(T, T->getDecl(), Name);
        return;
      }

      PrintNameContext context = PrintNameContext::Normal;
      if (Name == T->getASTContext().Id_Self)
        context = PrintNameContext::GenericParameter;
      Printer.printName(Name, context);
    }
  }

  void visitDependentMemberType(DependentMemberType *T) {
    visitParentType(T->getBase());
    Printer << ".";
    Printer.printName(T->getName());
  }

  void visitUnownedStorageType(UnownedStorageType *T) {
    if (Options.PrintStorageRepresentationAttrs)
      Printer << "@sil_unowned ";
    visit(T->getReferentType());
  }

  void visitUnmanagedStorageType(UnmanagedStorageType *T) {
    if (Options.PrintStorageRepresentationAttrs)
      Printer << "@sil_unmanaged ";
    visit(T->getReferentType());
  }

  void visitWeakStorageType(WeakStorageType *T) {
    if (Options.PrintStorageRepresentationAttrs)
      Printer << "@sil_weak ";
    visit(T->getReferentType());
  }

  void visitTypeVariableType(TypeVariableType *T) {
    if (T->getASTContext().LangOpts.DebugConstraintSolver) {
      Printer << "$T" << T->getID();
      return;
    }

    Printer << "_";
  }
};
} // unnamed namespace

void Type::print(raw_ostream &OS, const PrintOptions &PO) const {
  StreamPrinter Printer(OS);
  print(Printer, PO);
}
void Type::print(ASTPrinter &Printer, const PrintOptions &PO) const {
  if (isNull())
    Printer << "<null>";
  else
    TypePrinter(Printer, PO).visit(*this);
}

void LayoutConstraintInfo::print(raw_ostream &OS,
                                 const PrintOptions &PO) const {
  StreamPrinter Printer(OS);
  print(Printer, PO);
}

void LayoutConstraint::print(raw_ostream &OS,
                             const PrintOptions &PO) const {
  assert(*this);
  getPointer()->print(OS, PO);
}

void LayoutConstraintInfo::print(ASTPrinter &Printer,
                                 const PrintOptions &PO) const {
  Printer << getName();
  switch (getKind()) {
  case LayoutConstraintKind::UnknownLayout:
  case LayoutConstraintKind::RefCountedObject:
  case LayoutConstraintKind::NativeRefCountedObject:
  case LayoutConstraintKind::Class:
  case LayoutConstraintKind::NativeClass:
  case LayoutConstraintKind::Trivial:
    return;
  case LayoutConstraintKind::TrivialOfAtMostSize:
  case LayoutConstraintKind::TrivialOfExactSize:
    Printer << "(";
    Printer << SizeInBits;
    if (Alignment)
      Printer << ", " << Alignment <<")";
    Printer << ")";
    break;
  }
}

void GenericSignature::print(raw_ostream &OS) const {
  StreamPrinter Printer(OS);
  PrintAST(Printer, PrintOptions())
      .printGenericSignature(this,
                             PrintAST::PrintParams |
                             PrintAST::PrintRequirements);
}
void GenericSignature::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Requirement::dump() const {
  switch (getKind()) {
  case RequirementKind::Conformance:
    llvm::errs() << "conforms_to: ";
    break;
  case RequirementKind::Layout:
    llvm::errs() << "layout: ";
    break;
  case RequirementKind::Superclass:
    llvm::errs() << "superclass: ";
    break;
  case RequirementKind::SameType:
    llvm::errs() << "same_type: ";
    break;
  }

  if (getFirstType()) llvm::errs() << getFirstType() << " ";
  if (getKind() != RequirementKind::Layout && getSecondType())
    llvm::errs() << getSecondType();
  else if (getLayoutConstraint())
    llvm::errs() << getLayoutConstraint();
  llvm::errs() << "\n";
}

void Requirement::print(raw_ostream &os, const PrintOptions &opts) const {
  StreamPrinter printer(os);
  PrintAST(printer, opts).printRequirement(*this);
}

void Requirement::print(ASTPrinter &printer, const PrintOptions &opts) const {
  PrintAST(printer, opts).printRequirement(*this);
}

std::string GenericSignature::getAsString() const {
  std::string result;
  llvm::raw_string_ostream out(result);
  print(out);
  return out.str();
}

static StringRef getStringForParameterConvention(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In: return "@in ";
  case ParameterConvention::Indirect_In_Guaranteed:  return "@in_guaranteed ";
  case ParameterConvention::Indirect_Inout: return "@inout ";
  case ParameterConvention::Indirect_InoutAliasable: return "@inout_aliasable ";
  case ParameterConvention::Direct_Owned: return "@owned ";
  case ParameterConvention::Direct_Unowned: return "";
  case ParameterConvention::Direct_Guaranteed: return "@guaranteed ";
  }
  llvm_unreachable("bad parameter convention");
}

StringRef swift::getCheckedCastKindName(CheckedCastKind kind) {
  switch (kind) {
  case CheckedCastKind::Unresolved:
    return "unresolved";
  case CheckedCastKind::Coercion:
    return "coercion";
  case CheckedCastKind::ValueCast:
    return "value_cast";
  case CheckedCastKind::ArrayDowncast:
    return "array_downcast";
  case CheckedCastKind::DictionaryDowncast:
    return "dictionary_downcast";
  case CheckedCastKind::SetDowncast:
    return "set_downcast";
  case CheckedCastKind::BridgingCoercion:
    return "bridging_coercion";
  case CheckedCastKind::Swift3BridgingDowncast:
    return "bridging_downcast";
  }
  llvm_unreachable("bad checked cast name");
}

void SILParameterInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void SILParameterInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}
void SILParameterInfo::print(ASTPrinter &Printer,
                             const PrintOptions &Opts) const {
  Printer << getStringForParameterConvention(getConvention());
  getType().print(Printer, Opts);
}

static StringRef getStringForResultConvention(ResultConvention conv) {
  switch (conv) {
  case ResultConvention::Indirect: return "@out ";
  case ResultConvention::Owned: return "@owned ";
  case ResultConvention::Unowned: return "";
  case ResultConvention::UnownedInnerPointer: return "@unowned_inner_pointer ";
  case ResultConvention::Autoreleased: return "@autoreleased ";
  }
  llvm_unreachable("bad result convention");
}

void SILResultInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void SILResultInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}
void SILResultInfo::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  Printer << getStringForResultConvention(getConvention());
  getType().print(Printer, Opts);
}

std::string Type::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

std::string TypeBase::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

void TypeBase::dumpPrint() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void TypeBase::print(raw_ostream &OS, const PrintOptions &PO) const {
  Type(const_cast<TypeBase *>(this)).print(OS, PO);
}
void TypeBase::print(ASTPrinter &Printer, const PrintOptions &PO) const {
  Type(const_cast<TypeBase *>(this)).print(Printer, PO);
}

std::string LayoutConstraint::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

std::string LayoutConstraintInfo::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

void ProtocolConformance::printName(llvm::raw_ostream &os,
                                    const PrintOptions &PO) const {
  if (getKind() == ProtocolConformanceKind::Normal) {
    if (auto genericSig = getGenericSignature()) {
      StreamPrinter sPrinter(os);
      TypePrinter typePrinter(sPrinter, PO);
      typePrinter
          .printGenericSignature(genericSig,
                                 PrintAST::PrintParams |
                                 PrintAST::PrintRequirements);
      os << ' ';
    }
  }

  getType()->print(os, PO);
  os << ": ";

  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto normal = cast<NormalProtocolConformance>(this);
    os << normal->getProtocol()->getName()
       << " module " << normal->getDeclContext()->getParentModule()->getName();
    break;
  }
  case ProtocolConformanceKind::Specialized: {
    auto spec = cast<SpecializedProtocolConformance>(this);
    os << "specialize <";
    interleave(spec->getGenericSubstitutions(),
               [&](const Substitution &s) { s.print(os, PO); },
               [&] { os << ", "; });
    os << "> (";
    spec->getGenericConformance()->printName(os, PO);
    os << ")";
    break;
  }
  case ProtocolConformanceKind::Inherited: {
    auto inherited = cast<InheritedProtocolConformance>(this);
    os << "inherit (";
    inherited->getInheritedConformance()->printName(os, PO);
    os << ")";
    break;
  }
  }
}

void Substitution::print(llvm::raw_ostream &os,
                         const PrintOptions &PO) const {
  Replacement->print(os, PO);
}
