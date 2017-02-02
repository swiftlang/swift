//===--- SwiftDocSupport.cpp ----------------------------------------------===//
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

#include "SwiftASTManager.h"
#include "SwiftEditorDiagConsumer.h"
#include "SwiftLangSupport.h"
#include "SourceKit/Support/UIdent.h"

#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/SourceEntityWalker.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/IDE/ModuleInterfacePrinting.h"
#include "swift/IDE/SyntaxModel.h"
// This is included only for createLazyResolver(). Move to different header ?
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Config.h"

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"

using namespace SourceKit;
using namespace swift;
using namespace ide;

static ModuleDecl *getModuleByFullName(ASTContext &Ctx, StringRef ModuleName) {
  SmallVector<std::pair<Identifier, SourceLoc>, 4>
      AccessPath;
  while (!ModuleName.empty()) {
    StringRef SubModuleName;
    std::tie(SubModuleName, ModuleName) = ModuleName.split('.');
    AccessPath.push_back({ Ctx.getIdentifier(SubModuleName), SourceLoc() });
  }
  return Ctx.getModule(AccessPath);
}

static ModuleDecl *getModuleByFullName(ASTContext &Ctx, Identifier ModuleName) {
  return Ctx.getModule(std::make_pair(ModuleName, SourceLoc()));
}

namespace {
struct TextRange {
  unsigned Offset;
  unsigned Length;
};

struct TextEntity {
  const Decl *Dcl = nullptr;
  const Decl *SynthesizeTarget = nullptr;
  const Decl *DefaultImplementationOf = nullptr;
  StringRef Argument;
  TextRange Range;
  unsigned LocOffset = 0;
  std::vector<TextEntity> SubEntities;
  const bool IsSynthesizedExtension;

  TextEntity(const Decl *D, const Decl *SynthesizeTarget,
             const Decl* DefaultImplementationOf,
             unsigned StartOffset, bool IsSynthesizedExtension)
    : Dcl(D), SynthesizeTarget(SynthesizeTarget),
      DefaultImplementationOf(DefaultImplementationOf), Range{StartOffset, 0},
      IsSynthesizedExtension(IsSynthesizedExtension) {}

  TextEntity(const Decl *D, const Decl *SynthesizeTarget,
             const Decl* DefaultImplementationOf, TextRange TR,
             unsigned LocOffset, bool IsSynthesizedExtension) : Dcl(D),
             DefaultImplementationOf(DefaultImplementationOf), Range(TR),
              LocOffset(LocOffset),
              IsSynthesizedExtension(IsSynthesizedExtension) {}

  TextEntity(const Decl *D, const Decl *SynthesizeTarget,
             const Decl* DefaultImplementationOf, StringRef Arg,
             TextRange TR, unsigned LocOffset, bool IsSynthesizedExtension)
    : Dcl(D), SynthesizeTarget(SynthesizeTarget),
      DefaultImplementationOf(DefaultImplementationOf), Argument(Arg), Range(TR),
      LocOffset(LocOffset), IsSynthesizedExtension(IsSynthesizedExtension) {}
};

struct TextReference {
  const ValueDecl *Dcl = nullptr;
  TextRange Range;
  const Type Ty;

  TextReference(const ValueDecl *D, unsigned Offset, unsigned Length,
                const Type Ty = Type()) : Dcl(D), Range{Offset, Length}, Ty(Ty) {}
};

class AnnotatingPrinter : public StreamPrinter {

  std::pair<const ExtensionDecl *, const NominalTypeDecl *>
    SynthesizedExtensionInfo = {nullptr, nullptr};

  typedef llvm::SmallDenseMap<ValueDecl*, ValueDecl*> DefaultImplementMap;
  llvm::SmallDenseMap<ProtocolDecl*, DefaultImplementMap> AllDefaultMaps;
  DefaultImplementMap *DefaultMapToUse = nullptr;

  void initDefaultMapToUse(const Decl *D) {
    const ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D);
    if (!ED)
      return;
    if (ED->getExtendedType()) {
      if (auto NTD = ED->getExtendedType()->getAnyNominal()) {
        if (ProtocolDecl *PD = dyn_cast<ProtocolDecl>(NTD)) {
          auto Pair = AllDefaultMaps.insert({PD, DefaultImplementMap()});
          DefaultMapToUse = &Pair.first->getSecond();
          if (Pair.second) {
            swift::collectDefaultImplementationForProtocolMembers(PD,
                                                      Pair.first->getSecond());
          }
        }
      }
    }
  }

  void deinitDefaultMapToUse(const Decl*D) {
    if (isa<ExtensionDecl>(D)) {
      DefaultMapToUse = nullptr;
    }
  }

  ValueDecl *getDefaultImplementation(const Decl *D) {
    if (!DefaultMapToUse)
      return nullptr;
    ValueDecl *VD = const_cast<ValueDecl*>(dyn_cast<ValueDecl>(D));
    auto Found = DefaultMapToUse->find(VD);
    if (Found != DefaultMapToUse->end()) {
      return Found->second;
    }
    return nullptr;
  }

public:
  std::vector<TextEntity> TopEntities;
  std::vector<TextEntity> EntitiesStack;
  std::vector<TextReference> References;

  using StreamPrinter::StreamPrinter;

  ~AnnotatingPrinter() {
    assert(EntitiesStack.empty());
  }

  bool shouldContinuePre(const Decl *D, Optional<BracketOptions> Bracket) {
    assert(Bracket.hasValue());
    if (!Bracket.getValue().shouldOpenExtension(D) &&
        isa<ExtensionDecl>(D))
      return false;
    return true;
  }

  bool shouldContinuePost(const Decl *D, Optional<BracketOptions> Bracket) {
    assert(Bracket.hasValue());
    if (!Bracket.getValue().shouldCloseNominal(D) && dyn_cast<NominalTypeDecl>(D))
      return false;
    if (!Bracket.getValue().shouldCloseExtension(D) &&
        isa<ExtensionDecl>(D))
      return false;
    return true;
  }

  void printSynthesizedExtensionPre(const ExtensionDecl *ED,
                                    const NominalTypeDecl *NTD,
                                    Optional<BracketOptions> Bracket) override {
    assert(!SynthesizedExtensionInfo.first);
    SynthesizedExtensionInfo = {ED, NTD};
    if (!shouldContinuePre(ED, Bracket))
      return;
    unsigned StartOffset = OS.tell();
    EntitiesStack.emplace_back(ED, SynthesizedExtensionInfo.second, nullptr,
                               StartOffset, true);
  }

  void printSynthesizedExtensionPost(const ExtensionDecl *ED,
                                     const NominalTypeDecl *NTD,
                                     Optional<BracketOptions> Bracket) override {
    assert(SynthesizedExtensionInfo.first);
    SynthesizedExtensionInfo = {nullptr, nullptr};
    if (!shouldContinuePost(ED, Bracket))
      return;
    TextEntity Entity = std::move(EntitiesStack.back());
    EntitiesStack.pop_back();
    unsigned EndOffset = OS.tell();
    Entity.Range.Length = EndOffset - Entity.Range.Offset;
    TopEntities.push_back(std::move(Entity));
  }

  void printDeclPre(const Decl *D, Optional<BracketOptions> Bracket) override {
    if (isa<ParamDecl>(D))
      return; // Parameters are handled specially in addParameters().
    if (!shouldContinuePre(D, Bracket))
      return;
    unsigned StartOffset = OS.tell();
    initDefaultMapToUse(D);
    const NominalTypeDecl *SynthesizedTarget = nullptr;
    // If D is declared in the extension, then the synthesized target is valid.
    if (D->getDeclContext() == SynthesizedExtensionInfo.first)
      SynthesizedTarget = SynthesizedExtensionInfo.second;
    EntitiesStack.emplace_back(D, SynthesizedTarget,
                               getDefaultImplementation(D), StartOffset, false);
  }

  void printDeclLoc(const Decl *D) override {
    if (EntitiesStack.back().Dcl == D) {
      unsigned LocOffset = OS.tell();
      EntitiesStack.back().LocOffset = LocOffset;
    }
  }

  void printDeclPost(const Decl *D, Optional<BracketOptions> Bracket) override {
    if (isa<ParamDecl>(D))
      return; // Parameters are handled specially in addParameters().
    if (!shouldContinuePost(D, Bracket))
      return;
    assert(!EntitiesStack.empty());
    TextEntity Entity = std::move(EntitiesStack.back());
    EntitiesStack.pop_back();
    unsigned EndOffset = OS.tell();
    Entity.Range.Length = EndOffset - Entity.Range.Offset;
    if (EntitiesStack.empty()) {
      assert (D->getDeclContext()->isModuleScopeContext());
      TopEntities.push_back(std::move(Entity));
    } else {
      assert (!D->getDeclContext()->isModuleScopeContext());
      EntitiesStack.back().SubEntities.push_back(std::move(Entity));
    }
    deinitDefaultMapToUse(D);
  }

  void printTypeRef(Type T, const TypeDecl *TD, Identifier Name) override {
    unsigned StartOffset = OS.tell();
    References.emplace_back(TD, StartOffset, Name.str().size());
    StreamPrinter::printTypeRef(T, TD, Name);
  }
};

struct SourceTextInfo {
  std::string Text;
  std::vector<TextEntity> TopEntities;
  std::vector<TextReference> References;
};

}

static void initDocGenericParams(const Decl *D, DocEntityInfo &Info) {
  auto *DC = dyn_cast<DeclContext>(D);
  if (DC == nullptr || !DC->isInnermostContextGeneric())
    return;

  GenericSignature *GenericSig = DC->getGenericSignatureOfContext();

  if (!GenericSig)
    return;

  // FIXME: Not right for extensions of nested generic types
  for (auto *GP : GenericSig->getInnermostGenericParams()) {
    if (GP->getDecl()->isImplicit())
      continue;
    DocGenericParam Param;
    Param.Name = GP->getName().str();
    Info.GenericParams.push_back(Param);
  }

  ProtocolDecl *proto = nullptr;
  if (auto *typeDC = DC->getInnermostTypeContext())
    proto = typeDC->getAsProtocolOrProtocolExtensionContext();

  for (auto &Req : GenericSig->getRequirements()) {
    // Skip protocol Self requirement.
    if (proto &&
        Req.getKind() == RequirementKind::Conformance &&
        Req.getFirstType()->isEqual(proto->getSelfInterfaceType()) &&
        Req.getSecondType()->getAnyNominal() == proto)
      continue;

    std::string ReqStr;
    PrintOptions Opts;
    llvm::raw_string_ostream OS(ReqStr);
    Req.print(OS, Opts);
    OS.flush();
    Info.GenericRequirements.push_back(std::move(ReqStr));
  }
}

static bool initDocEntityInfo(const Decl *D, const Decl *SynthesizedTarget,
                              const Decl *DefaultImplementationOf,
                              bool IsRef, bool IsSynthesizedExtension,
                              DocEntityInfo &Info,
                              StringRef Arg = StringRef()) {
  if (!D || isa<ParamDecl>(D) ||
      (isa<VarDecl>(D) && D->getDeclContext()->isLocalContext())) {
    Info.Kind = SwiftLangSupport::getUIDForLocalVar(IsRef);
    if (D) {
      llvm::raw_svector_ostream OS(Info.Name);
      SwiftLangSupport::printDisplayName(cast<ValueDecl>(D), OS);
    } else {
      Info.Name = "_";
    }

    if (!Arg.empty())
      Info.Argument = Arg.str();

    return false;
  }

  if (IsSynthesizedExtension)
    Info.Kind = SwiftLangSupport::getUIDForExtensionOfDecl(SynthesizedTarget);
  else
    Info.Kind = SwiftLangSupport::getUIDForDecl(D, IsRef);

  if (Info.Kind.isInvalid())
    return true;
  if (const ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
    llvm::raw_svector_ostream NameOS(Info.Name);
    SwiftLangSupport::printDisplayName(VD, NameOS);
    {
      llvm::raw_svector_ostream OS(Info.USR);
      SwiftLangSupport::printUSR(VD, OS);
      if (SynthesizedTarget) {
        OS << SwiftLangSupport::SynthesizedUSRSeparator;
        SwiftLangSupport::printUSR(dyn_cast<ValueDecl>(SynthesizedTarget), OS);
        {
          llvm::raw_svector_ostream OS(Info.OriginalUSR);
          SwiftLangSupport::printUSR(VD, OS);
        }
      }
    }
  }

  if (DefaultImplementationOf) {
    llvm::raw_svector_ostream OS(Info.ProvideImplementationOfUSR);
    SwiftLangSupport::printUSR((ValueDecl*)DefaultImplementationOf, OS);
  }

  Info.IsUnavailable = AvailableAttr::isUnavailable(D);
  Info.IsDeprecated = D->getAttrs().getDeprecated(D->getASTContext()) != nullptr;
  Info.IsOptional = D->getAttrs().hasAttribute<OptionalAttr>();

  if (!IsRef) {
    llvm::raw_svector_ostream OS(Info.DocComment);

    {
      llvm::SmallString<128> DocBuffer;
      {
        llvm::raw_svector_ostream OSS(DocBuffer);
        ide::getDocumentationCommentAsXML(D, OSS);
      }
      StringRef DocRef = (StringRef)DocBuffer;
      if (IsSynthesizedExtension &&
          DocRef.find("<Declaration>") != StringRef::npos) {
        StringRef Open = "<Declaration>extension ";
        assert(DocRef.find(Open) != StringRef::npos);
        auto FirstPart = DocRef.substr(0, DocRef.find(Open) + (Open).size());
        auto SecondPart = DocRef.substr(FirstPart.size());
        auto ExtendedName = ((ExtensionDecl*)D)->getExtendedType()->
          getAnyNominal()->getName().str();
        assert(SecondPart.startswith(ExtendedName));
        SecondPart = SecondPart.substr(ExtendedName.size());
        llvm::SmallString<128> UpdatedDocBuffer;
        UpdatedDocBuffer.append(FirstPart);
        UpdatedDocBuffer.append(((NominalTypeDecl*)SynthesizedTarget)->getName().
                                str());
        UpdatedDocBuffer.append(SecondPart);
        OS << UpdatedDocBuffer;
      } else
        OS << DocBuffer;
    }

    initDocGenericParams(D, Info);

    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      llvm::raw_svector_ostream OS(Info.FullyAnnotatedDecl);
      if (SynthesizedTarget)
        SwiftLangSupport::printFullyAnnotatedSynthesizedDeclaration(VD,
          (NominalTypeDecl*)SynthesizedTarget, OS);
      else
        SwiftLangSupport::printFullyAnnotatedDeclaration(VD, Type(), OS);
    }
  }

  return false;
}

static bool initDocEntityInfo(const TextEntity &Entity,
                              DocEntityInfo &Info) {
  if (initDocEntityInfo(Entity.Dcl, Entity.SynthesizeTarget,
                        Entity.DefaultImplementationOf,
                        /*IsRef=*/false, Entity.IsSynthesizedExtension,
                        Info, Entity.Argument))
    return true;
  Info.Offset = Entity.Range.Offset;
  Info.Length = Entity.Range.Length;
  return false;
}

static const TypeDecl *getTypeDeclFromType(Type Ty) {
  if (auto Alias = dyn_cast<NameAliasType>(Ty.getPointer()))
    return Alias->getDecl();
  return Ty->getAnyNominal();
}

static void passInherits(const ValueDecl *D, DocInfoConsumer &Consumer) {
  DocEntityInfo EntInfo;
  if (initDocEntityInfo(D, nullptr, nullptr, /*IsRef=*/true, false, EntInfo))
    return;
  Consumer.handleInheritsEntity(EntInfo);
}
static void passConforms(const ValueDecl *D, DocInfoConsumer &Consumer) {
  DocEntityInfo EntInfo;
  if (initDocEntityInfo(D, nullptr, nullptr, /*IsRef=*/true, false, EntInfo))
    return;
  Consumer.handleConformsToEntity(EntInfo);
}
static void passInherits(ArrayRef<TypeLoc> InheritedTypes,
                         DocInfoConsumer &Consumer) {
  for (auto Inherited : InheritedTypes) {
    if (!Inherited.getType())
      continue;

    if (auto Proto = Inherited.getType()->getAs<ProtocolType>()) {
      passConforms(Proto->getDecl(), Consumer);
      continue;
    }

    if (auto ProtoComposition
               = Inherited.getType()->getAs<ProtocolCompositionType>()) {
      for (auto T : ProtoComposition->getProtocols())
        passInherits(TypeLoc::withoutLoc(T), Consumer);
      continue;
    }

    if (auto TD = getTypeDeclFromType(Inherited.getType())) {
      passInherits(TD, Consumer);
      continue;
    }
  }
}

static void passConforms(ArrayRef<ValueDecl *> Dcls,
                         DocInfoConsumer &Consumer) {
  for (auto D : Dcls)
    passConforms(D, Consumer);
}
static void passExtends(const ValueDecl *D, DocInfoConsumer &Consumer) {
  DocEntityInfo EntInfo;
  if (initDocEntityInfo(D, nullptr, nullptr, /*IsRef=*/true, false, EntInfo))
    return;
  Consumer.handleExtendsEntity(EntInfo);
}

static void passInheritsAndConformancesForValueDecl(const ValueDecl *VD,
                                                    DocInfoConsumer &Consumer) {
  if (auto Overridden = VD->getOverriddenDecl())
    passInherits(Overridden, Consumer);
  passConforms(VD->getSatisfiedProtocolRequirements(/*Sorted=*/true),
               Consumer);
}

static void reportRelated(ASTContext &Ctx,
                          const Decl *D,
                          const Decl* SynthesizedTarget,
                          DocInfoConsumer &Consumer) {
  if (!D || isa<ParamDecl>(D))
    return;
  if (const ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
    if (SynthesizedTarget) {
      passExtends((ValueDecl*)SynthesizedTarget, Consumer);
    } else if (Type T = ED->getExtendedType()) {
      if (auto TD = getTypeDeclFromType(T))
        passExtends(TD, Consumer);
    }

    passInherits(ED->getInherited(), Consumer);

  } else if (auto *TAD = dyn_cast<TypeAliasDecl>(D)) {

    if (TAD->hasInterfaceType()) {
      // If underlying type exists, report the inheritance and conformance of the
      // underlying type.
      auto Ty = TAD->getDeclaredInterfaceType();
      if (auto NM = Ty->getAnyNominal()) {
        passInherits(NM->getInherited(), Consumer);
        passConforms(NM->getSatisfiedProtocolRequirements(/*Sorted=*/true),
                     Consumer);
        return;
      }
    }

    // Otherwise, report the inheritance of the type alias itself.
    passInheritsAndConformancesForValueDecl(TAD, Consumer);
  } else if (const TypeDecl *TD = dyn_cast<TypeDecl>(D)) {
    passInherits(TD->getInherited(), Consumer);
    passConforms(TD->getSatisfiedProtocolRequirements(/*Sorted=*/true),
                 Consumer);
  } else if (auto *VD = dyn_cast<ValueDecl>(D)) {
    passInheritsAndConformancesForValueDecl(VD, Consumer);
  }
}

static ArrayRef<const DeclAttribute*>
getDeclAttributes(const Decl *D, std::vector<const DeclAttribute*> &Scratch) {
  for (auto Attr : D->getAttrs()) {
    Scratch.push_back(Attr);
  }
  // For enum elements, inherit their parent enum decls' deprecated attributes.
  if (auto *DE = dyn_cast<EnumElementDecl>(D)) {
    for (auto Attr : DE->getParentEnum()->getAttrs()) {
      if (auto Avail = dyn_cast<AvailableAttr>(Attr)) {
        if (Avail->Deprecated || Avail->isUnconditionallyDeprecated()) {
          Scratch.push_back(Attr);
        }
      }
    }
  }

  return llvm::makeArrayRef(Scratch);
}

// Only reports @available.
// FIXME: Handle all attributes.
static void reportAttributes(ASTContext &Ctx,
                             const Decl *D,
                             DocInfoConsumer &Consumer) {
  static UIdent AvailableAttrKind("source.lang.swift.attribute.availability");
  static UIdent PlatformIOS("source.availability.platform.ios");
  static UIdent PlatformOSX("source.availability.platform.osx");
  static UIdent PlatformtvOS("source.availability.platform.tvos");
  static UIdent PlatformWatchOS("source.availability.platform.watchos");
  static UIdent PlatformIOSAppExt("source.availability.platform.ios_app_extension");
  static UIdent PlatformOSXAppExt("source.availability.platform.osx_app_extension");
  static UIdent PlatformtvOSAppExt("source.availability.platform.tvos_app_extension");
  static UIdent PlatformWatchOSAppExt("source.availability.platform.watchos_app_extension");
  std::vector<const DeclAttribute*> Scratch;

  for (auto Attr : getDeclAttributes(D, Scratch)) {
    if (auto Av = dyn_cast<AvailableAttr>(Attr)) {
      UIdent PlatformUID;
      switch (Av->Platform) {
      case PlatformKind::none:
        PlatformUID = UIdent(); break;
      case PlatformKind::iOS:
        PlatformUID = PlatformIOS; break;
      case PlatformKind::OSX:
        PlatformUID = PlatformOSX; break;
      case PlatformKind::tvOS:
        PlatformUID = PlatformtvOS; break;
      case PlatformKind::watchOS:
        PlatformUID = PlatformWatchOS; break;
      case PlatformKind::iOSApplicationExtension:
        PlatformUID = PlatformIOSAppExt; break;
      case PlatformKind::OSXApplicationExtension:
        PlatformUID = PlatformOSXAppExt; break;
      case PlatformKind::tvOSApplicationExtension:
        PlatformUID = PlatformtvOSAppExt; break;
      case PlatformKind::watchOSApplicationExtension:
        PlatformUID = PlatformWatchOSAppExt; break;
      }

      AvailableAttrInfo Info;
      Info.AttrKind = AvailableAttrKind;
      Info.IsUnavailable = Av->isUnconditionallyUnavailable();
      Info.IsDeprecated = Av->isUnconditionallyDeprecated();
      Info.Platform = PlatformUID;
      Info.Message = Av->Message;
      if (Av->Introduced)
        Info.Introduced = *Av->Introduced;
      if (Av->Deprecated)
        Info.Deprecated = *Av->Deprecated;
      if (Av->Obsoleted)
        Info.Obsoleted = *Av->Obsoleted;

      Consumer.handleAvailableAttribute(Info);
    }
  }
}

static void reportDocEntities(ASTContext &Ctx,
                              ArrayRef<TextEntity> Entities,
                              DocInfoConsumer &Consumer) {
  for (auto &Entity : Entities) {
    DocEntityInfo EntInfo;
    if (initDocEntityInfo(Entity, EntInfo))
      continue;
    Consumer.startSourceEntity(EntInfo);
    reportRelated(Ctx, Entity.Dcl,
          Entity.IsSynthesizedExtension ? Entity.SynthesizeTarget : nullptr,
                  Consumer);
    reportDocEntities(Ctx, Entity.SubEntities, Consumer);
    reportAttributes(Ctx, Entity.Dcl, Consumer);
    Consumer.finishSourceEntity(EntInfo.Kind);
  }
}

namespace {
class DocSyntaxWalker : public SyntaxModelWalker {
  SourceManager &SM;
  unsigned BufferID;
  ArrayRef<TextReference> References;
  DocInfoConsumer &Consumer;
  SourceLoc LastArgLoc;
  SourceLoc LastParamLoc;

public:
  DocSyntaxWalker(SourceManager &SM, unsigned BufferID,
                  ArrayRef<TextReference> References,
                  DocInfoConsumer &Consumer)
    : SM(SM), BufferID(BufferID), References(References), Consumer(Consumer) {}

  bool walkToNodePre(SyntaxNode Node) override {
    unsigned Offset = SM.getLocOffsetInBuffer(Node.Range.getStart(), BufferID);
    unsigned Length = Node.Range.getByteLength();

    reportRefsUntil(Offset);
    if (!References.empty() && References.front().Range.Offset == Offset)
      return true;

    switch (Node.Kind) {
    case SyntaxNodeKind::EditorPlaceholder:
      return true;

    case SyntaxNodeKind::Keyword:
      if (Node.Range.getStart() == LastArgLoc ||
          Node.Range.getStart() == LastParamLoc)
        return true;
      break;

    case SyntaxNodeKind::Identifier:
    case SyntaxNodeKind::DollarIdent:
    case SyntaxNodeKind::Integer:
    case SyntaxNodeKind::Floating:
    case SyntaxNodeKind::String:
    case SyntaxNodeKind::StringInterpolationAnchor:
    case SyntaxNodeKind::CommentLine:
    case SyntaxNodeKind::CommentBlock:
    case SyntaxNodeKind::CommentMarker:
    case SyntaxNodeKind::CommentURL:
    case SyntaxNodeKind::DocCommentLine:
    case SyntaxNodeKind::DocCommentBlock:
    case SyntaxNodeKind::DocCommentField:
    case SyntaxNodeKind::TypeId:
    case SyntaxNodeKind::BuildConfigKeyword:
    case SyntaxNodeKind::BuildConfigId:
    case SyntaxNodeKind::AttributeId:
    case SyntaxNodeKind::AttributeBuiltin:
    case SyntaxNodeKind::ObjectLiteral:
      break;
    }

    DocEntityInfo Info;
    Info.Kind = SwiftLangSupport::getUIDForSyntaxNodeKind(Node.Kind);
    Info.Offset = Offset;
    Info.Length = Length;
    Consumer.handleAnnotation(Info);
    return true;
  }

  void finished() {
    reportRefsUntil(std::numeric_limits<unsigned>::max());
  }

  bool walkToSubStructurePre(SyntaxStructureNode Node) override {
    if (Node.Kind == SyntaxStructureKind::Parameter) {
      auto Param = dyn_cast<ParamDecl>(Node.Dcl);

      auto passAnnotation = [&](UIdent Kind, SourceLoc Loc, Identifier Name) {
        if (Loc.isInvalid())
          return;
        unsigned Offset = SM.getLocOffsetInBuffer(Loc, BufferID);
        unsigned Length = Name.empty() ? 1 : Name.getLength();
        reportRefsUntil(Offset);

        DocEntityInfo Info;
        Info.Kind = Kind;
        Info.Offset = Offset;
        Info.Length = Length;
        Consumer.handleAnnotation(Info);
      };

      // Argument
      static UIdent KindArgument("source.lang.swift.syntaxtype.argument");
      passAnnotation(KindArgument, Param->getArgumentNameLoc(),
                     Param->getArgumentName());
      LastArgLoc = Param->getArgumentNameLoc();

      // Parameter
      static UIdent KindParameter("source.lang.swift.syntaxtype.parameter");
      passAnnotation(KindParameter, Param->getNameLoc(), Param->getName());
      LastParamLoc = Param->getNameLoc();
    }

    return true;
  }

private:
  void reportRefsUntil(unsigned Offset) {
    while (!References.empty() && References.front().Range.Offset < Offset) {
      const TextReference &Ref = References.front();
      References = References.slice(1);
      DocEntityInfo Info;
      if (initDocEntityInfo(Ref.Dcl, nullptr, nullptr, /*IsRef=*/true, false,
                            Info))
        continue;
      Info.Offset = Ref.Range.Offset;
      Info.Length = Ref.Range.Length;
      Info.Ty = Ref.Ty;
      Consumer.handleAnnotation(Info);
    }
  }
};
}

static bool makeParserAST(CompilerInstance &CI, StringRef Text) {
  CompilerInvocation Invocation;
  Invocation.setModuleName("main");
  Invocation.setInputKind(InputFileKind::IFK_Swift);

  std::unique_ptr<llvm::MemoryBuffer> Buf;
  Buf = llvm::MemoryBuffer::getMemBuffer(Text, "<module-interface>");
  Invocation.addInputBuffer(Buf.get());
  if (CI.setup(Invocation))
    return true;
  CI.performParseOnly();
  return false;
}

static void collectFuncEntities(std::vector<TextEntity> &Ents,
                                std::vector<TextEntity*> &FuncEntities) {
  for (TextEntity &Ent : Ents) {
    if (isa<AbstractFunctionDecl>(Ent.Dcl) || isa<SubscriptDecl>(Ent.Dcl)) {
      // We are getting the entities via a pointer and later adding to their
      // subentities; make sure it doesn't have subentities now or we are going
      // to invalidate the pointers.
      assert(Ent.SubEntities.empty());
      FuncEntities.push_back(&Ent);
    }
    collectFuncEntities(Ent.SubEntities, FuncEntities);
  }
}

static void addParameters(ArrayRef<Identifier> &ArgNames,
                          const ParameterList *paramList,
                          TextEntity &Ent,
                          SourceManager &SM,
                          unsigned BufferID) {
  for (auto &param : *paramList) {
    StringRef Arg;
    if (!ArgNames.empty()) {
      Identifier Id = ArgNames.front();
      Arg = Id.empty() ? "_" : Id.str();
      ArgNames = ArgNames.slice(1);
    }

    if (auto typeRepr = param->getTypeLoc().getTypeRepr()) {
      SourceRange TypeRange = param->getTypeLoc().getSourceRange();
      if (auto InOutTyR = dyn_cast_or_null<InOutTypeRepr>(typeRepr))
        TypeRange = InOutTyR->getBase()->getSourceRange();
      if (TypeRange.isInvalid())
        continue;

      unsigned StartOffs = SM.getLocOffsetInBuffer(TypeRange.Start, BufferID);
      unsigned EndOffs =
        SM.getLocOffsetInBuffer(Lexer::getLocForEndOfToken(SM, TypeRange.End),
                                BufferID);
      TextRange TR{ StartOffs, EndOffs-StartOffs };
      TextEntity Param(param, nullptr, nullptr, Arg, TR, StartOffs, false);
      Ent.SubEntities.push_back(std::move(Param));
    }
  }
}

static void addParameters(const AbstractFunctionDecl *FD,
                          TextEntity &Ent,
                          SourceManager &SM,
                          unsigned BufferID) {
  auto params = FD->getParameterLists();
  // Ignore 'self'.
  if (FD->getDeclContext()->isTypeContext())
    params = params.slice(1);

  ArrayRef<Identifier> ArgNames;
  DeclName Name = FD->getFullName();
  if (Name) {
    ArgNames = Name.getArgumentNames();
  }
  for (auto paramList : params) {
    addParameters(ArgNames, paramList, Ent, SM, BufferID);
  }
}

static void addParameters(const SubscriptDecl *D,
                          TextEntity &Ent,
                          SourceManager &SM,
                          unsigned BufferID) {
  ArrayRef<Identifier> ArgNames;
  DeclName Name = D->getFullName();
  if (Name) {
    ArgNames = Name.getArgumentNames();
  }
  addParameters(ArgNames, D->getIndices(), Ent, SM, BufferID);
}

namespace {
class FuncWalker : public ASTWalker {
  SourceManager &SM;
  unsigned BufferID;
  llvm::MutableArrayRef<TextEntity*> FuncEnts;

public:
  FuncWalker(SourceManager &SM, unsigned BufferID,
             llvm::MutableArrayRef<TextEntity*> FuncEnts)
    : SM(SM), BufferID(BufferID), FuncEnts(FuncEnts) {}

  bool walkToDeclPre(Decl *D) override {
    if (D->isImplicit())
      return false; // Skip body.

    if (FuncEnts.empty())
      return false;

    if (!isa<AbstractFunctionDecl>(D) && !isa<SubscriptDecl>(D))
      return true;

    unsigned Offset = SM.getLocOffsetInBuffer(D->getLoc(), BufferID);
    auto Found = FuncEnts.end();
    if (FuncEnts.front()->LocOffset == Offset) {
      Found = FuncEnts.begin();
    } else {
      Found = std::lower_bound(FuncEnts.begin(), FuncEnts.end(), Offset,
        [](TextEntity *Ent, unsigned Offs) {
          return Ent->LocOffset < Offs;
        });
    }
    if (Found == FuncEnts.end() || (*Found)->LocOffset != Offset)
      return false;
    if (auto FD = dyn_cast<AbstractFunctionDecl>(D)) {
      addParameters(FD, **Found, SM, BufferID);
    } else {
      addParameters(cast<SubscriptDecl>(D), **Found, SM, BufferID);
    }
    FuncEnts = llvm::MutableArrayRef<TextEntity*>(Found+1, FuncEnts.end());
    return false; // skip body.
  }
};
}

static void addParameterEntities(CompilerInstance &CI,
                                 SourceTextInfo &IFaceInfo) {
  std::vector<TextEntity*> FuncEntities;
  collectFuncEntities(IFaceInfo.TopEntities, FuncEntities);
  llvm::MutableArrayRef<TextEntity*> FuncEnts(FuncEntities.data(),
                                              FuncEntities.size());
  for (auto Unit : CI.getMainModule()->getFiles()) {
    auto SF = dyn_cast<SourceFile>(Unit);
    if (!SF)
      continue;
    FuncWalker Walker(CI.getSourceMgr(), *SF->getBufferID(), FuncEnts);
    SF->walk(Walker);
  }
}

static void reportSourceAnnotations(const SourceTextInfo &IFaceInfo,
                                    CompilerInstance &CI,
                                    DocInfoConsumer &Consumer) {
  for (auto Unit : CI.getMainModule()->getFiles()) {
    auto SF = dyn_cast<SourceFile>(Unit);
    if (!SF)
      continue;

    SyntaxModelContext SyntaxContext(*SF);
    DocSyntaxWalker SyntaxWalker(CI.getSourceMgr(), *SF->getBufferID(),
                                 IFaceInfo.References, Consumer);
    SyntaxContext.walk(SyntaxWalker);
    SyntaxWalker.finished();
  }
}

static bool getModuleInterfaceInfo(ASTContext &Ctx, StringRef ModuleName,
                                   SourceTextInfo &Info) {
  // Load standard library so that Clang importer can use it.
  auto *Stdlib = getModuleByFullName(Ctx, Ctx.StdlibModuleName);
  if (!Stdlib)
    return true;

  auto *M = getModuleByFullName(Ctx, ModuleName);
  if (!M)
    return true;

  PrintOptions Options = PrintOptions::printDocInterface();
  ModuleTraversalOptions TraversalOptions = None;
  TraversalOptions |= ModuleTraversal::VisitSubmodules;
  TraversalOptions |= ModuleTraversal::VisitHidden;

  SmallString<128> Text;
  llvm::raw_svector_ostream OS(Text);
  AnnotatingPrinter Printer(OS);
  printModuleInterface(M, None, TraversalOptions, Printer, Options,
                       true);
  Info.Text = OS.str();
  Info.TopEntities = std::move(Printer.TopEntities);
  Info.References = std::move(Printer.References);
  return false;
}

static bool reportModuleDocInfo(CompilerInvocation Invocation,
                                StringRef ModuleName,
                                DocInfoConsumer &Consumer) {
  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  if (CI.setup(Invocation))
    return true;

  ASTContext &Ctx = CI.getASTContext();
  // Setup a typechecker for protocol conformance resolving.
  OwnedResolver TypeResolver = createLazyResolver(Ctx);

  SourceTextInfo IFaceInfo;
  if (getModuleInterfaceInfo(Ctx, ModuleName, IFaceInfo))
    return true;

  CompilerInstance ParseCI;
  if (makeParserAST(ParseCI, IFaceInfo.Text))
    return true;
  addParameterEntities(ParseCI, IFaceInfo);

  Consumer.handleSourceText(IFaceInfo.Text);
  reportDocEntities(Ctx, IFaceInfo.TopEntities, Consumer);
  reportSourceAnnotations(IFaceInfo, ParseCI, Consumer);

  return false;
}

namespace {
class SourceDocASTWalker : public SourceEntityWalker {
public:
  SourceManager &SM;
  unsigned BufferID;

  std::vector<TextEntity> TopEntities;
  std::vector<TextEntity> EntitiesStack;
  std::vector<TextReference> References;

  SourceDocASTWalker(SourceManager &SM, unsigned BufferID)
    : SM(SM), BufferID(BufferID) {}

  ~SourceDocASTWalker() {
    assert(EntitiesStack.empty());
  }

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (!isa<ValueDecl>(D) && !isa<ExtensionDecl>(D))
      return true;
    if (isLocal(D))
      return true;
    TextRange TR = getTextRange(D->getSourceRange());
    unsigned LocOffset = getOffset(Range.getStart());
    EntitiesStack.emplace_back(D, nullptr, nullptr, TR, LocOffset, false);
    return true;
  }

  bool walkToDeclPost(Decl *D) override {
    if (EntitiesStack.empty() || EntitiesStack.back().Dcl != D)
      return true;

    TextEntity Entity = std::move(EntitiesStack.back());
    EntitiesStack.pop_back();
    if (EntitiesStack.empty())
      TopEntities.push_back(Entity);
    else
      EntitiesStack.back().SubEntities.push_back(Entity);
    return true;
  }

  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type Ty,
                          SemaReferenceKind Kind) override {
    unsigned StartOffset = getOffset(Range.getStart());
    References.emplace_back(D, StartOffset, Range.getByteLength(), Ty);
    return true;
  }

  bool visitSubscriptReference(ValueDecl *D, CharSourceRange Range,
                               bool IsOpenBracket) override {
    // Treat both open and close brackets equally
    return visitDeclReference(D, Range, nullptr, nullptr, Type(),
                              SemaReferenceKind::SubscriptRef);
  }

  bool isLocal(Decl *D) const {
    return isa<ParamDecl>(D) || D->getDeclContext()->getLocalContext();
  }

  unsigned getOffset(SourceLoc Loc) const {
    return SM.getLocOffsetInBuffer(Loc, BufferID);
  }

  TextRange getTextRange(SourceRange R) const {
    unsigned Start = getOffset(R.Start);
    unsigned End = getOffset(R.End);
    return TextRange{ Start, End-Start };
  }
};
}

static bool getSourceTextInfo(CompilerInstance &CI,
                              SourceTextInfo &Info) {
  SourceManager &SM = CI.getSourceMgr();
  unsigned BufID = CI.getInputBufferIDs().back();

  SourceDocASTWalker Walker(SM, BufID);
  Walker.walk(*CI.getMainModule());

  CharSourceRange FullRange = SM.getRangeForBuffer(BufID);
  Info.Text = SM.extractText(FullRange);
  Info.TopEntities = std::move(Walker.TopEntities);
  Info.References = std::move(Walker.References);
  return false;
}

static bool reportSourceDocInfo(CompilerInvocation Invocation,
                                llvm::MemoryBuffer *InputBuf,
                                DocInfoConsumer &Consumer) {
  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  EditorDiagConsumer DiagConsumer;
  CI.addDiagnosticConsumer(&DiagConsumer);

  Invocation.addInputBuffer(InputBuf);
  if (CI.setup(Invocation))
    return true;
  DiagConsumer.setInputBufferIDs(CI.getInputBufferIDs());

  ASTContext &Ctx = CI.getASTContext();
  CloseClangModuleFiles scopedCloseFiles(*Ctx.getClangModuleLoader());
  CI.performSema();

  // Setup a typechecker for protocol conformance resolving.
  OwnedResolver TypeResolver = createLazyResolver(Ctx);

  SourceTextInfo SourceInfo;
  if (getSourceTextInfo(CI, SourceInfo))
    return true;
  addParameterEntities(CI, SourceInfo);

  reportDocEntities(Ctx, SourceInfo.TopEntities, Consumer);
  reportSourceAnnotations(SourceInfo, CI, Consumer);
  for (auto &Diag : DiagConsumer.getDiagnosticsForBuffer(
                                                CI.getInputBufferIDs().back()))
    Consumer.handleDiagnostic(Diag);

  return false;
}

void SwiftLangSupport::getDocInfo(llvm::MemoryBuffer *InputBuf,
                                  StringRef ModuleName,
                                  ArrayRef<const char *> Args,
                                  DocInfoConsumer &Consumer) {
  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  CompilerInvocation Invocation;
  std::string Error;
  bool Failed = getASTManager().initCompilerInvocation(Invocation, Args,
                                                       CI.getDiags(),
                                                       StringRef(),
                                                       Error);
  if (Failed) {
    Consumer.failed(Error);
    return;
  }
  Invocation.getClangImporterOptions().ImportForwardDeclarations = true;

  if (!ModuleName.empty()) {
    bool Error = reportModuleDocInfo(Invocation, ModuleName, Consumer);
    if (Error)
      Consumer.failed("Error occurred");
    return;
  }

  Failed = reportSourceDocInfo(Invocation, InputBuf, Consumer);
  if (Failed)
    Consumer.failed("Error occurred");
}

void SwiftLangSupport::findModuleGroups(StringRef ModuleName,
                                        ArrayRef<const char *> Args,
                                        std::function<void(ArrayRef<StringRef>,
                                                           StringRef Error)> Receiver) {
  CompilerInvocation Invocation;
  Invocation.getClangImporterOptions().ImportForwardDeclarations = true;
  Invocation.clearInputs();

  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);
  std::vector<StringRef> Groups;
  std::string Error;
  if (getASTManager().initCompilerInvocation(Invocation, Args, CI.getDiags(),
                                             StringRef(), Error)) {
    Receiver(Groups, Error);
    return;
  }
  if (CI.setup(Invocation)) {
    Error = "Compiler invocation set up fails.";
    Receiver(Groups, Error);
    return;
  }

  ASTContext &Ctx = CI.getASTContext();
  // Setup a typechecker for protocol conformance resolving.
  OwnedResolver TypeResolver = createLazyResolver(Ctx);
  // Load standard library so that Clang importer can use it.
  auto *Stdlib = getModuleByFullName(Ctx, Ctx.StdlibModuleName);
  if (!Stdlib) {
    Error = "Cannot load stdlib.";
    Receiver(Groups, Error);
    return;
  }
  auto *M = getModuleByFullName(Ctx, ModuleName);
  if (!M) {
    Error = "Cannot find the module.";
    Receiver(Groups, Error);
    return;
  }
  std::vector<StringRef> Scratch;
  Receiver(collectModuleGroups(M, Scratch), Error);
}
