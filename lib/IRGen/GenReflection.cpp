//===--- GenReflection.cpp - IR generation for nominal type reflection ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation of type metadata for struct/class
//  stored properties and enum cases for use with reflection.
//===----------------------------------------------------------------------===//

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Reflection/MetadataSourceBuilder.h"
#include "swift/Reflection/Records.h"
#include "swift/SIL/SILModule.h"

#include "ConstantBuilder.h"
#include "GenClass.h"
#include "GenHeap.h"
#include "GenProto.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"

using namespace swift;
using namespace irgen;
using namespace reflection;

class MetadataSourceEncoder
  : public MetadataSourceVisitor<MetadataSourceEncoder> {
  llvm::raw_ostream &OS;
public:
  MetadataSourceEncoder(llvm::raw_ostream &OS) : OS(OS) {}

  void
  visitClosureBindingMetadataSource(const ClosureBindingMetadataSource *CB) {
    OS << 'B';
    OS << CB->getIndex();
  }

  void
  visitReferenceCaptureMetadataSource(const ReferenceCaptureMetadataSource *RC){
    OS << 'R';
    OS << RC->getIndex();
  }

  void
  visitMetadataCaptureMetadataSource(const MetadataCaptureMetadataSource *MC) {
    OS << 'M';
    OS << MC->getIndex();
  }

  void
  visitGenericArgumentMetadataSource(const GenericArgumentMetadataSource *GA) {
    OS << 'G';
    OS << GA->getIndex();
    visit(GA->getSource());
    OS << '_';
  }

  void visitParentMetadataSource(const ParentMetadataSource *P) {
    OS << 'P';
    visit(P->getChild());
    OS << '_';
  }

  void visitSelfMetadataSource(const SelfMetadataSource *S) {
    OS << 'S';
  }

  void
  visitSelfWitnessTableMetadataSource(const SelfWitnessTableMetadataSource *S) {
    OS << 'W';
  }
};

class PrintMetadataSource
: public MetadataSourceVisitor<PrintMetadataSource, void> {
  llvm::raw_ostream &OS;
  unsigned Indent;

  llvm::raw_ostream &indent(unsigned Amount) {
    for (unsigned i = 0; i < Amount; ++i)
      OS << ' ';
    return OS;
  }

  llvm::raw_ostream &printHeader(std::string Name) {
    indent(Indent) << '(' << Name;
    return OS;
  }

  template<typename T>
  llvm::raw_ostream &printField(std::string name, const T &value) {
    if (!name.empty())
      OS << " " << name << "=" << value;
    else
      OS << " " << value;
    return OS;
  }

  void printRec(const MetadataSource *MS) {
    OS << "\n";

    Indent += 2;
    visit(MS);
    Indent -= 2;
  }

  void closeForm() {
    OS << ')';
  }

public:
  PrintMetadataSource(llvm::raw_ostream &OS, unsigned Indent)
    : OS(OS), Indent(Indent) {}

  void
  visitClosureBindingMetadataSource(const ClosureBindingMetadataSource *CB) {
    printHeader("closure-binding");
    printField("index", CB->getIndex());
    closeForm();
  }

  void
  visitReferenceCaptureMetadataSource(const ReferenceCaptureMetadataSource *RC){
    printHeader("reference-capture");
    printField("index", RC->getIndex());
    closeForm();
  }

  void
  visitMetadataCaptureMetadataSource(const MetadataCaptureMetadataSource *MC){
    printHeader("metadata-capture");
    printField("index", MC->getIndex());
    closeForm();
  }

  void
  visitGenericArgumentMetadataSource(const GenericArgumentMetadataSource *GA) {
    printHeader("generic-argument");
    printField("index", GA->getIndex());
    printRec(GA->getSource());
    closeForm();
  }

  void
  visitParentMetadataSource(const ParentMetadataSource *P) {
    printHeader("parent-of");
    printRec(P->getChild());
    closeForm();
  }

  void
  visitSelfMetadataSource(const SelfMetadataSource *S) {
    printHeader("self");
    closeForm();
  }

  void
  visitSelfWitnessTableMetadataSource(const SelfWitnessTableMetadataSource *S) {
    printHeader("self-witness-table");
    closeForm();
  }
};

class ReflectionMetadataBuilder : public ConstantBuilder<> {
protected:
  llvm::SetVector<CanType> &BuiltinTypes;

  // Collect any builtin types referenced from this type.
  void addBuiltinTypeRefs(CanType type) {
    type.visit([&](Type t) {
      if (t->is<BuiltinType>())
        BuiltinTypes.insert(CanType(t));

      // We need at least size/alignment information for types imported by
      // clang, so we'll treat them as a builtin type, essentially an
      // opaque blob.
      if (auto Nominal = t->getAnyNominal())
        if (Nominal->hasClangNode())
          BuiltinTypes.insert(CanType(t));
    });
  }

  /// Add a 32-bit relative offset to a mangled typeref string
  /// in the typeref reflection section.
  void addTypeRef(Module *ModuleContext, CanType type) {
    assert(type);
    Mangle::Mangler mangler(/*DWARFMangling*/false,
                            /*usePunyCode*/ true,
                            /*OptimizeProtocolNames*/ false);
    mangler.setModuleContext(ModuleContext);
    mangler.mangleType(type, 0);
    auto mangledName = IGM.getAddrOfStringForTypeRef(mangler.finalize());
    addRelativeAddress(mangledName);
  }

public:
  ReflectionMetadataBuilder(IRGenModule &IGM,
                            llvm::SetVector<CanType> &BuiltinTypes)
    : ConstantBuilder(IGM), BuiltinTypes(BuiltinTypes) {}
};

class AssociatedTypeMetadataBuilder : public ReflectionMetadataBuilder {
  static const uint32_t AssociatedTypeRecordSize = 8;

  llvm::PointerUnion<const NominalTypeDecl *, const ExtensionDecl *>
  NominalOrExtensionDecl;

  void addConformance(Module *ModuleContext,
                      CanType ConformingType,
                      const ProtocolConformance *Conformance) {
    SmallVector<std::pair<StringRef, CanType>, 2> AssociatedTypes;

    auto collectTypeWitness = [&](const AssociatedTypeDecl *AssocTy,
                                  const Substitution &Sub,
                                  const TypeDecl *TD) -> bool {

      auto Subst = ArchetypeBuilder::mapTypeOutOfContext(
        Conformance->getDeclContext(), Sub.getReplacement());

      AssociatedTypes.push_back({
        AssocTy->getNameStr(),
        Subst->getCanonicalType()
      });
      return false;
    };

    addTypeRef(ModuleContext, ConformingType);

    auto ProtoTy = Conformance->getProtocol()->getDeclaredType();
    addTypeRef(ModuleContext, ProtoTy->getCanonicalType());

    Conformance->forEachTypeWitness(/*resolver*/ nullptr, collectTypeWitness);

    addConstantInt32(AssociatedTypes.size());
    addConstantInt32(AssociatedTypeRecordSize);

    for (auto AssocTy : AssociatedTypes) {
      auto NameGlobal = IGM.getAddrOfStringForTypeRef(AssocTy.first);
      addRelativeAddress(NameGlobal);
      addBuiltinTypeRefs(AssocTy.second);
      addTypeRef(ModuleContext, AssocTy.second);
    }
  }

  const NominalTypeDecl *getNominalTypeDecl() const {
    return NominalOrExtensionDecl.dyn_cast<const NominalTypeDecl *>();
  }

  const ExtensionDecl *getExtensionDecl() const {
    return NominalOrExtensionDecl.dyn_cast<const ExtensionDecl *>();
  }

  void layout() {
    if (auto Decl = getNominalTypeDecl()) {
      PrettyStackTraceDecl DebugStack("emitting associated type metadata",
                                      Decl);
      for (auto Conformance : Decl->getAllConformances()) {
        if (Conformance->isIncomplete())
          continue;
        addConformance(Decl->getModuleContext(),
                       Decl->getDeclaredType()->getCanonicalType(),
                       Conformance);
      }
    } else if (auto Ext = getExtensionDecl()) {
      PrettyStackTraceDecl DebugStack("emitting associated type metadata", Ext);
      for (auto Conformance : Ext->getLocalConformances()) {
        auto Decl = Ext->getExtendedType()->getNominalOrBoundGenericNominal();
        addConformance(Ext->getDeclContext()->getParentModule(),
                       Decl->getDeclaredType()->getCanonicalType(),
                       Conformance);
      }
    }
  }

public:
  AssociatedTypeMetadataBuilder(IRGenModule &IGM, const NominalTypeDecl *Decl,
                                llvm::SetVector<CanType> &BuiltinTypes)
    : ReflectionMetadataBuilder(IGM, BuiltinTypes),
      NominalOrExtensionDecl(Decl) {}

  AssociatedTypeMetadataBuilder(IRGenModule &IGM, const ExtensionDecl *Decl,
                                llvm::SetVector<CanType> &BuiltinTypes)
    : ReflectionMetadataBuilder(IGM, BuiltinTypes),
      NominalOrExtensionDecl(Decl) {}


  llvm::GlobalVariable *emit() {
    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
        new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
                                 llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();
    if (!init)
      return nullptr;

    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_assocty_metadata");
    var->setSection(IGM.getAssociatedTypeMetadataSectionName());
    var->setAlignment(4);

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);
    
    return var;
  }
};

class FieldTypeMetadataBuilder : public ReflectionMetadataBuilder {
  const uint32_t fieldRecordSize = 12;
  const NominalTypeDecl *NTD;

  void addFieldDecl(const ValueDecl *value, CanType type) {
    reflection::FieldRecordFlags Flags;
    Flags.setIsObjC(value->isObjC());

    addConstantInt32(Flags.getRawValue());

    if (!type) {
      addConstantInt32(0);
    } else {
      addTypeRef(value->getModuleContext(), type);
      addBuiltinTypeRefs(type);
    }

    if (IGM.IRGen.Opts.EnableReflectionNames) {
      auto fieldName = IGM.getAddrOfFieldName(value->getNameStr());
      addRelativeAddress(fieldName);
    } else {
      addConstantInt32(0);
    }
  }

  void addObjCClassRecord() {
    addConstantInt16(uint16_t(FieldDescriptorKind::ObjCClass));
    addConstantInt16(0);
    addConstantInt32(0);
  }

  void addImportedRecord() {
    addConstantInt16(uint16_t(FieldDescriptorKind::Imported));
    addConstantInt16(0);
    addConstantInt32(0);
  }

  void layout() {
    using swift::reflection::FieldDescriptorKind;

    PrettyStackTraceDecl DebugStack("emitting field type metadata", NTD);
    auto type = NTD->getDeclaredType()->getCanonicalType();
    addTypeRef(NTD->getModuleContext(), type);

    if (auto CD = dyn_cast<ClassDecl>(NTD)) {
      auto RC = getReferenceCountingForClass(IGM, const_cast<ClassDecl *>(CD));
      if (RC == ReferenceCounting::ObjC || CD->hasClangNode()) {
        addObjCClassRecord();
        return;
      }
    }

    if (NTD->hasClangNode()) {
      addImportedRecord();
      return;
    }

    switch (NTD->getKind()) {
      case DeclKind::Class:
      case DeclKind::Struct: {
        auto properties = NTD->getStoredProperties();
        addConstantInt16(uint16_t(isa<StructDecl>(NTD)
                                  ? FieldDescriptorKind::Struct
                                  : FieldDescriptorKind::Class));
        addConstantInt16(fieldRecordSize);
        addConstantInt32(std::distance(properties.begin(), properties.end()));
        for (auto property : properties)
          addFieldDecl(property,
                       property->getInterfaceType()
                       ->getCanonicalType());
        break;
      }
      case DeclKind::Enum: {
        auto enumDecl = cast<EnumDecl>(NTD);
        auto cases = enumDecl->getAllElements();
        addConstantInt16(uint16_t(FieldDescriptorKind::Enum));
        addConstantInt16(fieldRecordSize);
        addConstantInt32(std::distance(cases.begin(), cases.end()));
        for (auto enumCase : cases) {
          if (enumCase->hasArgumentType()) {
            addFieldDecl(enumCase,
                         enumCase->getArgumentInterfaceType()
                         ->getCanonicalType());
          } else {
            addFieldDecl(enumCase, CanType());
          }
        }
        break;
      }
      case DeclKind::Protocol: {
        auto protocolDecl = cast<ProtocolDecl>(NTD);
        FieldDescriptorKind Kind;
        if (protocolDecl->isObjC())
          Kind = FieldDescriptorKind::ObjCProtocol;
        else if (protocolDecl->requiresClass())
          Kind = FieldDescriptorKind::ClassProtocol;
        else
          Kind = FieldDescriptorKind::Protocol;
        addConstantInt16(uint16_t(Kind));
        addConstantInt16(fieldRecordSize);
        addConstantInt32(0);
        break;
      }
      default:
        llvm_unreachable("Not a nominal type");
        break;
    }
  }

public:
  FieldTypeMetadataBuilder(IRGenModule &IGM,
                           const NominalTypeDecl * NTD,
                           llvm::SetVector<CanType> &BuiltinTypes)
    : ReflectionMetadataBuilder(IGM, BuiltinTypes),
      NTD(NTD) {}

  llvm::GlobalVariable *emit() {

    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
        new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
                                 llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();

    if (!init)
      return nullptr;

    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_reflection_metadata");
    var->setSection(IGM.getFieldTypeMetadataSectionName());
    var->setAlignment(4);

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);

    return var;
  }
};

class BuiltinTypeMetadataBuilder : public ReflectionMetadataBuilder {
  void addBuiltinType(CanType builtinType) {
    addTypeRef(builtinType->getASTContext().TheBuiltinModule, builtinType);

    auto &ti = cast<FixedTypeInfo>(IGM.getTypeInfoForUnlowered(builtinType));
    addConstantInt32(ti.getFixedSize().getValue());
    addConstantInt32(ti.getFixedAlignment().getValue());
    addConstantInt32(ti.getFixedStride().getValue());
    addConstantInt32(ti.getFixedExtraInhabitantCount(IGM));
  }

  void layout() {
    for (auto builtinType : BuiltinTypes) {
      addBuiltinType(builtinType);
    }
  }

public:
  BuiltinTypeMetadataBuilder(IRGenModule &IGM,
                             llvm::SetVector<CanType> &BuiltinTypes)
    : ReflectionMetadataBuilder(IGM, BuiltinTypes) {}

  llvm::GlobalVariable *emit() {

    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
        new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
                                 llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();

    if (!init)
      return nullptr;

    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_builtin_metadata");
    var->setSection(IGM.getBuiltinTypeMetadataSectionName());
    var->setAlignment(4);

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);

    return var;
  }
};

/// Builds a constant LLVM struct describing the layout of a heap closure,
/// the types of its captures, and the sources of metadata if any of the
/// captures are generic.
class CaptureDescriptorBuilder : public ReflectionMetadataBuilder {
  swift::reflection::MetadataSourceBuilder SourceBuilder;
  SILFunction &Caller;
  CanSILFunctionType OrigCalleeType;
  CanSILFunctionType SubstCalleeType;
  ArrayRef<Substitution> Subs;
  HeapLayout &Layout;
  unsigned FirstCaptureIndex;
public:
  CaptureDescriptorBuilder(IRGenModule &IGM,
                           llvm::SetVector<CanType> &BuiltinTypes,
                           SILFunction &Caller,
                           CanSILFunctionType OrigCalleeType,
                           CanSILFunctionType SubstCalleeType,
                           ArrayRef<Substitution> Subs,
                           HeapLayout &Layout,
                           unsigned FirstCaptureIndex)
    : ReflectionMetadataBuilder(IGM, BuiltinTypes),
      Caller(Caller), OrigCalleeType(OrigCalleeType),
      SubstCalleeType(SubstCalleeType), Subs(Subs),
      Layout(Layout), FirstCaptureIndex(FirstCaptureIndex) {}

  using MetadataSourceMap
    = std::vector<std::pair<CanType, const reflection::MetadataSource*>>;

  void addMetadataSource(const reflection::MetadataSource *Source) {
    if (Source == nullptr) {
      addConstantInt32(0);
    } else {
      SmallString<16> EncodeBuffer;
      llvm::raw_svector_ostream OS(EncodeBuffer);
      MetadataSourceEncoder Encoder(OS);
      Encoder.visit(Source);

      auto EncodedSource = IGM.getAddrOfStringForTypeRef(OS.str());
      addRelativeAddress(EncodedSource);
    }
  }

  /// Slice off the NecessaryBindings struct at the beginning, if it's there.
  /// We'll keep track of how many things are in the bindings struct with its
  /// own count in the capture descriptor.
  ArrayRef<SILType> getElementTypes() {
    return Layout.getElementTypes().slice(Layout.hasBindings() ? 1 : 0);
  }

  /// Build a map from generic parameter -> source of its metadata at runtime.
  ///
  /// If the callee that we are partially applying to create a box/closure
  /// isn't generic, then the map is empty.
  MetadataSourceMap getMetadataSourceMap() {
    MetadataSourceMap SourceMap;

    if (!OrigCalleeType->isPolymorphic())
      return SourceMap;

    // Any generic parameters that are not fulfilled are passed in via the
    // bindings. Structural types are decomposed, so emit the contents of
    // the bindings structure directly.
    auto &Bindings = Layout.getBindings();
    for (unsigned i = 0; i < Bindings.size(); ++i) {
      // Skip protocol requirements (FIXME: for now?)
      if (Bindings[i].Protocol != nullptr)
        continue;

      auto Source = SourceBuilder.createClosureBinding(i);
      auto BindingType = Caller.mapTypeOutOfContext(Bindings[i].TypeParameter);
      SourceMap.push_back({BindingType->getCanonicalType(), Source});
    }

    PolymorphicConvention Convention(IGM, OrigCalleeType);

    using SourceKind = PolymorphicConvention::SourceKind;

    // Check if any requirements were fulfilled by metadata stored inside a
    // captured value.
    auto GenericSig = OrigCalleeType->getGenericSignature();
    auto SubstMap = GenericSig->getSubstitutionMap(Subs);
    auto Generics = GenericSig->getGenericParams();

    for (auto GenericParam : Generics) {
      auto GenericParamType = GenericParam->getCanonicalType();

      auto Fulfillment
        = Convention.getFulfillmentForTypeMetadata(GenericParamType);

      if (Fulfillment != nullptr) {
        auto ConventionSource = Convention.getSource(Fulfillment->SourceIndex);

        if (ConventionSource.getKind() == SourceKind::SelfMetadata ||
            ConventionSource.getKind() == SourceKind::SelfWitnessTable) {
          // Handled as part of bindings
          continue;
        }

        // The metadata might be reached via a non-trivial path (eg,
        // dereferencing an isa pointer or a generic argument). Record
        // the path. We assume captured values map 1-1 with function
        // parameters.
        auto ParamIndex = ConventionSource.getParamIndex();
        auto Index = ParamIndex - FirstCaptureIndex;

        auto ParamType = SubstCalleeType->getParameters()[ParamIndex].getSILType();
        auto CaptureType = getElementTypes()[Index];
        assert(ParamType == CaptureType);
        (void) ParamType;
        (void) CaptureType;

        auto Root = ConventionSource.getMetadataSource(SourceBuilder);
        auto Src = Fulfillment->Path.getMetadataSource(SourceBuilder, Root);

        auto SubstType = Caller.mapTypeOutOfContext(SubstMap[GenericParam]);
        SourceMap.push_back({SubstType->getCanonicalType(), Src});
      }
    }

    return SourceMap;
  }

  /// Get the interface types of all of the captured values, mapped out of the
  /// context of the callee we're partially applying.
  std::vector<CanType> getCaptureTypes() {
    std::vector<CanType> CaptureTypes;

    for (auto ElementType : getElementTypes()) {
      auto SwiftType = ElementType.getSwiftRValueType();
      auto InterfaceType = Caller.mapTypeOutOfContext(SwiftType);
      CaptureTypes.push_back(InterfaceType->getCanonicalType());
    }

    return CaptureTypes;
  }

  void layout() {
    auto CaptureTypes = getCaptureTypes();
    auto MetadataSources = getMetadataSourceMap();

    addConstantInt32(CaptureTypes.size());
    addConstantInt32(MetadataSources.size());
    addConstantInt32(Layout.getBindings().size());

    // Now add typerefs of all of the captures.
    for (auto CaptureType : CaptureTypes) {
      addTypeRef(Caller.getModule().getSwiftModule(), CaptureType);
      addBuiltinTypeRefs(CaptureType);
    }

    // Add the pairs that make up the generic param -> metadata source map
    // to the struct.
    for (auto GenericAndSource : MetadataSources) {
      auto GenericParam = GenericAndSource.first->getCanonicalType();
      auto Source = GenericAndSource.second;

      addTypeRef(nullptr, GenericParam);
      addMetadataSource(Source);
    }
  }

  llvm::GlobalVariable *emit() {
    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
      new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
                               llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();

    if (!init)
      return nullptr;

    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_capture_descriptor");
    var->setSection(IGM.getCaptureDescriptorMetadataSectionName());
    var->setAlignment(4);

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);

    return var;
  }
};

static std::string getReflectionSectionName(IRGenModule &IGM,
                                            std::string Base) {
  SmallString<50> SectionName;
  llvm::raw_svector_ostream OS(SectionName);
  switch (IGM.TargetInfo.OutputObjectFormat) {
    case llvm::Triple::MachO:
      assert(Base.size() <= 7
             && "Mach-O section name length must be <= 16 characters");
      OS << "__TEXT, __swift3_" << Base << ", regular, no_dead_strip";
      break;
    case llvm::Triple::ELF:
      OS << ".swift3_" << Base;
      break;
    default:
      llvm_unreachable("Don't know how to emit field name table for "
                       "the selected object format.");
  }
  return OS.str();
}

std::string IRGenModule::getFieldTypeMetadataSectionName() {
  return getReflectionSectionName(*this, "fieldmd");
}

std::string IRGenModule::getBuiltinTypeMetadataSectionName() {
  return getReflectionSectionName(*this, "builtin");
}

std::string IRGenModule::getAssociatedTypeMetadataSectionName() {
  return getReflectionSectionName(*this, "assocty");
}

std::string IRGenModule::getCaptureDescriptorMetadataSectionName() {
  return getReflectionSectionName(*this, "capture");
}

std::string IRGenModule::getReflectionStringsSectionName() {
  return getReflectionSectionName(*this, "reflstr");
}

std::string IRGenModule::getReflectionTypeRefSectionName() {
  return getReflectionSectionName(*this, "typeref");
}

llvm::Constant *IRGenModule::getAddrOfFieldName(StringRef Name) {
  auto &entry = FieldNames[Name];
  if (entry.second)
    return entry.second;

  entry = createStringConstant(Name, /*willBeRelativelyAddressed*/ true,
                               getReflectionStringsSectionName());
  return entry.second;
}

llvm::Constant *IRGenModule::getAddrOfStringForTypeRef(StringRef Str) {
  auto &entry = StringsForTypeRef[Str];
  if (entry.second)
    return entry.second;

  entry = createStringConstant(Str, /*willBeRelativelyAddressed*/ true,
                               getReflectionTypeRefSectionName());
  return entry.second;
}

llvm::Constant *
IRGenModule::getAddrOfCaptureDescriptor(SILFunction &Caller,
                                        CanSILFunctionType OrigCalleeType,
                                        CanSILFunctionType SubstCalleeType,
                                        ArrayRef<Substitution> Subs,
                                        HeapLayout &Layout,
                                        unsigned FirstCaptureIndex) {
  if (!IRGen.Opts.EnableReflectionMetadata)
    return llvm::Constant::getNullValue(CaptureDescriptorPtrTy);

  llvm::SetVector<CanType> BuiltinTypes;
  CaptureDescriptorBuilder builder(*this, BuiltinTypes, Caller,
                                   OrigCalleeType, SubstCalleeType, Subs,
                                   Layout, FirstCaptureIndex);

  auto var = builder.emit();
  if (var)
    addUsedGlobal(var);

  return llvm::ConstantExpr::getBitCast(var, CaptureDescriptorPtrTy);
}

void IRGenModule::emitReflectionMetadata(const NominalTypeDecl *Decl) {
  if (!IRGen.Opts.EnableReflectionMetadata)
    return;

  emitFieldMetadataRecord(Decl);
  emitAssociatedTypeMetadataRecord(Decl);
}

void IRGenModule::emitAssociatedTypeMetadataRecord(const NominalTypeDecl *Decl){
  if (!IRGen.Opts.EnableReflectionMetadata)
    return;

  AssociatedTypeMetadataBuilder builder(*this, Decl, BuiltinTypes);
  auto var = builder.emit();
  if (var)
    addUsedGlobal(var);
}

void IRGenModule::emitAssociatedTypeMetadataRecord(const ExtensionDecl *Ext) {
  if (!IRGen.Opts.EnableReflectionMetadata)
    return;

  AssociatedTypeMetadataBuilder builder(*this, Ext, BuiltinTypes);
  auto var = builder.emit();
  if (var)
    addUsedGlobal(var);
}

void IRGenModule::emitBuiltinReflectionMetadata() {
  if (getSwiftModule()->isStdlibModule()) {
    BuiltinTypes.insert(Context.TheNativeObjectType);
    BuiltinTypes.insert(Context.TheUnknownObjectType);
    BuiltinTypes.insert(Context.TheBridgeObjectType);
    BuiltinTypes.insert(Context.TheRawPointerType);
    BuiltinTypes.insert(Context.TheUnsafeValueBufferType);
  }

  for (auto Ty : BuiltinTypes)
    if (auto Nominal = Ty.getAnyNominal())
      if (Nominal->hasClangNode())
        emitFieldMetadataRecord(Nominal);

  BuiltinTypeMetadataBuilder builder(*this, BuiltinTypes);
  auto var = builder.emit();
  if (var)
    addUsedGlobal(var);
}

void IRGenModule::emitFieldMetadataRecord(const NominalTypeDecl *Decl) {
  if (!IRGen.Opts.EnableReflectionMetadata)
    return;

  FieldTypeMetadataBuilder builder(*this, Decl, BuiltinTypes);
  auto var = builder.emit();
  if (var)
    addUsedGlobal(var);
}
