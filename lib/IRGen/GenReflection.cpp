//===--- GenReflection.cpp - IR generation for nominal type reflection ----===//
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
//  This file implements IR generation of type metadata for struct/class
//  stored properties and enum cases for use with reflection.
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/IRGen/Linking.h"
#include "swift/Reflection/MetadataSourceBuilder.h"
#include "swift/Reflection/Records.h"
#include "swift/SIL/SILModule.h"

#include "ConstantBuilder.h"
#include "GenClass.h"
#include "GenDecl.h"
#include "GenEnum.h"
#include "GenHeap.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenMangler.h"
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

  void printRec(const reflection::MetadataSource *MS) {
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

llvm::Constant *IRGenModule::getTypeRef(CanType type) {
  IRGenMangler Mangler;
  auto SymbolicName = Mangler.mangleTypeForReflection(*this, type);
  return getAddrOfStringForTypeRef(SymbolicName);
}

class ReflectionMetadataBuilder {
protected:
  IRGenModule &IGM;
  ConstantInitBuilder InitBuilder;
  ConstantStructBuilder B;

  ReflectionMetadataBuilder(IRGenModule &IGM)
    : IGM(IGM), InitBuilder(IGM), B(InitBuilder.beginStruct()) {}

  virtual ~ReflectionMetadataBuilder() {}
  
  // Collect any builtin types referenced from this type.
  void addBuiltinTypeRefs(CanType type) {
    type.visit([&](CanType t) {
      if (IGM.getSwiftModule()->isStdlibModule() && isa<BuiltinType>(t))
        IGM.BuiltinTypes.insert(t);

      // We need size/alignment information for imported value types,
      // so emit builtin descriptors for them.
      //
      // In effect they're treated like an opaque blob, which is OK
      // for now, at least until we want to import C++ types or
      // something like that.
      //
      // Classes and protocols go down a different path.
      if (auto Nominal = t->getAnyNominal())
        if (Nominal->hasClangNode()) {
          if (auto CD = dyn_cast<ClassDecl>(Nominal))
            IGM.ImportedClasses.insert(CD);
          else if (auto PD = dyn_cast<ProtocolDecl>(Nominal))
            IGM.ImportedProtocols.insert(PD);
          else
            IGM.OpaqueTypes.insert(Nominal);
        }
    });
  }

  /// Add a 32-bit relative offset to a mangled typeref string
  /// in the typeref reflection section.
  void addTypeRef(CanType type) {
    B.addRelativeAddress(IGM.getTypeRef(type));
  }

  /// Add a 32-bit relative offset to a mangled nominal type string
  /// in the typeref reflection section.
  void addNominalRef(const NominalTypeDecl *nominal) {
    if (auto proto = dyn_cast<ProtocolDecl>(nominal)) {
      IRGenMangler mangler;
      SymbolicMangling mangledStr;
      mangledStr.String = mangler.mangleBareProtocol(proto);
      auto mangledName = IGM.getAddrOfStringForTypeRef(mangledStr);
      B.addRelativeAddress(mangledName);
    } else {
      CanType type = nominal->getDeclaredType()->getCanonicalType();
      B.addRelativeAddress(IGM.getTypeRef(type));
    }
  }

  llvm::GlobalVariable *emit(Optional<LinkEntity> entity,
                             const char *section) {
    layout();

    llvm::GlobalVariable *var;

    // Some reflection records have a mangled symbol name, for uniquing
    // imported type metadata.
    if (entity) {
      auto info = LinkInfo::get(IGM, *entity, ForDefinition);

      auto init = B.finishAndCreateFuture();

      var = createVariable(IGM, info, init.getType(), Alignment(4));
      var->setConstant(true);
      init.installInGlobal(var);

    // Others, such as capture descriptors, do not have a name.
    } else {
      var = B.finishAndCreateGlobal("\x01l__swift4_reflection_descriptor",
                                    Alignment(4), /*isConstant*/ true,
                                    llvm::GlobalValue::PrivateLinkage);
    }

    var->setSection(section);

    IGM.addUsedGlobal(var);

    disableAddressSanitizer(IGM, var);

    return var;
  }

  virtual void layout() = 0;
};

class AssociatedTypeMetadataBuilder : public ReflectionMetadataBuilder {
  static const uint32_t AssociatedTypeRecordSize = 8;

  const ProtocolConformance *Conformance;
  ArrayRef<std::pair<StringRef, CanType>> AssociatedTypes;

  void layout() override {
    // If the conforming type is generic, we just want to emit the
    // unbound generic type here.
    auto *Nominal = Conformance->getType()->getAnyNominal();
    assert(Nominal && "Structural conformance?");

    PrettyStackTraceDecl DebugStack("emitting associated type metadata",
                                    Nominal);

    addTypeRef(Nominal->getDeclaredType()->getCanonicalType());
    addNominalRef(Conformance->getProtocol());

    B.addInt32(AssociatedTypes.size());
    B.addInt32(AssociatedTypeRecordSize);

    for (auto AssocTy : AssociatedTypes) {
      auto NameGlobal = IGM.getAddrOfFieldName(AssocTy.first);
      B.addRelativeAddress(NameGlobal);
      addBuiltinTypeRefs(AssocTy.second);
      addTypeRef(AssocTy.second);
    }
  }

public:
  AssociatedTypeMetadataBuilder(IRGenModule &IGM,
                        const ProtocolConformance *Conformance,
                        ArrayRef<std::pair<StringRef, CanType>> AssociatedTypes)
    : ReflectionMetadataBuilder(IGM), Conformance(Conformance),
      AssociatedTypes(AssociatedTypes) {}

  llvm::GlobalVariable *emit() {
    auto entity = LinkEntity::forReflectionAssociatedTypeDescriptor(Conformance);
    auto section = IGM.getAssociatedTypeMetadataSectionName();
    return ReflectionMetadataBuilder::emit(entity, section);
  }
};

class FieldTypeMetadataBuilder : public ReflectionMetadataBuilder {
  const uint32_t fieldRecordSize = 12;
  const NominalTypeDecl *NTD;

  void addFieldDecl(const ValueDecl *value, CanType type,
                    bool indirect=false) {
    reflection::FieldRecordFlags flags;
    flags.setIsIndirectCase(indirect);
    if (auto var = dyn_cast<VarDecl>(value))
      flags.setIsVar(!var->isLet());

    B.addInt32(flags.getRawValue());

    if (!type) {
      B.addInt32(0);
    } else {
      addTypeRef(type);
      addBuiltinTypeRefs(type);
    }

    if (IGM.IRGen.Opts.EnableReflectionNames) {
      auto name = value->getBaseName().getIdentifier().str();
      auto fieldName = IGM.getAddrOfFieldName(name);
      B.addRelativeAddress(fieldName);
    } else {
      B.addInt32(0);
    }
  }

  void layoutRecord() {
    auto kind = FieldDescriptorKind::Struct;

    if (auto CD = dyn_cast<ClassDecl>(NTD)) {
      auto type = CD->getDeclaredType()->getCanonicalType();
      auto RC = getReferenceCountingForType(IGM, type);
      if (RC == ReferenceCounting::ObjC)
        kind = FieldDescriptorKind::ObjCClass;
      else
        kind = FieldDescriptorKind::Class;
    }

    B.addInt16(uint16_t(kind));
    B.addInt16(fieldRecordSize);

    // Imported classes don't need field descriptors
    if (NTD->hasClangNode() && isa<ClassDecl>(NTD)) {
      B.addInt32(0);
      return;
    }

    assert(!NTD->hasClangNode() || isa<StructDecl>(NTD));

    auto properties = NTD->getStoredProperties();
    B.addInt32(std::distance(properties.begin(), properties.end()));
    for (auto property : properties)
      addFieldDecl(property,
                   property->getInterfaceType()
                       ->getCanonicalType());
  }

  void layoutEnum() {
    auto enumDecl = cast<EnumDecl>(NTD);
    auto &strategy = irgen::getEnumImplStrategy(
        IGM, enumDecl->getDeclaredTypeInContext()
                     ->getCanonicalType());

    auto kind = FieldDescriptorKind::Enum;

    // If this is a fixed-size multi-payload enum, we have to emit a descriptor
    // with the size and alignment of the type, because the reflection library
    // cannot derive this information at runtime.
    if (strategy.getElementsWithPayload().size() > 1 &&
        !strategy.needsPayloadSizeInMetadata()) {
      kind = FieldDescriptorKind::MultiPayloadEnum;
      IGM.OpaqueTypes.insert(enumDecl);
    }

    B.addInt16(uint16_t(kind));
    B.addInt16(fieldRecordSize);
    B.addInt32(strategy.getElementsWithPayload().size()
               + strategy.getElementsWithNoPayload().size());

    for (auto enumCase : strategy.getElementsWithPayload()) {
      bool indirect = (enumCase.decl->isIndirect() ||
                       enumDecl->isIndirect());
      addFieldDecl(enumCase.decl,
                   enumCase.decl->getArgumentInterfaceType()
                                ->getCanonicalType(),
                   indirect);
    }

    for (auto enumCase : strategy.getElementsWithNoPayload()) {
      addFieldDecl(enumCase.decl, CanType());
    }
  }

  void layoutProtocol() {
    auto protocolDecl = cast<ProtocolDecl>(NTD);
    FieldDescriptorKind Kind;
    if (protocolDecl->isObjC())
      Kind = FieldDescriptorKind::ObjCProtocol;
    else if (protocolDecl->requiresClass())
      Kind = FieldDescriptorKind::ClassProtocol;
    else
      Kind = FieldDescriptorKind::Protocol;
    B.addInt16(uint16_t(Kind));
    B.addInt16(fieldRecordSize);
    B.addInt32(0);
  }

  void layout() override {
    if (NTD->hasClangNode() &&
        !isa<ClassDecl>(NTD) &&
        !isa<StructDecl>(NTD) &&
        !isa<ProtocolDecl>(NTD))
      return;

    PrettyStackTraceDecl DebugStack("emitting field type metadata", NTD);
    addNominalRef(NTD);

    auto *CD = dyn_cast<ClassDecl>(NTD);
    if (CD && CD->getSuperclass()) {
      addTypeRef(CD->getSuperclass()->getCanonicalType());
    } else {
      B.addInt32(0);
    }

    switch (NTD->getKind()) {
    case DeclKind::Class:
    case DeclKind::Struct:
      layoutRecord();
      break;

    case DeclKind::Enum:
      layoutEnum();
      break;

    case DeclKind::Protocol:
      layoutProtocol();
      break;

    default:
      llvm_unreachable("Not a nominal type");
      break;
    }
  }

public:
  FieldTypeMetadataBuilder(IRGenModule &IGM,
                           const NominalTypeDecl * NTD)
    : ReflectionMetadataBuilder(IGM), NTD(NTD) {}

  llvm::GlobalVariable *emit() {
    auto entity = LinkEntity::forReflectionFieldDescriptor(
        NTD->getDeclaredType()->getCanonicalType());
    auto section = IGM.getFieldTypeMetadataSectionName();
    return ReflectionMetadataBuilder::emit(entity, section);
  }
};

class FixedTypeMetadataBuilder : public ReflectionMetadataBuilder {
  ModuleDecl *module;
  CanType type;
  const FixedTypeInfo *ti;

public:
  FixedTypeMetadataBuilder(IRGenModule &IGM,
                           CanType builtinType)
    : ReflectionMetadataBuilder(IGM) {
    module = builtinType->getASTContext().TheBuiltinModule;
    type = builtinType;
    ti = &cast<FixedTypeInfo>(IGM.getTypeInfoForUnlowered(builtinType));
  }

  FixedTypeMetadataBuilder(IRGenModule &IGM,
                           const NominalTypeDecl *nominalDecl)
    : ReflectionMetadataBuilder(IGM) {
    module = nominalDecl->getParentModule();
    type = nominalDecl->getDeclaredType()->getCanonicalType();
    ti = &cast<FixedTypeInfo>(IGM.getTypeInfoForUnlowered(
        nominalDecl->getDeclaredTypeInContext()->getCanonicalType()));
  }
  
  void layout() override {
    addTypeRef(type);

    B.addInt32(ti->getFixedSize().getValue());
    B.addInt32(ti->getFixedAlignment().getValue());
    B.addInt32(ti->getFixedStride().getValue());
    B.addInt32(ti->getFixedExtraInhabitantCount(IGM));
  }

  llvm::GlobalVariable *emit() {
    auto entity = LinkEntity::forReflectionBuiltinDescriptor(type);
    auto section = IGM.getBuiltinTypeMetadataSectionName();
    return ReflectionMetadataBuilder::emit(entity, section);
  }
};

void IRGenModule::emitBuiltinTypeMetadataRecord(CanType builtinType) {
  FixedTypeMetadataBuilder builder(*this, builtinType);
  builder.emit();
}

void IRGenModule::emitOpaqueTypeMetadataRecord(const NominalTypeDecl *nominalDecl) {
  FixedTypeMetadataBuilder builder(*this, nominalDecl);
  builder.emit();
}

bool IRGenModule::shouldEmitOpaqueTypeMetadataRecord(
    const NominalTypeDecl *nominalDecl) {
  if (nominalDecl->getAttrs().hasAttribute<AlignmentAttr>()) {
    auto &ti = getTypeInfoForUnlowered(nominalDecl->getDeclaredTypeInContext());
    if (isa<FixedTypeInfo>(ti))
      return true;
  }

  return false;
}

/// Builds a constant LLVM struct describing the layout of a fixed-size
/// SIL @box. These look like closure contexts, but without any necessary
/// bindings or metadata sources, and only a single captured value.
class BoxDescriptorBuilder : public ReflectionMetadataBuilder {
  CanType BoxedType;
public:
  BoxDescriptorBuilder(IRGenModule &IGM, CanType BoxedType)
    : ReflectionMetadataBuilder(IGM), BoxedType(BoxedType) {}
  
  void layout() override {
    B.addInt32(1);
    B.addInt32(0); // Number of sources
    B.addInt32(0); // Number of generic bindings

    addTypeRef(BoxedType);
    addBuiltinTypeRefs(BoxedType);
  }

  llvm::GlobalVariable *emit() {
    auto section = IGM.getCaptureDescriptorMetadataSectionName();
    return ReflectionMetadataBuilder::emit(None, section);
  }
};

/// Builds a constant LLVM struct describing the layout of a heap closure,
/// the types of its captures, and the sources of metadata if any of the
/// captures are generic.
class CaptureDescriptorBuilder : public ReflectionMetadataBuilder {
  swift::reflection::MetadataSourceBuilder SourceBuilder;
  CanSILFunctionType OrigCalleeType;
  CanSILFunctionType SubstCalleeType;
  SubstitutionList Subs;
  const HeapLayout &Layout;

public:
  CaptureDescriptorBuilder(IRGenModule &IGM,
                           CanSILFunctionType OrigCalleeType,
                           CanSILFunctionType SubstCalleeType,
                           SubstitutionList Subs,
                           const HeapLayout &Layout)
    : ReflectionMetadataBuilder(IGM),
      OrigCalleeType(OrigCalleeType),
      SubstCalleeType(SubstCalleeType), Subs(Subs),
      Layout(Layout) {}

  using MetadataSourceMap
    = std::vector<std::pair<CanType, const reflection::MetadataSource*>>;

  void addMetadataSource(const reflection::MetadataSource *Source) {
    if (Source == nullptr) {
      B.addInt32(0);
    } else {
      SmallString<16> EncodeBuffer;
      llvm::raw_svector_ostream OS(EncodeBuffer);
      MetadataSourceEncoder Encoder(OS);
      Encoder.visit(Source);

      auto EncodedSource = IGM.getAddrOfStringForTypeRef(OS.str());
      B.addRelativeAddress(EncodedSource);
    }
  }

  /// Give up if we captured an opened existential type. Eventually we
  /// should figure out how to represent this.
  static bool hasOpenedExistential(CanSILFunctionType OrigCalleeType,
                                   const HeapLayout &Layout) {
    if (!OrigCalleeType->isPolymorphic() ||
        OrigCalleeType->isPseudogeneric())
      return false;

    auto &Bindings = Layout.getBindings();
    for (unsigned i = 0; i < Bindings.size(); ++i) {
      // Skip protocol requirements (FIXME: for now?)
      if (Bindings[i].Protocol != nullptr)
        continue;

      if (Bindings[i].TypeParameter->hasOpenedExistential())
        return true;
    }

    auto ElementTypes = Layout.getElementTypes().slice(
        Layout.hasBindings() ? 1 : 0);
    for (auto ElementType : ElementTypes) {
      auto SwiftType = ElementType.getSwiftRValueType();
      if (SwiftType->hasOpenedExistential())
        return true;
    }

    return false;
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

    // Generic parameters of pseudogeneric functions do not have
    // runtime metadata.
    if (!OrigCalleeType->isPolymorphic() ||
        OrigCalleeType->isPseudogeneric())
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
      auto BindingType = Bindings[i].TypeParameter;
      auto InterfaceType = BindingType->mapTypeOutOfContext();
      SourceMap.push_back({InterfaceType->getCanonicalType(), Source});
    }

    // Check if any requirements were fulfilled by metadata stored inside a
    // captured value.

    auto SubstMap =
      OrigCalleeType->getGenericSignature()->getSubstitutionMap(Subs);

    enumerateGenericParamFulfillments(IGM, OrigCalleeType,
        [&](CanType GenericParam,
            const irgen::MetadataSource &Source,
            const MetadataPath &Path) {

      const reflection::MetadataSource *Root;
      switch (Source.getKind()) {
      case irgen::MetadataSource::Kind::SelfMetadata:
      case irgen::MetadataSource::Kind::SelfWitnessTable:
        // Handled as part of bindings
        return;

      case irgen::MetadataSource::Kind::GenericLValueMetadata:
        // FIXME?
        return;

      case irgen::MetadataSource::Kind::ClassPointer:
        Root = SourceBuilder.createReferenceCapture(Source.getParamIndex());
        break;

      case irgen::MetadataSource::Kind::Metadata:
        Root = SourceBuilder.createMetadataCapture(Source.getParamIndex());
        break;
      }

      // The metadata might be reached via a non-trivial path (eg,
      // dereferencing an isa pointer or a generic argument). Record
      // the path. We assume captured values map 1-1 with function
      // parameters.
      auto Src = Path.getMetadataSource(SourceBuilder, Root);

      auto SubstType = GenericParam.subst(SubstMap);
      auto InterfaceType = SubstType->mapTypeOutOfContext();
      SourceMap.push_back({InterfaceType->getCanonicalType(), Src});
    });

    return SourceMap;
  }

  /// Get the interface types of all of the captured values, mapped out of the
  /// context of the callee we're partially applying.
  std::vector<CanType> getCaptureTypes() {
    std::vector<CanType> CaptureTypes;

    for (auto ElementType : getElementTypes()) {
      auto SwiftType = ElementType.getSwiftRValueType();

      // Erase pseudogeneric captures down to AnyObject.
      if (OrigCalleeType->isPseudogeneric()) {
        SwiftType = SwiftType.transform([&](Type t) -> Type {
          if (auto *archetype = t->getAs<ArchetypeType>()) {
            assert(archetype->requiresClass() && "don't know what to do");
            return IGM.Context.getAnyObjectType();
          }
          return t;
        })->getCanonicalType();
      }

      auto InterfaceType = SwiftType->mapTypeOutOfContext();
      CaptureTypes.push_back(InterfaceType->getCanonicalType());
    }

    return CaptureTypes;
  }

  void layout() override {
    auto CaptureTypes = getCaptureTypes();
    auto MetadataSources = getMetadataSourceMap();

    B.addInt32(CaptureTypes.size());
    B.addInt32(MetadataSources.size());
    B.addInt32(Layout.getBindings().size());

    // Now add typerefs of all of the captures.
    for (auto CaptureType : CaptureTypes) {
      addTypeRef(CaptureType);
      addBuiltinTypeRefs(CaptureType);
    }

    // Add the pairs that make up the generic param -> metadata source map
    // to the struct.
    for (auto GenericAndSource : MetadataSources) {
      auto GenericParam = GenericAndSource.first->getCanonicalType();
      auto Source = GenericAndSource.second;

      addTypeRef(GenericParam);
      addMetadataSource(Source);
    }
  }

  llvm::GlobalVariable *emit() {
    auto section = IGM.getCaptureDescriptorMetadataSectionName();
    return ReflectionMetadataBuilder::emit(None, section);
  }
};

static std::string getReflectionSectionName(IRGenModule &IGM,
                                            StringRef LongName,
                                            StringRef FourCC) {
  SmallString<50> SectionName;
  llvm::raw_svector_ostream OS(SectionName);
  switch (IGM.TargetInfo.OutputObjectFormat) {
  case llvm::Triple::UnknownObjectFormat:
    llvm_unreachable("unknown object format");
  case llvm::Triple::COFF:
    assert(FourCC.size() <= 4 &&
           "COFF section name length must be <= 8 characters");
    OS << ".sw5" << FourCC << "$B";
    break;
  case llvm::Triple::ELF:
    OS << "swift4_" << LongName;
    break;
  case llvm::Triple::MachO:
    assert(LongName.size() <= 7 &&
           "Mach-O section name length must be <= 16 characters");
    OS << "__TEXT,__swift4_" << LongName << ", regular, no_dead_strip";
    break;
  case llvm::Triple::Wasm:
    llvm_unreachable("web assembly object format is not supported.");
    break;
  }
  return OS.str();
}

const char *IRGenModule::getFieldTypeMetadataSectionName() {
  if (FieldTypeSection.empty())
    FieldTypeSection = getReflectionSectionName(*this, "fieldmd", "flmd");
  return FieldTypeSection.c_str();
}

const char *IRGenModule::getBuiltinTypeMetadataSectionName() {
  if (BuiltinTypeSection.empty())
    BuiltinTypeSection = getReflectionSectionName(*this, "builtin", "bltn");
  return BuiltinTypeSection.c_str();
}

const char *IRGenModule::getAssociatedTypeMetadataSectionName() {
  if (AssociatedTypeSection.empty())
    AssociatedTypeSection = getReflectionSectionName(*this, "assocty", "asty");
  return AssociatedTypeSection.c_str();
}

const char *IRGenModule::getCaptureDescriptorMetadataSectionName() {
  if (CaptureDescriptorSection.empty())
    CaptureDescriptorSection = getReflectionSectionName(*this, "capture", "cptr");
  return CaptureDescriptorSection.c_str();
}

const char *IRGenModule::getReflectionStringsSectionName() {
  if (ReflectionStringsSection.empty())
    ReflectionStringsSection = getReflectionSectionName(*this, "reflstr", "rfst");
  return ReflectionStringsSection.c_str();
}

const char *IRGenModule::getReflectionTypeRefSectionName() {
  if (ReflectionTypeRefSection.empty())
    ReflectionTypeRefSection = getReflectionSectionName(*this, "typeref", "tyrf");
  return ReflectionTypeRefSection.c_str();
}

llvm::Constant *IRGenModule::getAddrOfFieldName(StringRef Name) {
  auto &entry = FieldNames[Name];
  if (entry.second)
    return entry.second;

  entry = createStringConstant(Name, /*willBeRelativelyAddressed*/ true,
                               getReflectionStringsSectionName());
  disableAddressSanitizer(*this, entry.first);
  return entry.second;
}

llvm::Constant *
IRGenModule::getAddrOfBoxDescriptor(CanType BoxedType) {
  if (!IRGen.Opts.EnableReflectionMetadata)
    return llvm::Constant::getNullValue(CaptureDescriptorPtrTy);

  BoxDescriptorBuilder builder(*this, BoxedType);
  auto var = builder.emit();

  return llvm::ConstantExpr::getBitCast(var, CaptureDescriptorPtrTy);
}

llvm::Constant *
IRGenModule::getAddrOfCaptureDescriptor(SILFunction &Caller,
                                        CanSILFunctionType OrigCalleeType,
                                        CanSILFunctionType SubstCalleeType,
                                        SubstitutionList Subs,
                                        const HeapLayout &Layout) {
  if (!IRGen.Opts.EnableReflectionMetadata)
    return llvm::Constant::getNullValue(CaptureDescriptorPtrTy);

  if (CaptureDescriptorBuilder::hasOpenedExistential(OrigCalleeType, Layout))
    return llvm::Constant::getNullValue(CaptureDescriptorPtrTy);

  CaptureDescriptorBuilder builder(*this,
                                   OrigCalleeType, SubstCalleeType, Subs,
                                   Layout);
  auto var = builder.emit();
  return llvm::ConstantExpr::getBitCast(var, CaptureDescriptorPtrTy);
}

void IRGenModule::
emitAssociatedTypeMetadataRecord(const ProtocolConformance *Conformance) {
  if (!IRGen.Opts.EnableReflectionMetadata)
    return;

  SmallVector<std::pair<StringRef, CanType>, 2> AssociatedTypes;

  auto collectTypeWitness = [&](const AssociatedTypeDecl *AssocTy,
                                Type Replacement,
                                const TypeDecl *TD) -> bool {
    AssociatedTypes.push_back({
      AssocTy->getNameStr(),
      Replacement->getCanonicalType()
    });
    return false;
  };

  Conformance->forEachTypeWitness(/*resolver*/ nullptr, collectTypeWitness);

  // If there are no associated types, don't bother emitting any
  // metadata.
  if (AssociatedTypes.empty())
    return;

  AssociatedTypeMetadataBuilder builder(*this, Conformance, AssociatedTypes);
  builder.emit();
}

void IRGenModule::emitBuiltinReflectionMetadata() {
  if (getSwiftModule()->isStdlibModule()) {
    BuiltinTypes.insert(Context.TheNativeObjectType);
    BuiltinTypes.insert(Context.TheUnknownObjectType);
    BuiltinTypes.insert(Context.TheBridgeObjectType);
    BuiltinTypes.insert(Context.TheRawPointerType);
    BuiltinTypes.insert(Context.TheUnsafeValueBufferType);

    // This would not be necessary if RawPointer had the same set of
    // extra inhabitants as these. But maybe it's best not to codify
    // that in the ABI anyway.
    CanType thinFunction = CanFunctionType::get(
      AnyFunctionType::CanParamArrayRef(), Context.TheEmptyTupleType,
      AnyFunctionType::ExtInfo().withRepresentation(
          FunctionTypeRepresentation::Thin));
    BuiltinTypes.insert(thinFunction);

    CanType anyMetatype = CanExistentialMetatypeType::get(
      Context.TheAnyType);
    BuiltinTypes.insert(anyMetatype);
  }

  for (auto CD : ImportedClasses)
    emitFieldMetadataRecord(CD);

  for (auto PD : ImportedProtocols)
    emitFieldMetadataRecord(PD);

  for (auto SD : ImportedStructs)
    emitFieldMetadataRecord(SD);

  for (auto builtinType : BuiltinTypes)
    emitBuiltinTypeMetadataRecord(builtinType);

  for (auto nominalDecl : OpaqueTypes)
    emitOpaqueTypeMetadataRecord(nominalDecl);
}

void IRGenerator::emitBuiltinReflectionMetadata() {
  for (auto &m : *this) {
    m.second->emitBuiltinReflectionMetadata();
  }
}

void IRGenModule::emitFieldMetadataRecord(const NominalTypeDecl *Decl) {
  if (!IRGen.Opts.EnableReflectionMetadata)
    return;

  // @objc enums never have generic parameters or payloads,
  // and lower as their raw type.
  if (auto *ED = dyn_cast<EnumDecl>(Decl))
    if (ED->isObjC()) {
      emitOpaqueTypeMetadataRecord(ED);
      return;
    }

  FieldTypeMetadataBuilder builder(*this, Decl);
  FieldDescriptors.push_back(builder.emit());
}

void IRGenModule::emitReflectionMetadataVersion() {
  auto Init =
    llvm::ConstantInt::get(Int16Ty, SWIFT_REFLECTION_METADATA_VERSION);
  auto Version = new llvm::GlobalVariable(Module, Int16Ty, /*constant*/ true,
                                          llvm::GlobalValue::LinkOnceODRLinkage,
                                          Init,
                                          "__swift_reflection_version");
  Version->setVisibility(llvm::GlobalValue::HiddenVisibility);
  addUsedGlobal(Version);
}

void IRGenerator::emitReflectionMetadataVersion() {
  for (auto &m : *this) {
    m.second->emitReflectionMetadataVersion();
  }
}
