//===--- IRGenMangler.h - mangling of IRGen symbols -------------*- C++ -*-===//
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

#ifndef SWIFT_IRGEN_IRGENMANGLER_H
#define SWIFT_IRGEN_IRGENMANGLER_H

#include "IRGenModule.h"
#include "swift/AST/ASTMangler.h"
#include "swift/IRGen/ValueWitness.h"
#include "llvm/Support/SaveAndRestore.h"

namespace swift {

class ProtocolConformance;
class NormalProtocolConformance;

namespace irgen {

enum class MangledTypeRefRole;

/// A mangling string that includes embedded symbolic references.
struct SymbolicMangling {
  std::string String;
  std::vector<std::pair<Mangle::ASTMangler::SymbolicReferent, unsigned>>
    SymbolicReferences;
  
  unsigned runtimeSizeInBytes() const {
    return String.size();
  }
};

/// The mangler for all kind of symbols produced in IRGen.
class IRGenMangler : public Mangle::ASTMangler {
public:
  IRGenMangler() { }

  std::string mangleDispatchThunk(const FuncDecl *func) {
    beginMangling();
    appendEntity(func);
    appendOperator("Tj");
    return finalize();
  }

  std::string mangleConstructorDispatchThunk(const ConstructorDecl *ctor,
                                             bool isAllocating) {
    beginMangling();
    appendConstructorEntity(ctor, isAllocating);
    appendOperator("Tj");
    return finalize();
  }

  std::string mangleMethodDescriptor(const FuncDecl *func) {
    beginMangling();
    appendEntity(func);
    appendOperator("Tq");
    return finalize();
  }

  std::string mangleConstructorMethodDescriptor(const ConstructorDecl *ctor,
                                                bool isAllocating) {
    beginMangling();
    appendConstructorEntity(ctor, isAllocating);
    appendOperator("Tq");
    return finalize();
  }

  std::string mangleMethodLookupFunction(const ClassDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mu");
  }

  std::string mangleValueWitness(Type type, ValueWitness witness);

  std::string mangleValueWitnessTable(Type type) {
    const char * const witnessTableOp = "WV";
    if (type->isAnyObject()) {
      // Special-case for AnyObject, whose witness table is under the old name
      // Builtin.UnknownObject, even though we don't use that as a Type anymore.
      beginMangling();
      appendOperator("BO");
      appendOperator(witnessTableOp);
      return finalize();
    }
    return mangleTypeSymbol(type, witnessTableOp);
  }

  std::string mangleTypeMetadataAccessFunction(Type type) {
    return mangleTypeSymbol(type, "Ma");
  }

  std::string mangleTypeMetadataLazyCacheVariable(Type type) {
    return mangleTypeSymbol(type, "ML");
  }

  std::string mangleTypeMetadataDemanglingCacheVariable(Type type) {
    return mangleTypeSymbol(type, "MD");
  }

  std::string mangleTypeFullMetadataFull(Type type) {
    return mangleTypeSymbol(type, "Mf");
  }

  std::string mangleTypeMetadataFull(Type type) {
    return mangleTypeSymbol(type, "N");
  }

  std::string mangleTypeMetadataPattern(const NominalTypeDecl *decl) {
    return mangleNominalTypeSymbol(decl, "MP");
  }

  std::string mangleClassMetaClass(const ClassDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mm");
  }

  std::string mangleObjCMetadataUpdateFunction(const ClassDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "MU");
  }

  std::string mangleObjCResilientClassStub(const ClassDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Ms");
  }

  std::string mangleFullObjCResilientClassStub(const ClassDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mt");
  }

  std::string mangleClassMetadataBaseOffset(const ClassDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mo");
  }

  std::string mangleNominalTypeDescriptor(const NominalTypeDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mn");
  }
  
  std::string mangleOpaqueTypeDescriptorAccessor(const OpaqueTypeDecl *decl) {
    beginMangling();
    appendOpaqueDeclName(decl);
    appendOperator("Mg");
    return finalize();
  }

  std::string
  mangleOpaqueTypeDescriptorAccessorImpl(const OpaqueTypeDecl *decl) {
    beginMangling();
    appendOpaqueDeclName(decl);
    appendOperator("Mh");
    return finalize();
  }

  std::string
  mangleOpaqueTypeDescriptorAccessorKey(const OpaqueTypeDecl *decl) {
    beginMangling();
    appendOpaqueDeclName(decl);
    appendOperator("Mj");
    return finalize();
  }

  std::string
  mangleOpaqueTypeDescriptorAccessorVar(const OpaqueTypeDecl *decl) {
    beginMangling();
    appendOpaqueDeclName(decl);
    appendOperator("Mk");
    return finalize();
  }

  std::string mangleTypeMetadataInstantiationCache(const NominalTypeDecl *Decl){
    return mangleNominalTypeSymbol(Decl, "MI");
  }

  std::string mangleTypeMetadataInstantiationFunction(
                                                  const NominalTypeDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mi");
  }

  std::string mangleTypeMetadataSingletonInitializationCache(
                                                  const NominalTypeDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Ml");
  }

  std::string mangleTypeMetadataCompletionFunction(const NominalTypeDecl *Decl){
    return mangleNominalTypeSymbol(Decl, "Mr");
  }
  
  std::string mangleModuleDescriptor(const ModuleDecl *Decl) {
    beginMangling();
    appendContext(Decl, Decl->getAlternateModuleName());
    appendOperator("MXM");
    return finalize();
  }
  
  std::string mangleExtensionDescriptor(const ExtensionDecl *Decl) {
    beginMangling();
    appendContext(Decl, Decl->getAlternateModuleName());
    appendOperator("MXE");
    return finalize();
  }
  
  void appendAnonymousDescriptorName(PointerUnion<DeclContext*, VarDecl*> Name){
    if (auto DC = Name.dyn_cast<DeclContext *>()) {
      return appendContext(DC, StringRef());
    }
    if (auto VD = Name.dyn_cast<VarDecl *>()) {
      return appendEntity(VD);
    }
    llvm_unreachable("unknown kind");
  }
  
  std::string mangleAnonymousDescriptorName(
                                    PointerUnion<DeclContext*, VarDecl*> Name) {
    beginMangling();
    appendAnonymousDescriptorName(Name);
    return finalize();
  }
  
  std::string mangleAnonymousDescriptor(
                                    PointerUnion<DeclContext*, VarDecl*> Name) {
    beginMangling();
    appendAnonymousDescriptorName(Name);
    appendOperator("MXX");
    return finalize();
  }

  std::string mangleBareProtocol(const ProtocolDecl *Decl) {
    beginMangling();
    appendAnyGenericType(Decl);
    return finalize();
  }

  std::string mangleProtocolDescriptor(const ProtocolDecl *Decl) {
    beginMangling();
    appendProtocolName(Decl);
    appendOperator("Mp");
    return finalize();
  }

  std::string mangleProtocolRequirementsBaseDescriptor(
                                                    const ProtocolDecl *Decl) {
    beginMangling();
    appendProtocolName(Decl);
    appendOperator("TL");
    return finalize();
  }

  std::string mangleAssociatedTypeDescriptor(
                                         const AssociatedTypeDecl *assocType) {
    // Don't optimize away the protocol name, because we need it to distinguish
    // among the type descriptors of different protocols.
    llvm::SaveAndRestore<bool> optimizeProtocolNames(OptimizeProtocolNames,
                                                     false);
    beginMangling();
    bool isAssocTypeAtDepth = false;
    (void)appendAssocType(
        assocType->getDeclaredInterfaceType()->castTo<DependentMemberType>(),
        isAssocTypeAtDepth);
    appendOperator("Tl");
    return finalize();
  }

  std::string mangleAssociatedConformanceDescriptor(
      const ProtocolDecl *proto,
      CanType subject,
      const ProtocolDecl *requirement) {
    beginMangling();
    appendAnyGenericType(proto);
    if (isa<GenericTypeParamType>(subject)) {
      appendType(subject);
    } else {
      bool isFirstAssociatedTypeIdentifier = true;
      appendAssociatedTypePath(subject, isFirstAssociatedTypeIdentifier);
    }
    appendProtocolName(requirement);
    appendOperator("Tn");
    return finalize();
  }

  std::string mangleBaseConformanceDescriptor(
      const ProtocolDecl *proto,
      const ProtocolDecl *requirement) {
    beginMangling();
    appendAnyGenericType(proto);
    appendProtocolName(requirement);
    appendOperator("Tb");
    return finalize();
  }

  std::string mangleDefaultAssociatedConformanceAccessor(
      const ProtocolDecl *proto,
      CanType subject,
      const ProtocolDecl *requirement) {
    beginMangling();
    appendAnyGenericType(proto);
    bool isFirstAssociatedTypeIdentifier = true;
    appendAssociatedTypePath(subject, isFirstAssociatedTypeIdentifier);
    appendProtocolName(requirement);
    appendOperator("TN");
    return finalize();
  }

  std::string mangleProtocolConformanceDescriptor(
                                   const RootProtocolConformance *conformance);
  
  std::string manglePropertyDescriptor(const AbstractStorageDecl *storage) {
    beginMangling();
    appendEntity(storage);
    appendOperator("MV");
    return finalize();
  }

  std::string mangleFieldOffset(const ValueDecl *Decl) {
    beginMangling();
    appendEntity(Decl);
    appendOperator("Wvd");
    return finalize();
  }

  std::string mangleEnumCase(const ValueDecl *Decl) {
    beginMangling();
    appendEntity(Decl);
    appendOperator("WC");
    return finalize();
  }

  std::string mangleProtocolWitnessTablePattern(const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "Wp");
  }

  std::string mangleGenericProtocolWitnessTableInstantiationFunction(
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "WI");
  }

  std::string mangleProtocolWitnessTableLazyAccessFunction(Type type,
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(type, C, "Wl");
  }

  std::string mangleProtocolWitnessTableLazyCacheVariable(Type type,
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(type, C, "WL");
  }

  std::string mangleAssociatedTypeWitnessTableAccessFunction(
                                      const ProtocolConformance *Conformance,
                                      CanType AssociatedType,
                                      const ProtocolDecl *Proto) {
    beginMangling();
    appendProtocolConformance(Conformance);
    bool isFirstAssociatedTypeIdentifier = true;
    appendAssociatedTypePath(AssociatedType, isFirstAssociatedTypeIdentifier);
    appendAnyGenericType(Proto);
    appendOperator("WT");
    return finalize();
  }

  std::string mangleBaseWitnessTableAccessFunction(
                                      const ProtocolConformance *Conformance,
                                      const ProtocolDecl *BaseProto) {
    beginMangling();
    appendProtocolConformance(Conformance);
    appendAnyGenericType(BaseProto);
    appendOperator("Wb");
    return finalize();
  }

  void appendAssociatedTypePath(CanType associatedType, bool &isFirst) {
    if (auto memberType = dyn_cast<DependentMemberType>(associatedType)) {
      appendAssociatedTypePath(memberType.getBase(), isFirst);
      appendAssociatedTypeName(memberType);
      appendListSeparator(isFirst);
    } else {
      assert(isa<GenericTypeParamType>(associatedType));
    }
  }

  std::string mangleCoroutineContinuationPrototype(CanSILFunctionType type) {
    return mangleTypeSymbol(type, "TC");
  }

  std::string mangleReflectionBuiltinDescriptor(Type type) {
    const char * const reflectionDescriptorOp = "MB";
    if (type->isAnyObject()) {
      // Special-case for AnyObject, whose reflection descriptor is under the
      // old name Builtin.UnknownObject, even though we don't use that as a Type
      // anymore.
      beginMangling();
      appendOperator("BO");
      appendOperator(reflectionDescriptorOp);
      return finalize();
    }
    return mangleTypeSymbol(type, reflectionDescriptorOp);
  }

  std::string mangleReflectionFieldDescriptor(Type type) {
    return mangleTypeSymbol(type, "MF");
  }

  std::string mangleReflectionAssociatedTypeDescriptor(
                                                 const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "MA");
  }

  std::string mangleOutlinedCopyFunction(CanType ty,
                                         CanGenericSignature sig) {
    beginMangling();
    appendType(ty);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOy");
    return finalize();
  }
  std::string mangleOutlinedConsumeFunction(CanType ty,
                                            CanGenericSignature sig) {
    beginMangling();
    appendType(ty);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOe");
    return finalize();
  }

  std::string mangleOutlinedRetainFunction(CanType t,
                                           CanGenericSignature sig) {
    beginMangling();
    appendType(t);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOr");
    return finalize();
  }
  std::string mangleOutlinedReleaseFunction(CanType t,
                                            CanGenericSignature sig) {
    beginMangling();
    appendType(t);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOs");
    return finalize();
  }

  std::string mangleOutlinedInitializeWithTakeFunction(CanType t,
                                                       CanGenericSignature sig) {
    beginMangling();
    appendType(t);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOb");
    return finalize();
  }
  std::string mangleOutlinedInitializeWithCopyFunction(CanType t,
                                                       CanGenericSignature sig) {
    beginMangling();
    appendType(t);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOc");
    return finalize();
  }
  std::string mangleOutlinedAssignWithTakeFunction(CanType t,
                                                   CanGenericSignature sig) {
    beginMangling();
    appendType(t);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOd");
    return finalize();
  }
  std::string mangleOutlinedAssignWithCopyFunction(CanType t,
                                                   CanGenericSignature sig) {
    beginMangling();
    appendType(t);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOf");
    return finalize();
  }
  std::string mangleOutlinedDestroyFunction(CanType t,
                                            CanGenericSignature sig) {
    beginMangling();
    appendType(t);
    if (sig)
      appendGenericSignature(sig);
    appendOperator("WOh");
    return finalize();
  }

  std::string manglePartialApplyForwarder(StringRef FuncName);
  
  std::string mangleTypeForForeignMetadataUniquing(Type type) {
    return mangleTypeWithoutPrefix(type);
  }

  SymbolicMangling mangleTypeForReflection(IRGenModule &IGM,
                                           Type Ty);
  
  SymbolicMangling mangleProtocolConformanceForReflection(IRGenModule &IGM,
                                            Type Ty,
                                            ProtocolConformanceRef conformance);

  std::string mangleTypeForLLVMTypeName(CanType Ty);

  std::string mangleProtocolForLLVMTypeName(ProtocolCompositionType *type);

  std::string mangleSymbolNameForSymbolicMangling(
                                              const SymbolicMangling &mangling,
                                              MangledTypeRefRole role);

  std::string mangleSymbolNameForAssociatedConformanceWitness(
                                  const NormalProtocolConformance *conformance,
                                  CanType associatedType,
                                  const ProtocolDecl *proto);

  std::string mangleSymbolNameForMangledMetadataAccessorString(
                                           const char *kind,
                                           CanGenericSignature genericSig,
                                           CanType type);

  std::string mangleSymbolNameForMangledConformanceAccessorString(
                                           const char *kind,
                                           CanGenericSignature genericSig,
                                           CanType type,
                                           ProtocolConformanceRef conformance);

  std::string mangleSymbolNameForGenericEnvironment(
                                                CanGenericSignature genericSig);
protected:
  SymbolicMangling
  withSymbolicReferences(IRGenModule &IGM,
                         llvm::function_ref<void ()> body);

  std::string mangleTypeSymbol(Type type, const char *Op) {
    beginMangling();
    appendType(type);
    appendOperator(Op);
    return finalize();
  }

  std::string mangleNominalTypeSymbol(const NominalTypeDecl *Decl,
                                      const char *Op) {
    beginMangling();
    appendAnyGenericType(Decl);
    appendOperator(Op);
    return finalize();
  }

  std::string mangleConformanceSymbol(Type type,
                                      const ProtocolConformance *Conformance,
                                      const char *Op) {
    beginMangling();
    if (type)
      appendType(type);
    appendProtocolConformance(Conformance);
    appendOperator(Op);
    return finalize();
  }
};

} // end namespace irgen
} // end namespace swift

#endif
