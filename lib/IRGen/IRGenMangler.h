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

namespace swift {

class ProtocolConformance;
class NormalProtocolConformance;

namespace irgen {

/// A mangling string that includes embedded symbolic references.
struct SymbolicMangling {
  std::string String;
  std::vector<std::pair<const DeclContext *, unsigned>> SymbolicReferences;
};

/// The mangler for all kind of symbols produced in IRGen.
class IRGenMangler : public Mangle::ASTMangler {
public:
  IRGenMangler() { }

  std::string mangleValueWitness(Type type, ValueWitness witness);

  std::string mangleValueWitnessTable(Type type) {
    return mangleTypeSymbol(type, "WV");
  }

  std::string mangleTypeMetadataAccessFunction(Type type) {
    return mangleTypeSymbol(type, "Ma");
  }

  std::string mangleTypeMetadataLazyCacheVariable(Type type) {
    return mangleTypeSymbol(type, "ML");
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

  std::string mangleClassMetadataBaseOffset(const ClassDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mo");
  }

  std::string mangleNominalTypeDescriptor(const NominalTypeDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mn");
  }

  std::string mangleTypeMetadataInstantiationCache(const NominalTypeDecl *Decl){
    return mangleNominalTypeSymbol(Decl, "MI");
  }

  std::string mangleTypeMetadataInstantiationFunction(
                                                  const NominalTypeDecl *Decl) {
    return mangleNominalTypeSymbol(Decl, "Mi");
  }

  std::string mangleTypeMetadataCompletionFunction(const NominalTypeDecl *Decl){
    return mangleNominalTypeSymbol(Decl, "Mr");
  }
  
  std::string mangleModuleDescriptor(const ModuleDecl *Decl) {
    beginMangling();
    appendContext(Decl);
    appendOperator("MXM");
    return finalize();
  }
  
  std::string mangleExtensionDescriptor(const ExtensionDecl *Decl) {
    beginMangling();
    appendContext(Decl);
    appendOperator("MXE");
    return finalize();
  }
  
  std::string mangleAnonymousDescriptor(const DeclContext *DC) {
    beginMangling();
    appendContext(DC);
    appendOperator("MXX");
    return finalize();
  }
  
  std::string mangleBareProtocol(const ProtocolDecl *Decl) {
    beginMangling();
    appendProtocolName(Decl);
    appendOperator("P");
    return finalize();
  }

  std::string mangleProtocolDescriptor(const ProtocolDecl *Decl) {
    beginMangling();
    appendProtocolName(Decl);
    appendOperator("Mp");
    return finalize();
  }

  std::string mangleProtocolRequirementArray(const ProtocolDecl *Decl) {
    beginMangling();
    appendProtocolName(Decl);
    appendOperator("WR");
    return finalize();
  }

  std::string mangleProtocolConformanceDescriptor(
                                 const ProtocolConformance *Conformance) {
    beginMangling();
    appendProtocolConformance(Conformance);
    appendOperator("Mc");
    return finalize();
  }
  
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

  std::string mangleDirectProtocolWitnessTable(const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "WP");
  }

  std::string mangleProtocolWitnessTablePattern(const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "Wp");
  }

  std::string mangleGenericProtocolWitnessTableCache(
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "WG");
  }

  std::string mangleGenericProtocolWitnessTableInstantiationFunction(
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "WI");
  }

  std::string mangleResilientProtocolWitnessTable(
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "Wr");
  }

  std::string mangleProtocolWitnessTableAccessFunction(
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(Type(), C, "Wa");
  }

  std::string mangleProtocolWitnessTableLazyAccessFunction(Type type,
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(type, C, "Wl");
  }

  std::string mangleProtocolWitnessTableLazyCacheVariable(Type type,
                                                const ProtocolConformance *C) {
    return mangleConformanceSymbol(type, C, "WL");
  }

  std::string mangleAssociatedTypeMetadataAccessFunction(
                                      const ProtocolConformance *Conformance,
                                      StringRef AssocTyName) {
    beginMangling();
    appendProtocolConformance(Conformance);
    appendIdentifier(AssocTyName);
    appendOperator("Wt");
    return finalize();
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

  std::string mangleAssociatedTypeGenericParamRef(unsigned baseOrdinal,
                                                  CanType member) {
    beginMangling();
    bool isFirstAssociatedTypeIdentifier = true;
    appendType(GenericTypeParamType::get(0, baseOrdinal,
                                         member->getASTContext()));
    appendAssociatedTypePath(member, isFirstAssociatedTypeIdentifier);
    appendOperator("MXA");
    return finalize();
  }

  void appendAssociatedTypePath(CanType associatedType, bool &isFirst) {
    if (auto memberType = dyn_cast<DependentMemberType>(associatedType)) {
      appendAssociatedTypePath(memberType.getBase(), isFirst);
      appendIdentifier(memberType->getName().str());
      appendListSeparator(isFirst);
    } else {
      assert(isa<GenericTypeParamType>(associatedType));
    }
  }

  std::string mangleCoroutineContinuationPrototype(CanSILFunctionType type) {
    return mangleTypeSymbol(type, "TC");
  }

  std::string mangleReflectionBuiltinDescriptor(Type type) {
    return mangleTypeSymbol(type, "MB");
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
  
  std::string mangleForProtocolDescriptor(ProtocolType *Proto) {
    beginMangling();
    appendProtocolName(Proto->getDecl());
    appendOperator("P");
    return finalize();
  }

  std::string mangleTypeForForeignMetadataUniquing(Type type) {
    return mangleTypeWithoutPrefix(type);
  }

  SymbolicMangling mangleTypeForReflection(IRGenModule &IGM,
                                           Type Ty);

  std::string mangleTypeForLLVMTypeName(CanType Ty);

  std::string mangleProtocolForLLVMTypeName(ProtocolCompositionType *type);

  std::string mangleSymbolNameForSymbolicMangling(
                                              const SymbolicMangling &mangling);
protected:

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
