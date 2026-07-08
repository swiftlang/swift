//===--- PrintClangExistentialType.cpp - Print existential types -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "PrintClangExistentialType.h"
#include "ClangSyntaxPrinter.h"
#include "DeclAndTypePrinter.h"
#include "PrimitiveTypeMapping.h"
#include "PrintClangValueType.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/SipHash.h"

using namespace swift;

std::optional<std::string>
ClangExistentialTypePrinter::getCxxTypeName(Type ty,
                                            DeclAndTypePrinter &printer) {
  if (ty->isVoid())
    return std::string("void");

  if (auto *typeDecl = ty->getAnyNominal()) {
    auto info = printer.getTypeMapping().getKnownCxxTypeInfo(typeDecl);
    if (info)
      return info->name.str();
  }

  return std::nullopt;
}

bool ClangExistentialTypePrinter::canEmitExistentialMethod(
    const FuncDecl *FD, DeclAndTypePrinter &printer) {
  if (FD->isStatic())
    return false;
  if (FD->isOperator())
    return false;
  if (FD->hasAsync())
    return false;
  if (FD->hasThrows())
    return false;
  if (FD->getAttrs().hasAttribute<MutatingAttr>())
    return false;
  if (FD->isMutating())
    return false;
  if (!FD->requiresNewWitnessTableEntry())
    return false;

  auto methodTy = FD->getMethodInterfaceType()->castTo<FunctionType>();

  if (!getCxxTypeName(methodTy->getResult(), printer))
    return false;

  for (auto &param : methodTy->getParams()) {
    if (param.isInOut())
      return false;
    if (!getCxxTypeName(param.getPlainType(), printer))
      return false;
  }

  return true;
}

void ClangExistentialTypePrinter::printProtocolRequirementMethods(
    const ProtocolDecl *PD, DeclAndTypePrinter &declAndTypePrinter) {
  llvm::SmallPtrSet<const FuncDecl *, 8> emittedMethods;
  SmallVector<BaseWTStep, 4> emptyChain;
  emitMethodsForProtocol(PD, emptyChain, emittedMethods, declAndTypePrinter);
}

void ClangExistentialTypePrinter::emitMethodsForProtocol(
    const ProtocolDecl *PD, ArrayRef<BaseWTStep> baseChain,
    llvm::SmallPtrSetImpl<const FuncDecl *> &emittedMethods,
    DeclAndTypePrinter &declAndTypePrinter) {
  // Walk PD's witness table layout in the same order as SILWitnessVisitor.
  // Offset 0 is the conformance descriptor.
  size_t currentOffset = 1;

  // 1. Base protocol conformances from the requirement signature.
  //    For Self-type conformance requirements, recurse into the base
  //    protocol to emit its methods (and its bases' methods).
  auto requirements = PD->getRequirementSignature().getRequirements();
  for (const auto &reqt : requirements) {
    if (reqt.getKind() != RequirementKind::Conformance)
      continue;

    auto protocol = reqt.getProtocolDecl();

    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
      continue;

    auto type = reqt.getFirstType()->getCanonicalType();
    if (isa<GenericTypeParamType>(type)) {
      // Base protocol conformance (Self: Protocol).
      // The base WT is at currentOffset in PD's WT.
      SmallVector<BaseWTStep, 4> extendedChain(baseChain);
      extendedChain.push_back({currentOffset});
      emitMethodsForProtocol(protocol, extendedChain, emittedMethods,
                             declAndTypePrinter);
    }

    currentOffset++;
  }

  // 2. Associated types.
  for (auto *assocType : PD->getAssociatedTypeMembers()) {
    if (assocType->getOverriddenDecls().empty())
      currentOffset++;
  }

  // 3. Method entries from ABI members.
  for (Decl *member : PD->getABIMembers()) {
    if (!member->isAvailableDuringLowering())
      continue;

    if (auto *FD = dyn_cast<FuncDecl>(member)) {
      if (isa<AccessorDecl>(FD))
        continue;
      if (!FD->requiresNewWitnessTableEntry()) {
        continue;
      }

      size_t methodOffset = currentOffset;
      currentOffset++;

      if (!canEmitExistentialMethod(FD, declAndTypePrinter))
        continue;

      if (!emittedMethods.insert(FD).second)
        continue;

      uint16_t ptrAuthDisc =
          llvm::getPointerAuthStableSipHash(SILDeclRef(FD).mangle());

      emitExistentialMethod(FD, methodOffset, ptrAuthDisc, baseChain,
                            declAndTypePrinter);
    } else if (auto *ASD = dyn_cast<AbstractStorageDecl>(member)) {
      ASD->visitOpaqueAccessors([&](AccessorDecl *accessor) {
        if (accessor->requiresNewWitnessTableEntry())
          currentOffset++;
      });
    } else if (auto *CD = dyn_cast<ConstructorDecl>(member)) {
      if (CD->requiresNewWitnessTableEntry())
        currentOffset++;
    }
  }
}

void ClangExistentialTypePrinter::emitExistentialMethod(
    const FuncDecl *FD, size_t methodOffset, uint16_t ptrAuthDisc,
    ArrayRef<BaseWTStep> baseChain,
    DeclAndTypePrinter &declAndTypePrinter) {
  ClangSyntaxPrinter printer(FD->getASTContext(), os);

  auto methodTy = FD->getMethodInterfaceType()->castTo<FunctionType>();
  auto resultCxxName =
      getCxxTypeName(methodTy->getResult(), declAndTypePrinter);

  // Method signature.
  os << "  ";
  printer.printInlineForThunk();
  os << *resultCxxName << " ";
  os << FD->getBaseIdentifier().str() << "(";

  bool firstParam = true;
  for (auto *param : *FD->getParameters()) {
    if (!firstParam)
      os << ", ";
    firstParam = false;
    auto paramCxxName =
        getCxxTypeName(param->getInterfaceType(), declAndTypePrinter);
    os << *paramCxxName << " " << param->getNameStr();
  }

  os << ") const {\n";

  // Type-only witness signature.
  os << "    // Type-only witness signature (never instantiated).\n";
  os << "    struct _w { _w() = delete; static SWIFT_CALL " << *resultCxxName
     << " call(";

  bool firstTyParam = true;
  for (auto *param : *FD->getParameters()) {
    if (!firstTyParam)
      os << ", ";
    firstTyParam = false;
    auto paramCxxName =
        getCxxTypeName(param->getInterfaceType(), declAndTypePrinter);
    os << *paramCxxName;
  }
  if (!firstTyParam)
    os << ", ";
  os << "void *_Nonnull, const void *_Nonnull, "
        "SWIFT_CONTEXT void *_Nonnull); };\n";

  // Emit the chain of base WT loads for inherited methods.
  // Each step loads the next base protocol's WT from the previous one.
  std::string wtExpr = "_witnessTables[0]";
  for (size_t i = 0; i < baseChain.size(); i++) {
    std::string varName = "_bwt" + std::to_string(i);
    os << "    auto *" << varName
       << " = reinterpret_cast<const void *const *>(" << wtExpr << ")["
       << baseChain[i].offset << "];\n";
    wtExpr = varName;
  }

  // Dispatch through the (possibly base) witness table.
  os << "    return _loadWitness<" << methodOffset << ", " << ptrAuthDisc
     << ", decltype(&_w::call)>(" << wtExpr << ")(";

  bool firstArg = true;
  for (auto *param : *FD->getParameters()) {
    if (!firstArg)
      os << ", ";
    firstArg = false;
    os << param->getNameStr();
  }
  if (!firstArg)
    os << ", ";
  os << (isClassBound ? "_getType(), " : "_type, ")
     << wtExpr << ", _projectValue());\n";
  os << "  }\n";
}

void ClangExistentialTypePrinter::printConversionMethods(
    const ProtocolDecl *PD, DeclAndTypePrinter &declAndTypePrinter) {
  auto printCxxImplClassName = ClangValueTypePrinter::printCxxImplClassName;
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

  auto requirements = PD->getRequirementSignature().getRequirements();
  size_t currentOffset = 1; // offset 0 is conformance descriptor

  for (const auto &reqt : requirements) {
    if (reqt.getKind() != RequirementKind::Conformance)
      continue;

    auto protocol = reqt.getProtocolDecl();
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
      continue;

    auto type = reqt.getFirstType()->getCanonicalType();
    if (isa<GenericTypeParamType>(type) && !protocol->isMarkerProtocol()) {
      // Emit asBaseProtocol() conversion method.
      os << "  ";
      printer.printInlineForThunk();
      printer.printBaseName(protocol);
      os << " as";
      printer.printBaseName(protocol);
      os << "() const {\n";
      os << "    auto *baseWT = reinterpret_cast<const void *const *>"
            "(_witnessTables[0])["
         << currentOffset << "];\n";
      os << "    return " << cxx_synthesis::getCxxImplNamespaceName() << "::";
      printCxxImplClassName(os, protocol);
      if (isClassBound) {
        os << "::_fromExistential(_value, baseWT);\n";
      } else {
        os << "::_fromExistential(_type, _projectValue(), baseWT);\n";
      }
      os << "  }\n";
    }

    currentOffset++;
  }
}

void ClangExistentialTypePrinter::printImplFromExistentialFactory(
    const ProtocolDecl *PD, DeclAndTypePrinter &declAndTypePrinter) {
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

  os << "  static ";
  printer.printInlineForThunk();
  printer.printBaseName(PD);
  if (isClassBound) {
    os << " _fromExistential("
          "void *_Nullable classPtr, "
          "const void *_Nonnull wt) {\n";
  } else {
    os << " _fromExistential("
          "void *_Nonnull typeMetadata, "
          "void *_Nonnull projectedValue, "
          "const void *_Nonnull wt) {\n";
  }
  os << "    ";
  printer.printBaseName(PD);
  os << " result;\n";
  if (isClassBound) {
    os << "    result._value = classPtr;\n";
    os << "    if (result._value)\n";
    os << "      swift::_impl::swift_retain("
          "reinterpret_cast<void *_Nonnull>(result._value));\n";
  } else {
    os << "    result._type = typeMetadata;\n";
    os << "    result._initializeWithValue(projectedValue);\n";
  }
  os << "    result._witnessTables[0] = wt;\n";
  os << "    return result;\n";
  os << "  }\n";
}

void ClangExistentialTypePrinter::printMarkerProtocolDecl(
    const ProtocolDecl *PD, DeclAndTypePrinter &declAndTypePrinter) {
  auto printCxxImplClassName = ClangValueTypePrinter::printCxxImplClassName;
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

  printer.printParentNamespaceForNestedTypes(PD, [&](raw_ostream &os) {
    // Forward declaration of the _impl helper class.
    printer.printNamespace(cxx_synthesis::getCxxImplNamespaceName(),
                           [&](raw_ostream &os) {
                             os << "class";
                             declAndTypePrinter.printAvailability(os, PD);
                             os << ' ';
                             printCxxImplClassName(os, PD);
                             os << ";\n";
                             os << "struct ";
                             printer.printBaseName(PD);
                             os << "Tag {\n";
                             os << "  static constexpr bool IsMarker "
                                   "= true;\n";
                             os << "};\n";
                           });

    // Marker protocol: empty subclass of swift::Any (zero witness tables).
    os << "class";
    declAndTypePrinter.printAvailability(os, PD);
    ClangSyntaxPrinter(PD->getASTContext(), os).printSymbolUSRAttribute(PD);
    os << ' ';
    printer.printBaseName(PD);
    os << " final : public swift::_impl::SwiftExistentialType<"
       << cxx_synthesis::getCxxImplNamespaceName() << "::";
    printer.printBaseName(PD);
    os << "Tag>";
    os << " {\npublic:\n";

    os << "private:\n";
    os << "  ";
    printer.printInlineForThunk();
    printer.printBaseName(PD);
    os << "() noexcept : SwiftExistentialType("
          "typename SwiftExistentialType::uninit_t{}) {}\n";
    os << "  friend class " << cxx_synthesis::getCxxImplNamespaceName() << "::";
    printCxxImplClassName(os, PD);
    os << ";\n";

    printer.printSwiftMangledNameForDebugger(PD);

    os << "};\n\n";

    // The _impl helper class.
    printer.printNamespace(
        cxx_synthesis::getCxxImplNamespaceName(), [&](raw_ostream &os) {
          os << "class";
          declAndTypePrinter.printAvailability(os, PD);
          os << ' ';
          printCxxImplClassName(os, PD);
          os << " {\npublic:\n";
          os << "};\n";
        });
  });
}

void ClangExistentialTypePrinter::printExistentialTypeDecl(
    const ProtocolDecl *PD, DeclAndTypePrinter &declAndTypePrinter) {
  os << "#if defined(SWIFT_CXX_EXISTENTIAL_INTEROP) && "
        "defined(__cpp_concepts) && __cpp_concepts >= 202002L\n";

  if (PD->isMarkerProtocol()) {
    printMarkerProtocolDecl(PD, declAndTypePrinter);
    os << "#endif // SWIFT_CXX_EXISTENTIAL_INTEROP && __cpp_concepts\n";
    return;
  }

  isClassBound = PD->requiresClass();

  auto printCxxImplClassName = ClangValueTypePrinter::printCxxImplClassName;
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

  // Collect same-module conformances for boxing constructors.
  SmallVector<ConformingType, 4> boxingConformances;
  {
    auto *module = PD->getModuleContext();
    SmallVector<Decl *, 64> decls;
    module->getTopLevelDeclsWithAuxiliaryDecls(decls);
    for (auto *D : decls) {
      auto *NTD = dyn_cast<NominalTypeDecl>(D);
      if (!NTD || isa<ProtocolDecl>(NTD))
        continue;
      if (NTD->isGenericContext())
        continue;
      if (isClassBound && !isa<ClassDecl>(NTD))
        continue;
      if (!isClassBound && isa<ClassDecl>(NTD))
        continue;
      if (!declAndTypePrinter.shouldInclude(NTD))
        continue;
      SmallVector<ProtocolConformance *, 1> conformances;
      if (!NTD->lookupConformance(const_cast<ProtocolDecl *>(PD),
                                  conformances))
        continue;
      if (conformances.empty())
        continue;
      auto *conformance = conformances.front()->getRootConformance();
      auto wtEntity =
          irgen::LinkEntity::forProtocolWitnessTable(conformance);
      boxingConformances.push_back(
          {NTD, wtEntity.mangleAsString(PD->getASTContext())});
    }
  }

  printer.printParentNamespaceForNestedTypes(PD, [&](raw_ostream &os) {
    // Forward declaration of the _impl helper class, plus WT externs
    // for boxing constructors.
    printer.printNamespace(cxx_synthesis::getCxxImplNamespaceName(),
                           [&](raw_ostream &os) {
                             os << "class";
                             declAndTypePrinter.printAvailability(os, PD);
                             os << ' ';
                             printCxxImplClassName(os, PD);
                             os << ";\n";
                             for (auto &c : boxingConformances) {
                               os << "SWIFT_EXTERN const char "
                                  << c.wtSymbol << "[];\n";
                             }
                             os << "struct ";
                             printer.printBaseName(PD);
                             os << "Tag {\n";
                             os << "  using WitnessTable = "
                                   "const void *_Nonnull;\n";
                             os << "};\n";
                           });

    // Forward-declare conforming types so boxing constructor
    // declarations can reference them (definitions are emitted
    // out-of-line after all types).
    for (auto &c : boxingConformances) {
      os << "class ";
      ClangSyntaxPrinter(PD->getASTContext(), os).printBaseName(c.type);
      os << ";\n";
    }

    // Existential wrapper class: subclass of SwiftExistentialType
    // or SwiftClassExistentialType for class-bound protocols.
    os << "class";
    declAndTypePrinter.printAvailability(os, PD);
    ClangSyntaxPrinter(PD->getASTContext(), os).printSymbolUSRAttribute(PD);
    os << ' ';
    printer.printBaseName(PD);
    os << " final : public swift::_impl::";
    os << (isClassBound ? "SwiftClassExistentialType"
                        : "SwiftExistentialType");
    os << "<" << cxx_synthesis::getCxxImplNamespaceName() << "::";
    printer.printBaseName(PD);
    os << "Tag>";
    os << " {\npublic:\n";

    // Emit protocol requirement methods.
    printProtocolRequirementMethods(PD, declAndTypePrinter);

    // Emit conversion methods (asBaseProtocol()) for direct base protocols.
    printConversionMethods(PD, declAndTypePrinter);

    // Emit per-conformance boxing constructors for concrete types in
    // this module that conform to the protocol.
    printBoxingConstructors(PD, boxingConformances, declAndTypePrinter);

    os << "private:\n";
    os << "  ";
    printer.printInlineForThunk();
    printer.printBaseName(PD);
    auto baseClassName =
        isClassBound ? "SwiftClassExistentialType" : "SwiftExistentialType";
    os << "() noexcept : " << baseClassName
       << "(typename " << baseClassName << "::uninit_t{}) {}\n";
    os << "  friend class " << cxx_synthesis::getCxxImplNamespaceName() << "::";
    printCxxImplClassName(os, PD);
    os << ";\n";

    printer.printSwiftMangledNameForDebugger(PD);

    os << "};\n\n";

    // The _impl helper class.
    printer.printNamespace(
        cxx_synthesis::getCxxImplNamespaceName(), [&](raw_ostream &os) {
          os << "class";
          declAndTypePrinter.printAvailability(os, PD);
          os << ' ';
          printCxxImplClassName(os, PD);
          os << " {\npublic:\n";
          printImplFromExistentialFactory(PD, declAndTypePrinter);
          printImplGetOpaquePointer(PD);
          printImplReturnNewValue(PD);
          os << "};\n";
        });
  });

  os << "#endif // SWIFT_CXX_EXISTENTIAL_INTEROP && __cpp_concepts\n";
}

void ClangExistentialTypePrinter::printImplGetOpaquePointer(
    const ProtocolDecl *PD) {
  ClangSyntaxPrinter printer(PD->getASTContext(), os);
  // const overload
  os << "  static ";
  printer.printInlineForThunk();
  os << "const char * _Nonnull getOpaquePointer(const ";
  printer.printBaseName(PD);
  os << " &object) { return reinterpret_cast<const char *>(&object); }\n";

  // non-const overload
  os << "  static ";
  printer.printInlineForThunk();
  os << "char * _Nonnull getOpaquePointer(";
  printer.printBaseName(PD);
  os << " &object) { return reinterpret_cast<char *>(&object); }\n";
}

void ClangExistentialTypePrinter::printImplReturnNewValue(
    const ProtocolDecl *PD) {
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

  os << "  template <class T>\n";

  os << "  static ";
  printer.printInlineForHelperFunction();
  printer.printBaseName(PD);
  os << " returnNewValue(T callable) {\n";
  os << "    ";
  printer.printBaseName(PD);
  os << " result;\n";
  os << "    callable(reinterpret_cast<char *>(&result));\n";
  os << "    return result;\n";
  os << "  }\n";
}

void ClangExistentialTypePrinter::printBoxingConstructors(
    const ProtocolDecl *PD,
    ArrayRef<ConformingType> boxingConformances,
    DeclAndTypePrinter &declAndTypePrinter) {
  ClangSyntaxPrinter printer(PD->getASTContext(), os);
  ClangSyntaxPrinter ooPrinter(PD->getASTContext(), outOfLineOS);

  for (auto &c : boxingConformances) {
    // Inline declaration (forward-declared type is enough).
    os << "  ";
    printer.printInlineForThunk();
    printer.printBaseName(PD);
    os << "(const ";
    printer.printBaseName(c.type);
    os << " &value) noexcept;\n";

    // Out-of-line definition (emitted after all types are declared).
    outOfLineOS << "#if defined(SWIFT_CXX_EXISTENTIAL_INTEROP) && "
                   "defined(__cpp_concepts) && __cpp_concepts >= 202002L\n";
    outOfLineOS << "  ";
    ooPrinter.printInlineForThunk();
    ooPrinter.printBaseName(PD);
    outOfLineOS << "::";
    ooPrinter.printBaseName(PD);
    outOfLineOS << "(const ";
    ooPrinter.printBaseName(c.type);
    outOfLineOS << " &value) noexcept\n";
    auto baseClass =
        isClassBound ? "SwiftClassExistentialType" : "SwiftExistentialType";
    outOfLineOS << "    : " << baseClass
       << "(typename " << baseClass << "::uninit_t{}) {\n";
    if (isClassBound) {
      outOfLineOS << "    _value = "
         << "swift::_impl::_impl_RefCountedClass::getOpaquePointer(value);\n";
      outOfLineOS << "    swift::_impl::swift_retain("
         << "reinterpret_cast<void *_Nonnull>(_value));\n";
    } else {
      outOfLineOS << "    _type = swift::TypeMetadataTrait<";
      ooPrinter.printBaseName(c.type);
      outOfLineOS << ">::getTypeMetadata();\n";
      outOfLineOS << "    _initializeWithValue(";
      outOfLineOS << cxx_synthesis::getCxxImplNamespaceName() << "::";
      ClangValueTypePrinter::printCxxImplClassName(outOfLineOS, c.type);
      outOfLineOS << "::getOpaquePointer(value));\n";
    }
    outOfLineOS << "    _witnessTables[0] = reinterpret_cast<const void *>("
       << cxx_synthesis::getCxxImplNamespaceName() << "::" << c.wtSymbol
       << ");\n";
    outOfLineOS << "  }\n";
    outOfLineOS << "#endif // SWIFT_CXX_EXISTENTIAL_INTEROP && __cpp_concepts\n";
  }
}
