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
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolAssociations.h"
#include "swift/AST/Types.h"
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
      // Self: Protocol -- this is a base protocol conformance.
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
  std::string wtExpr = "_witnessTable";
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
  os << "_type, " << wtExpr << ", _projectValue());\n";
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
            "(_witnessTable)["
         << currentOffset << "];\n";
      os << "    return " << cxx_synthesis::getCxxImplNamespaceName() << "::";
      printCxxImplClassName(os, protocol);
      os << "::_fromExistential(*this, baseWT);\n";
      os << "  }\n";
    }

    currentOffset++;
  }
}

void ClangExistentialTypePrinter::printImplFromExistentialFactory(
    const ProtocolDecl *PD, DeclAndTypePrinter &declAndTypePrinter) {
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

  auto pats = PD->getPrimaryAssociatedTypes();
  if (!pats.empty()) {
    os << "  template <";
    llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
      os << "typename " << pat->getNameStr();
    });
    os << ">\n";
  }

  os << "  static ";
  printer.printInlineForThunk();
  printer.printBaseName(PD);
  if (!pats.empty()) {
    os << "<";
    llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
      os << pat->getNameStr();
    });
    os << ">";
  }
  os << " _fromExistential("
        "const swift::_impl::SwiftExistentialType &src, "
        "const void *_Nonnull wt) {\n";
  os << "    ";
  printer.printBaseName(PD);
  if (!pats.empty()) {
    os << "<";
    llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
      os << pat->getNameStr();
    });
    os << ">";
  }
  os << " result;\n";
  os << "    result._initializeWithCopy(src);\n";
  os << "    result._witnessTable = wt;\n";
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
                           });

    // Marker protocol: empty subclass of swift::Any (zero witness tables).
    os << "class";
    declAndTypePrinter.printAvailability(os, PD);
    ClangSyntaxPrinter(PD->getASTContext(), os).printSymbolUSRAttribute(PD);
    os << ' ';
    printer.printBaseName(PD);
    os << " final : public swift::Any";
    os << " {\npublic:\n";

    os << "private:\n";
    os << "  ";
    printer.printInlineForThunk();
    printer.printBaseName(PD);
    os << "() noexcept : Any() {}\n";
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
  if (PD->isMarkerProtocol()) {
    printMarkerProtocolDecl(PD, declAndTypePrinter);
    return;
  }

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
                           });

    // Existential wrapper class: subclass of SwiftExistentialType.
    // Protocols with primary associated types become class templates.
    auto pats = PD->getPrimaryAssociatedTypes();
    if (!pats.empty()) {
      os << "template <";
      llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
        os << "typename " << pat->getNameStr() << " = swift::Any";
      });
      os << ">\n";
    }
    os << "class";
    declAndTypePrinter.printAvailability(os, PD);
    ClangSyntaxPrinter(PD->getASTContext(), os).printSymbolUSRAttribute(PD);
    os << ' ';
    printer.printBaseName(PD);
    os << " final : public swift::_impl::SwiftExistentialType";
    os << " {\npublic:\n";

    // Emit protocol requirement methods.
    printProtocolRequirementMethods(PD, declAndTypePrinter);

    // Emit conversion methods (asBaseProtocol()) for direct base protocols.
    printConversionMethods(PD, declAndTypePrinter);

    os << "private:\n";
    os << "  ";
    printer.printInlineForThunk();
    printer.printBaseName(PD);
    os << "() noexcept : SwiftExistentialType(uninit_t{}) {}\n";
    os << "#pragma clang diagnostic push\n";
    os << "#pragma clang diagnostic ignored \"-Wunused-private-field\"\n";
    os << "  const void *_Nonnull _witnessTable;\n";
    os << "#pragma clang diagnostic pop\n";
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
}

void ClangExistentialTypePrinter::printImplGetOpaquePointer(
    const ProtocolDecl *PD) {
  ClangSyntaxPrinter printer(PD->getASTContext(), os);
  auto pats = PD->getPrimaryAssociatedTypes();
  auto printProtoName = [&]() {
    printer.printBaseName(PD);
    if (!pats.empty()) {
      os << "<";
      llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
        os << pat->getNameStr();
      });
      os << ">";
    }
  };

  // const overload
  if (!pats.empty()) {
    os << "  template <";
    llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
      os << "typename " << pat->getNameStr();
    });
    os << ">\n";
  }
  os << "  static ";
  printer.printInlineForThunk();
  os << "const char * _Nonnull getOpaquePointer(const ";
  printProtoName();
  os << " &object) { return reinterpret_cast<const char *>(&object); }\n";

  // non-const overload
  if (!pats.empty()) {
    os << "  template <";
    llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
      os << "typename " << pat->getNameStr();
    });
    os << ">\n";
  }
  os << "  static ";
  printer.printInlineForThunk();
  os << "char * _Nonnull getOpaquePointer(";
  printProtoName();
  os << " &object) { return reinterpret_cast<char *>(&object); }\n";
}

void ClangExistentialTypePrinter::printImplReturnNewValue(
    const ProtocolDecl *PD) {
  ClangSyntaxPrinter printer(PD->getASTContext(), os);
  auto pats = PD->getPrimaryAssociatedTypes();

  os << "  template <";
  bool needsComma = false;
  for (auto *pat : pats) {
    if (needsComma) os << ", ";
    os << "typename " << pat->getNameStr();
    needsComma = true;
  }
  if (needsComma) os << ", ";
  os << "class T>\n";

  os << "  static ";
  printer.printInlineForHelperFunction();
  printer.printBaseName(PD);
  if (!pats.empty()) {
    os << "<";
    llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
      os << pat->getNameStr();
    });
    os << ">";
  }
  os << " returnNewValue(T callable) {\n";
  os << "    ";
  printer.printBaseName(PD);
  if (!pats.empty()) {
    os << "<";
    llvm::interleaveComma(pats, os, [&](const AssociatedTypeDecl *pat) {
      os << pat->getNameStr();
    });
    os << ">";
  }
  os << " result;\n";
  os << "    callable(reinterpret_cast<char *>(&result));\n";
  os << "    return result;\n";
  os << "  }\n";
}
