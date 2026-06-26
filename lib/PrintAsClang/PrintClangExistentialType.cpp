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
  // Compute witness table entry offsets by walking the protocol's
  // requirement signature and ABI members in the same order as
  // SILWitnessVisitor. This must stay in sync with IRGen's
  // WitnessTableLayout.
  //
  // Offset 0 is the protocol conformance descriptor (header).
  // Requirement entries start at offset 1
  // (WitnessTableFirstRequirementOffset).
  size_t currentOffset = 1;

  // 1. Base protocol conformances and associated conformances from
  //    the requirement signature.
  auto requirements = PD->getRequirementSignature().getRequirements();
  for (const auto &reqt : requirements) {
    if (reqt.getKind() != RequirementKind::Conformance)
      continue;

    auto requirement = reqt.getProtocolDecl();

    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(requirement))
      continue;

    // Base protocol or associated conformance -- both take a WT entry.
    currentOffset++;
  }

  // 2. Associated types.
  for (auto *assocType : PD->getAssociatedTypeMembers()) {
    if (assocType->getOverriddenDecls().empty())
      currentOffset++;
  }

  // 3. Method entries from ABI members.
  ClangSyntaxPrinter printer(PD->getASTContext(), os);

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

      // Compute the ptrauth discriminator for this witness entry.
      // This matches IRGen: siphash(SILDeclRef::mangle()) truncated to 16 bits.
      uint16_t ptrAuthDisc =
          llvm::getPointerAuthStableSipHash(SILDeclRef(FD).mangle());

      auto methodTy = FD->getMethodInterfaceType()->castTo<FunctionType>();
      auto resultCxxName = getCxxTypeName(methodTy->getResult(),
                                          declAndTypePrinter);

      os << "  ";
      printer.printInlineForThunk();
      os << *resultCxxName << " ";
      os << FD->getBaseIdentifier().str() << "(";

      // Print parameter list.
      bool firstParam = true;
      for (auto *param : *FD->getParameters()) {
        if (!firstParam)
          os << ", ";
        firstParam = false;

        auto paramCxxName = getCxxTypeName(param->getInterfaceType(),
                                           declAndTypePrinter);
        os << *paramCxxName << " " << param->getNameStr();
      }

      os << ") const {\n";

      // Use a local struct with a static method to define the swiftcall
      // function type -- Clang accepts SWIFT_CONTEXT on function
      // declarations but not on typedef/using type aliases.
      // _loadWitness handles the ptrauth struct overlay.
      //
      // Witness method ABI: [user args, Self metadata, WT, self(ctx)]
      os << "    // Type-only witness signature (never instantiated).\n";
      os << "    struct _w { _w() = delete; static SWIFT_CALL "
         << *resultCxxName << " call(";

      // User parameter types.
      bool firstTyParam = true;
      for (auto *param : *FD->getParameters()) {
        if (!firstTyParam)
          os << ", ";
        firstTyParam = false;
        auto paramCxxName = getCxxTypeName(param->getInterfaceType(),
                                           declAndTypePrinter);
        os << *paramCxxName;
      }
      if (!firstTyParam)
        os << ", ";
      os << "void *_Nonnull, const void *_Nonnull, "
            "SWIFT_CONTEXT void *_Nonnull); };\n";

      // Load and call through the authenticated witness pointer.
      os << "    return _loadWitness<" << methodOffset << ", "
         << ptrAuthDisc << ", decltype(&_w::call)>(_witnessTable)(";

      // User arguments.
      bool firstArg = true;
      for (auto *param : *FD->getParameters()) {
        if (!firstArg)
          os << ", ";
        firstArg = false;
        os << param->getNameStr();
      }
      if (!firstArg)
        os << ", ";
      os << "_type, _witnessTable, _projectValue());\n";
      os << "  }\n";
    } else if (auto *ASD = dyn_cast<AbstractStorageDecl>(member)) {
      // Count accessor entries but don't emit methods for them yet.
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
    os << "class";
    declAndTypePrinter.printAvailability(os, PD);
    ClangSyntaxPrinter(PD->getASTContext(), os).printSymbolUSRAttribute(PD);
    os << ' ';
    printer.printBaseName(PD);
    os << " final : public swift::_impl::SwiftExistentialType";
    os << " {\npublic:\n";

    // Emit protocol requirement methods.
    printProtocolRequirementMethods(PD, declAndTypePrinter);

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
          os << "};\n";
        });
  });
}
