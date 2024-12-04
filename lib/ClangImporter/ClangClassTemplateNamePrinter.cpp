//===--- ClangClassTemplateNamePrinter.cpp --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ClangClassTemplateNamePrinter.h"
#include "ImporterImpl.h"
#include "clang/AST/TemplateArgumentVisitor.h"
#include "clang/AST/TypeVisitor.h"

using namespace swift;
using namespace swift::importer;

struct TemplateInstantiationNamePrinter
    : clang::TypeVisitor<TemplateInstantiationNamePrinter, std::string> {
  ASTContext &swiftCtx;
  NameImporter *nameImporter;
  ImportNameVersion version;

  TemplateInstantiationNamePrinter(ASTContext &swiftCtx,
                                   NameImporter *nameImporter,
                                   ImportNameVersion version)
      : swiftCtx(swiftCtx), nameImporter(nameImporter), version(version) {}

  std::string VisitType(const clang::Type *type) {
    // Print "_" as a fallback if we couldn't emit a more meaningful type name.
    return "_";
  }

  std::string VisitBuiltinType(const clang::BuiltinType *type) {
    switch (type->getKind()) {
    case clang::BuiltinType::Void:
      return "Void";
    case clang::BuiltinType::NullPtr:
      return "nil";

#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)                  \
    case clang::BuiltinType::CLANG_BUILTIN_KIND:                               \
      return #SWIFT_TYPE_NAME;
#include "swift/ClangImporter/BuiltinMappedTypes.def"
    default:
      break;
    }

    return VisitType(type);
  }

  std::string VisitTagType(const clang::TagType *type) {
    auto tagDecl = type->getAsTagDecl();
    if (auto namedArg = dyn_cast_or_null<clang::NamedDecl>(tagDecl)) {
      if (auto typeDefDecl = tagDecl->getTypedefNameForAnonDecl())
        namedArg = typeDefDecl;
      llvm::SmallString<128> storage;
      llvm::raw_svector_ostream buffer(storage);
      nameImporter->importName(namedArg, version, clang::DeclarationName())
          .getDeclName()
          .print(buffer);
      return buffer.str().str();
    }
    return "_";
  }

  std::string VisitPointerType(const clang::PointerType *type) {
    std::string pointeeResult = Visit(type->getPointeeType().getTypePtr());

    enum class TagTypeDecorator { None, UnsafePointer, UnsafeMutablePointer };

    // If this is a pointer to foreign reference type, we should not wrap
    // it in Unsafe(Mutable)?Pointer, since it will be imported as a class
    // in Swift.
    bool isReferenceType = false;
    if (auto tagDecl = type->getPointeeType()->getAsTagDecl()) {
      if (auto *rd = dyn_cast<clang::RecordDecl>(tagDecl))
        isReferenceType =
            ClangImporter::Implementation::recordHasReferenceSemantics(
                rd, swiftCtx);
    }

    TagTypeDecorator decorator;
    if (!isReferenceType)
      decorator = type->getPointeeType().isConstQualified()
                      ? TagTypeDecorator::UnsafePointer
                      : TagTypeDecorator::UnsafeMutablePointer;
    else
      decorator = TagTypeDecorator::None;

    llvm::SmallString<128> storage;
    llvm::raw_svector_ostream buffer(storage);
    if (decorator != TagTypeDecorator::None)
      buffer << (decorator == TagTypeDecorator::UnsafePointer
                     ? "UnsafePointer"
                     : "UnsafeMutablePointer")
             << '<';
    buffer << pointeeResult;
    if (decorator != TagTypeDecorator::None)
      buffer << '>';

    return buffer.str().str();
  }

  std::string VisitFunctionProtoType(const clang::FunctionProtoType *type) {
    llvm::SmallString<128> storage;
    llvm::raw_svector_ostream buffer(storage);

    buffer << "((";
    llvm::interleaveComma(type->getParamTypes(), buffer,
                          [&](const clang::QualType &paramType) {
                            buffer << Visit(paramType.getTypePtr());
                          });
    buffer << ") -> ";
    buffer << Visit(type->getReturnType().getTypePtr());
    buffer << ")";

    return buffer.str().str();
  }

  std::string VisitVectorType(const clang::VectorType *type) {
    return (Twine("SIMD") + std::to_string(type->getNumElements()) + "<" +
            Visit(type->getElementType().getTypePtr()) + ">")
        .str();
  }

  std::string VisitArrayType(const clang::ArrayType *type) {
    return (Twine("[") + Visit(type->getElementType().getTypePtr()) + "]")
        .str();
  }

  std::string VisitConstantArrayType(const clang::ConstantArrayType *type) {
    return (Twine("Vector<") + Visit(type->getElementType().getTypePtr()) +
            ", " + std::to_string(type->getSExtSize()) + ">")
        .str();
  }
};

struct TemplateArgumentPrinter
    : clang::ConstTemplateArgumentVisitor<TemplateArgumentPrinter, void,
                                          llvm::raw_svector_ostream &> {
  TemplateInstantiationNamePrinter typePrinter;

  TemplateArgumentPrinter(ASTContext &swiftCtx, NameImporter *nameImporter,
                          ImportNameVersion version)
      : typePrinter(swiftCtx, nameImporter, version) {}

  void VisitTemplateArgument(const clang::TemplateArgument &arg,
                             llvm::raw_svector_ostream &buffer) {
    // Print "_" as a fallback if we couldn't emit a more meaningful type name.
    buffer << "_";
  }

  void VisitTypeTemplateArgument(const clang::TemplateArgument &arg,
                                 llvm::raw_svector_ostream &buffer) {
    auto ty = arg.getAsType();
    buffer << typePrinter.Visit(ty.getTypePtr());
    if (ty.isConstQualified()) {
      buffer << "_const";
    }
  }

  void VisitIntegralTemplateArgument(const clang::TemplateArgument &arg,
                                     llvm::raw_svector_ostream &buffer) {
    buffer << "_";
    if (arg.getIntegralType()->isBuiltinType()) {
      buffer << typePrinter.Visit(arg.getIntegralType().getTypePtr()) << "_";
    }
    arg.getAsIntegral().print(buffer, true);
  }

  void VisitPackTemplateArgument(const clang::TemplateArgument &arg,
                                 llvm::raw_svector_ostream &buffer) {
    VisitTemplateArgumentArray(arg.getPackAsArray(), buffer);
  }

  void VisitTemplateArgumentArray(ArrayRef<clang::TemplateArgument> args,
                                  llvm::raw_svector_ostream &buffer) {
    bool needsComma = false;
    for (auto &arg : args) {
      // Do not try to print empty packs.
      if (arg.getKind() == clang::TemplateArgument::ArgKind::Pack &&
          arg.getPackAsArray().empty())
        continue;

      if (needsComma)
        buffer << ", ";
      Visit(arg, buffer);
      needsComma = true;
    }
  }
};

std::string swift::importer::printClassTemplateSpecializationName(
    const clang::ClassTemplateSpecializationDecl *decl, ASTContext &swiftCtx,
    NameImporter *nameImporter, ImportNameVersion version) {
  TemplateArgumentPrinter templateArgPrinter(swiftCtx, nameImporter, version);

  llvm::SmallString<128> storage;
  llvm::raw_svector_ostream buffer(storage);
  decl->printName(buffer);
  buffer << "<";
  templateArgPrinter.VisitTemplateArgumentArray(
      decl->getTemplateArgs().asArray(), buffer);
  buffer << ">";
  return buffer.str().str();
}
