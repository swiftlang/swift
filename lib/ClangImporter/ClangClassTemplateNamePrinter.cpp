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
    Type swiftType = nullptr;
    switch (type->getKind()) {
    case clang::BuiltinType::Void:
      swiftType =
          swiftCtx.getNamedSwiftType(swiftCtx.getStdlibModule(), "Void");
      break;
#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)                  \
      case clang::BuiltinType::CLANG_BUILTIN_KIND:                             \
        swiftType = swiftCtx.getNamedSwiftType(swiftCtx.getStdlibModule(),     \
                                               #SWIFT_TYPE_NAME);              \
        break;
#define MAP_BUILTIN_CCHAR_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)            \
      case clang::BuiltinType::CLANG_BUILTIN_KIND:                             \
        swiftType = swiftCtx.getNamedSwiftType(swiftCtx.getStdlibModule(),     \
                                               #SWIFT_TYPE_NAME);              \
        break;
#include "swift/ClangImporter/BuiltinMappedTypes.def"
    default:
      break;
    }

    if (swiftType) {
      if (swiftType->is<NominalType>() || swiftType->isVoid()) {
        return swiftType->getStringAsComponent();
      }
    }
    return "_";
  }

  std::string VisitRecordType(const clang::RecordType *type) {
    auto tagDecl = type->getAsTagDecl();
    if (auto namedArg = dyn_cast_or_null<clang::NamedDecl>(tagDecl)) {
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
};

std::string swift::importer::printClassTemplateSpecializationName(
    const clang::ClassTemplateSpecializationDecl *decl, ASTContext &swiftCtx,
    NameImporter *nameImporter, ImportNameVersion version) {
  TemplateInstantiationNamePrinter templateNamePrinter(swiftCtx, nameImporter,
                                                       version);

  // TODO: the following logic should probably be a ConstTemplateArgumentVisitor
  llvm::SmallString<128> storage;
  llvm::raw_svector_ostream buffer(storage);
  decl->printName(buffer);
  buffer << "<";
  llvm::interleaveComma(
      decl->getTemplateArgs().asArray(), buffer,
      [&buffer, &templateNamePrinter](const clang::TemplateArgument &arg) {
        // Use import name here so builtin types such as "int" map to their
        // Swift equivalent ("CInt").
        if (arg.getKind() == clang::TemplateArgument::Type) {
          auto ty = arg.getAsType();
          buffer << templateNamePrinter.Visit(ty.getTypePtr());
          if (ty.isConstQualified()) {
            buffer << "_const";
          }
          return;
        } else if (arg.getKind() == clang::TemplateArgument::Integral) {
          buffer << "_";
          if (arg.getIntegralType()->isBuiltinType()) {
            buffer << templateNamePrinter.Visit(
                          arg.getIntegralType().getTypePtr())
                   << "_";
          }
          arg.getAsIntegral().print(buffer, true);
          return;
        }
        buffer << "_";
      });
  buffer << ">";
  return buffer.str().str();
}
