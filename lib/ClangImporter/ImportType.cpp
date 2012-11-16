//===--- ImportType.cpp - Import Clang Types ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for importing Clang types as Swift types.
//
//===----------------------------------------------------------------------===//

#include "ImporterImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/TypeVisitor.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

namespace {
  class SwiftTypeConverter : public clang::TypeVisitor<SwiftTypeConverter, Type>
  {
    ClangImporter::Implementation &Impl;

  public:
    explicit SwiftTypeConverter(ClangImporter::Implementation &impl)
      : Impl(impl) { }


#define DEPENDENT_TYPE(Class, Base)                            \
    Type Visit##Class##Type(const clang::Class##Type *) {      \
      llvm_unreachable("Dependent types cannot be converted"); \
    }
#define TYPE(Class, Base)
#include "clang/AST/TypeNodes.def"

    Type VisitBuiltinType(const clang::BuiltinType *type) {
      auto &clangContext = Impl.getClangASTContext();

      switch (type->getKind()) {
      case clang::BuiltinType::Void:
        return TupleType::getEmpty(Impl.SwiftContext);

      case clang::BuiltinType::Bool:
        return Impl.getNamedSwiftType("Bool");

      case clang::BuiltinType::Char_U:
      case clang::BuiltinType::UChar:
      case clang::BuiltinType::UShort:
      case clang::BuiltinType::UInt:
      case clang::BuiltinType::ULong:
      case clang::BuiltinType::ULongLong:
      case clang::BuiltinType::UInt128: 
      case clang::BuiltinType::Char16: // FIXME: Do we want a UTF-16 char type?
        return Impl.getNamedSwiftType(
                 "UInt" + llvm::utostr(clangContext.getTypeSize(type)));

      case clang::BuiltinType::Char_S:
      case clang::BuiltinType::SChar:
      case clang::BuiltinType::Short:
      case clang::BuiltinType::Int:
      case clang::BuiltinType::Long:
      case clang::BuiltinType::LongLong:
      case clang::BuiltinType::Int128:
        return Impl.getNamedSwiftType(
                 "Int" + llvm::utostr(clangContext.getTypeSize(type)));

      case clang::BuiltinType::Float:
        return Impl.getNamedSwiftType("Float");

      case clang::BuiltinType::Double:
        return Impl.getNamedSwiftType("Double");

      case clang::BuiltinType::Char32:
        // FIXME: This mapping works, but is it the right approach?
        return Impl.getNamedSwiftType("Char");

      // Types that cannot be mapped into Swift, and probably won't ever be.
      case clang::BuiltinType::Dependent:
      case clang::BuiltinType::ARCUnbridgedCast:
      case clang::BuiltinType::BoundMember:
      case clang::BuiltinType::BuiltinFn:
      case clang::BuiltinType::Overload:
      case clang::BuiltinType::PseudoObject:
      case clang::BuiltinType::UnknownAny:
        return Type();

      // FIXME: Types that can be mapped, but aren't yet.
      case clang::BuiltinType::Half:
      case clang::BuiltinType::LongDouble:
      case clang::BuiltinType::NullPtr:
      case clang::BuiltinType::WChar_S:
      case clang::BuiltinType::WChar_U:
        return Type();

      // FIXME: Objective-C types that need a mapping, but I haven't figured
      // out yet.
      case clang::BuiltinType::ObjCClass:
      case clang::BuiltinType::ObjCId:
      case clang::BuiltinType::ObjCSel: // FIXME: Questionable.
        return Type();
      }
    }

    Type VisitComplexType(const clang::ComplexType *type) {
      // FIXME: Implement once Complex is in the library.
      return Type();
    }

    Type VisitPointerType(const clang::PointerType *type) {
      // Function pointer types are mapped to function types.
      if (type->getPointeeType()->isFunctionType())
        return Impl.importType(type->getPointeeType());

      // All other C pointers come across as raw pointers.
      return Impl.SwiftContext.TheRawPointerType;
    }

    Type VisitBlockPointerType(const clang::BlockPointerType *type) {
      // Block pointer types are mapped to function types.
      return Impl.importType(type->getPointeeType());
    }

    Type VisitReferenceType(const clang::ReferenceType *type) {
      // FIXME: Reference types are currently handled only in function types.
      // That's probably the right answer, but revisit this later.
      return Type();
    }

    Type VisitMemberPointer(const clang::MemberPointerType *type) {
      // Member function pointers are mapped to curried functions.
      if (type->getPointeeType()->isFunctionProtoType()) {
        // Import the function type.
        auto funcTy = Impl.importType(type->getPointeeType());
        if (!funcTy)
          return Type();

        // Import the class type.
        auto classTy = Impl.importType(clang::QualType(type->getClass(), 0));
        if (!classTy)
          return Type();

        // The class type is passed [byref].
        // FIXME: Should be [byref(implicit)].
        auto thisTy = LValueType::get(classTy, LValueType::Qual::DefaultForType,
                                      Impl.SwiftContext);
        return FunctionType::get(ParenType::get(Impl.SwiftContext, thisTy),
                                 funcTy, Impl.SwiftContext);
      }

      // Note: without generalized lvalues, there is no way to map a data
      // member pointer into Swift.
      return Type();
    }

    Type VisitArrayType(const clang::ArrayType *type) {
      // FIXME: Mapping down to a raw pointer type is probably excessive.
      return Impl.SwiftContext.TheRawPointerType;
    }

    Type VisitVectorType(const clang::VectorType *type) {
      // FIXME: We could map these.
      return Type();
    }

    Type VisitExtVectorType(const clang::ExtVectorType *type) {
      // FIXME: We could map these.
      return Type();
    }

    Type VisitFunctionProtoType(const clang::FunctionProtoType *type) {
      // C-style variadic functions cannot be called from Swift.
      if (type->isVariadic())
        return Type();

      // Import the result type.
      auto resultTy = Impl.importType(type->getResultType());
      if (!resultTy)
        return Type();

      SmallVector<TupleTypeElt, 4> params;
      for (auto param = type->arg_type_begin(),
             paramEnd = type->arg_type_end();
           param != paramEnd; ++param) {
        clang::QualType paramTy = *param;
        bool byRef = false;

        if (auto refType = paramTy->getAs<clang::ReferenceType>()) {
          byRef = true;
          paramTy = refType->getPointeeType();
        }

        auto swiftParamTy = Impl.importType(paramTy);
        if (!swiftParamTy)
          return Type();

        if (byRef)
          swiftParamTy = LValueType::get(swiftParamTy,
                                         LValueType::Qual::DefaultForType,
                                         Impl.SwiftContext);

        // FIXME: If we were walking TypeLocs, we could actually get parameter
        // names. The probably doesn't matter outside of a FuncDecl, which
        // we'll have to special-case, but it's an interesting bit of data loss.
        params.push_back(TupleTypeElt(swiftParamTy, Identifier()));
      }

      // Form the parameter tuple.
      auto paramsTy = TupleType::get(params, Impl.SwiftContext);

      // Form the function type.
      return FunctionType::get(paramsTy, resultTy, Impl.SwiftContext);
    }

    Type VisitFunctionNoProtoType(const clang::FunctionNoProtoType *type) {
      // There is no sensible way to describe functions without prototypes
      // in the Swift type system.
      return Type();
    }

    Type VisitParenType(const clang::ParenType *type) {
      auto inner = Impl.importType(type->getInnerType());
      if (!inner)
        return Type();

      return ParenType::get(Impl.SwiftContext, inner);
    }

    Type VisitTypedefType(const clang::TypedefType *type) {
      auto decl = dyn_cast_or_null<TypeDecl>(Impl.importDecl(type->getDecl()));
      if (!decl)
        return nullptr;

      return decl->getDeclaredType();
    }

    Type VisitTypeOfExpr(const clang::TypeOfExprType *type) {
      return Impl.importType(
               Impl.getClangASTContext().getCanonicalType(clang::QualType(type,
                                                                          0)));
    }

    Type VisitTypeOfType(const clang::TypeOfType *type) {
      return Impl.importType(type->getUnderlyingType());
    }

    Type VisitDecltypeType(const clang::DecltypeType *type) {
      return Impl.importType(type->getUnderlyingType());
    }

    Type VisitUnaryTransformType(const clang::UnaryTransformType *type) {
      return Impl.importType(type->getUnderlyingType());
    }

    Type VisitRecordType(const clang::RecordType *type) {
      // FIXME: Import record types as structs.
      return Type();
    }

    Type VisitEnumType(const clang::EnumType *type) {
      // FIXME: Import enum types as oneofs.
      return Type();
    }

    Type VisitElaboratedType(const clang::ElaboratedType *type) {
      return Impl.importType(type->getNamedType());
    }

    Type VisitAttributedType(const clang::AttributedType *type) {
      return Impl.importType(type->getEquivalentType());
    }

    Type VisitSubstTemplateTypeParmType(
           const clang::SubstTemplateTypeParmType *type) {
      return Impl.importType(type->getReplacementType());
    }

    Type VisitTemplateSpecializationType(
           const clang::TemplateSpecializationType *type) {
      return Impl.importType(type->desugar());
    }

    Type VisitAutoType(const clang::AutoType *type) {
      return Impl.importType(type->getDeducedType());
    }

    Type VisitObjCObjectType(const clang::ObjCObjectType *type) {
      // FIXME: Objective-C types are only exposed when referenced via an
      // Objective-C object pointer type. Revisit this.
      return Type();
    }

    Type VisitObjCInterfaceType(const clang::ObjCInterfaceType *type) {
      // FIXME: Objective-C classes are only exposed when referenced via an
      // Objective-C object pointer type. Revisit this.
      return Type();
    }

    Type VisitObjCObjectPointerType(const clang::ObjCObjectPointerType *type) {
      // FIXME: Map these, once we learn how to import an Objective-C class
      // declaration and all of its various categories/extensions.
      return Type();
    }
  };
}

Type ClangImporter::Implementation::importType(clang::QualType type) {
  SwiftTypeConverter converter(*this);
  return converter.Visit(type.getTypePtr());
}

Type ClangImporter::Implementation::importFunctionType(
       clang::QualType resultType,
       ArrayRef<clang::ParmVarDecl *> params,
       bool isVariadic) {
  // Cannot import variadic types.
  if (isVariadic)
    return Type();

  // Import the result type.
  auto swiftResultTy = importType(resultType);
  if (!swiftResultTy)
    return Type();

  // Import the parameters.
  SmallVector<TupleTypeElt, 4> swiftParams;
  for (auto param : params) {
    auto paramTy = param->getType();
    if (paramTy->isVoidType())
      continue;

    bool byRef = false;

    // C++ reference types are mapped to [byref].
    if (auto refType = paramTy->getAs<clang::ReferenceType>()) {
      byRef = true;
      paramTy = refType->getPointeeType();
    }

    auto swiftParamTy = importType(paramTy);
    if (!swiftParamTy)
      return Type();

    if (byRef)
      swiftParamTy = LValueType::get(swiftParamTy,
                                     LValueType::Qual::DefaultForType,
                                     SwiftContext);

    swiftParams.push_back(TupleTypeElt(swiftParamTy,
                                       importName(param->getDeclName())));
  }

  // Form the parameter tuple.
  auto paramsTy = TupleType::get(swiftParams, SwiftContext);

  // Form the function type.
  return FunctionType::get(paramsTy, swiftResultTy, SwiftContext);
}


Type ClangImporter::Implementation::getNamedSwiftType(StringRef name) {
  if (!swiftModule) {
    for (auto module : SwiftContext.LoadedModules) {
      if (module.second->Name.str() == "swift") {
        swiftModule = module.second;
        break;
      }
    }

    // The 'swift' module hasn't been loaded. There's nothing we can do.
    if (!swiftModule)
      return Type();
  }

  // Look for the type.
  UnqualifiedLookup lookup(SwiftContext.getIdentifier(name), swiftModule);
  if (auto type = lookup.getSingleTypeResult()) {
    return type->getDeclaredType();
  }

  return Type();
}

