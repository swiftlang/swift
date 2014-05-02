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
#include "swift/Strings.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/TypeVisitor.h"
#include "swift/ClangImporter/ClangModule.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

/// Is the given type the result of importing a CF pointer?
static bool isImportedCFPointer(Type type) {
  if (auto classType = type->getAs<ClassType>()) {
    assert(classType->getDecl()->hasClangNode());
    auto clangNode = classType->getDecl()->getClangNode();
    return isa<clang::TypedefDecl>(clangNode.castAsDecl());
  }
  return false;
}

namespace {
  /// Various types that we want to do something interesting to after
  /// importing them.
  enum class ImportHint {
    /// There is nothing special about the source type.
    None,

    /// The source type is 'void'.
    Void,

    /// The source type is 'BOOL'.
    BOOL,

    /// The source type is 'NSString'.
    NSString,

    /// The source type is 'NSUInteger'.
    NSUInteger,

    /// The source type is a C pointer type.
    CPointer,

    /// The source type is an Objective-C object pointer type.
    ObjCPointer,

    /// The source type is a CF object pointer type.
    CFPointer,

    /// The source type is a C++ reference type.
    Reference,

    /// The source type is a block pointer type.
    Block,
  };

  struct ImportResult {
    Type AbstractType;
    ImportHint Hint;

    /*implicit*/ ImportResult(Type type = Type(),
                              ImportHint hint = ImportHint::None)
      : AbstractType(type), Hint(hint) {}

    /*implicit*/ ImportResult(TypeBase *type = nullptr,
                              ImportHint hint = ImportHint::None)
      : AbstractType(type), Hint(hint) {}

    explicit operator bool() const { return (bool) AbstractType; }
  };

  class SwiftTypeConverter :
    public clang::TypeVisitor<SwiftTypeConverter, ImportResult>
  {
    ClangImporter::Implementation &Impl;
    
  public:
    SwiftTypeConverter(ClangImporter::Implementation &impl)
      : Impl(impl) {}

    using TypeVisitor::Visit;
    ImportResult Visit(clang::QualType type) {
      return Visit(type.getTypePtr());
    }

#define DEPENDENT_TYPE(Class, Base)                               \
    ImportResult Visit##Class##Type(const clang::Class##Type *) { \
      llvm_unreachable("Dependent types cannot be converted");    \
    }
#define TYPE(Class, Base)
#include "clang/AST/TypeNodes.def"
    
    ImportResult VisitBuiltinType(const clang::BuiltinType *type) {
      switch (type->getKind()) {
      case clang::BuiltinType::Void:
        return { Type(), ImportHint::Void };

#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)  \
      case clang::BuiltinType::CLANG_BUILTIN_KIND:             \
        return Impl.getNamedSwiftType(Impl.getStdlibModule(),  \
                                        #SWIFT_TYPE_NAME);
#include "swift/ClangImporter/BuiltinMappedTypes.def"

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
        return Type();

      // Objective-C types that aren't mapped directly; rather, pointers to
      // these types will be mapped.
      case clang::BuiltinType::ObjCClass:
      case clang::BuiltinType::ObjCId:
      case clang::BuiltinType::ObjCSel:
        return Type();

      // OpenCL types that don't have Swift equivalents.
      case clang::BuiltinType::OCLImage1d:
      case clang::BuiltinType::OCLImage1dArray:
      case clang::BuiltinType::OCLImage1dBuffer:
      case clang::BuiltinType::OCLImage2d:
      case clang::BuiltinType::OCLImage2dArray:
      case clang::BuiltinType::OCLImage3d:
      case clang::BuiltinType::OCLEvent:
      case clang::BuiltinType::OCLSampler:
        return Type();
      }
    }

    ImportResult VisitComplexType(const clang::ComplexType *type) {
      // FIXME: Implement once Complex is in the library.
      return Type();
    }

    ImportResult VisitAtomicType(const clang::AtomicType *type) {
      // FIXME: handle pointers and fields of atomic type
      return Type();
    }

    ImportResult VisitMemberPointerType(const clang::MemberPointerType *type) {
      return Type();
    }
    
    ImportResult VisitPointerType(const clang::PointerType *type) {      
      // FIXME: Function pointer types can be mapped to Swift function types
      // once we have the notion of a "thin" function that does not capture
      // anything.
      if (type->getPointeeType()->isFunctionType())
        return Type();
      
      // "const char *" maps to Swift's CString.
      // FIXME: Map to a bridged type in parameter context?
      clang::ASTContext &clangContext = Impl.getClangASTContext();
      if (clangContext.hasSameType(type->getPointeeType(),
                                   clangContext.CharTy.withConst())) {
        return Impl.getNamedSwiftType(Impl.getStdlibModule(), "CString");
      }
      
      // Special case for NSZone*, which has its own Swift wrapper.
      if (auto pointee = type->getPointeeType()->getAs<clang::TypedefType>()) {
        const clang::RecordType *pointeeStruct = pointee->getAsStructureType();
        if (pointeeStruct &&
            !pointeeStruct->getDecl()->isCompleteDefinition() &&
            pointee->getDecl()->getName() == "NSZone") {
          Module *Foundation = Impl.getNamedModule(FOUNDATION_MODULE_NAME);
          Type wrapperTy = Impl.getNamedSwiftType(Foundation, "NSZone");
          if (wrapperTy)
            return wrapperTy;
        }
      }
      
      // All other C pointers to concrete types map to UnsafePointer<T> or
      // COpaquePointer, except in parameter position.
      auto pointeeQualType = type->getPointeeType();

      // Import void* as COpaquePointer.
      if (pointeeQualType->isVoidType()) {
        return { getOpaquePointerType(), ImportHint::CPointer };
      }
      
      auto pointeeType = Impl.importType(pointeeQualType,
                                         ImportTypeKind::Pointee);
      
      // If the pointed-to type is unrepresentable in Swift, import as
      // COpaquePointer (and don't hint to the adjuster).
      if (!pointeeType)
        return getOpaquePointerType();
      
      // Non-parameter pointers map to UnsafePointer.
      return { Impl.getNamedSwiftTypeSpecialization(Impl.getStdlibModule(),
                                               "UnsafePointer", pointeeType),
               ImportHint::CPointer };
    }

    Type getOpaquePointerType() {
      return Impl.getNamedSwiftType(Impl.getStdlibModule(), "COpaquePointer");
    }

    ImportResult VisitBlockPointerType(const clang::BlockPointerType *type) {
      // Block pointer types are mapped to function types.
      Type pointeeType = Impl.importType(type->getPointeeType(),
                                         ImportTypeKind::Abstract);
      if (!pointeeType)
        return Type();
      FunctionType *fTy = pointeeType->castTo<FunctionType>();
      
      auto rep = FunctionType::Representation::Block;
      auto funcTy = FunctionType::get(fTy->getInput(), fTy->getResult(),
                                   fTy->getExtInfo().withRepresentation(rep));
      return { funcTy, ImportHint::Block };
    }

    ImportResult VisitReferenceType(const clang::ReferenceType *type) {
      return { nullptr, ImportHint::Reference };
    }

    ImportResult VisitMemberPointer(const clang::MemberPointerType *type) {
      // FIXME: Member function pointers can be mapped to curried functions,
      // but only when we can express the notion of a function that does
      // not capture anything from its enclosing context.
      return Type();
    }

    ImportResult VisitArrayType(const clang::ArrayType *type) {
      // FIXME: Array types will need to be mapped differently depending on
      // context.
      return Type();
    }
    
    ImportResult VisitConstantArrayType(const clang::ConstantArrayType *type) {
      // FIXME: In a function argument context, arrays should import as
      // pointers.
      
      // FIXME: Map to a real fixed-size Swift array type when we have those.
      // Importing as a tuple at least fills the right amount of space, and
      // we can cheese static-offset "indexing" using .$n operations.
      
      Type elementType = Impl.importType(type->getElementType(),
                                         ImportTypeKind::Pointee);
      if (!elementType)
        return Type();
      
      TupleTypeElt elt(elementType);
      SmallVector<TupleTypeElt, 8> elts;
      for (size_t i = 0, size = type->getSize().getZExtValue(); i < size; ++i)
        elts.push_back(elt);
      
      return TupleType::get(elts, elementType->getASTContext());
    }

    ImportResult VisitVectorType(const clang::VectorType *type) {
      // FIXME: We could map these.
      return Type();
    }

    ImportResult VisitExtVectorType(const clang::ExtVectorType *type) {
      // FIXME: We could map these.
      return Type();
    }

    ImportResult VisitFunctionProtoType(const clang::FunctionProtoType *type) {
      // C-style variadic functions cannot be called from Swift.
      if (type->isVariadic())
        return Type();

      // Import the result type.
      auto resultTy = Impl.importType(type->getReturnType(),
                                      ImportTypeKind::Result);
      if (!resultTy)
        return Type();

      SmallVector<TupleTypeElt, 4> params;
      for (auto param = type->param_type_begin(),
             paramEnd = type->param_type_end();
           param != paramEnd; ++param) {
        auto swiftParamTy = Impl.importType(*param, ImportTypeKind::Parameter);
        if (!swiftParamTy)
          return Type();

        // FIXME: If we were walking TypeLocs, we could actually get parameter
        // names. The probably doesn't matter outside of a FuncDecl, which
        // we'll have to special-case, but it's an interesting bit of data loss.
        params.push_back(swiftParamTy);
      }

      // Form the parameter tuple.
      auto paramsTy = TupleType::get(params, Impl.SwiftContext);

      // Form the function type.
      return FunctionType::get(paramsTy, resultTy);
    }

    ImportResult
    VisitFunctionNoProtoType(const clang::FunctionNoProtoType *type) {
      // Import functions without prototypes as functions with no parameters.
      auto resultTy = Impl.importType(type->getReturnType(),
                                      ImportTypeKind::Result);
      if (!resultTy)
        return Type();

      return FunctionType::get(TupleType::getEmpty(Impl.SwiftContext),resultTy);
    }

    ImportResult VisitParenType(const clang::ParenType *type) {
      auto inner = Visit(type->getInnerType());
      if (!inner)
        return Type();

      return { ParenType::get(Impl.SwiftContext, inner.AbstractType),
               inner.Hint };
    }

    ImportResult VisitTypedefType(const clang::TypedefType *type) {
      // Import the underlying declaration.
      auto decl = dyn_cast_or_null<TypeDecl>(Impl.importDecl(type->getDecl()));

      // If that fails, fall back on importing the underlying type.
      if (!decl) return Visit(type->desugar());

      Type mappedType = decl->getDeclaredType();
      ImportHint hint = ImportHint::None;

      // For certain special typedefs, we don't want to use the imported type.
      if (auto specialKind = Impl.getSpecialTypedefKind(type->getDecl())) {
        switch (specialKind.getValue()) {
        case MappedTypeNameKind::DoNothing:
        case MappedTypeNameKind::DefineAndUse:
          break;
        case MappedTypeNameKind::DefineOnly:
          mappedType = cast<TypeAliasDecl>(decl)->getUnderlyingType();
          break;
        }

        if (type->getDecl()->getName() == "BOOL") {
          hint = ImportHint::BOOL;
        } else if (type->getDecl()->getName() == "NSUInteger") {
          hint = ImportHint::NSUInteger;
        } else if (isImportedCFPointer(mappedType)) {
          hint = ImportHint::CFPointer;
        } else if (mappedType->isAnyExistentialType()) { // id, Class
          hint = ImportHint::ObjCPointer;
        }
        // Any other interesting mapped types should be hinted here.

      // Otherwise, recurse on the underlying type in order to compute
      // the hint correctly.
      } else {
        auto underlyingResult = Visit(type->desugar());
        assert(underlyingResult.AbstractType->isEqual(mappedType) &&
               "typedef without special typedef kind was mapped "
               "differently from its underlying type?");
        hint = underlyingResult.Hint;
      }

      return { mappedType, hint };
    }

#define SUGAR_TYPE(KIND)                                            \
    ImportResult Visit##KIND##Type(const clang::KIND##Type *type) { \
      return Visit(type->desugar());                                \
    }
    SUGAR_TYPE(Decayed)
    SUGAR_TYPE(TypeOfExpr)
    SUGAR_TYPE(TypeOf)
    SUGAR_TYPE(Decltype)
    SUGAR_TYPE(UnaryTransform)
    SUGAR_TYPE(Elaborated)
    SUGAR_TYPE(Attributed)
    SUGAR_TYPE(SubstTemplateTypeParm)
    SUGAR_TYPE(TemplateSpecialization)
    SUGAR_TYPE(Auto)
    SUGAR_TYPE(Adjusted)
    SUGAR_TYPE(PackExpansion)

    ImportResult VisitRecordType(const clang::RecordType *type) {
      auto decl = dyn_cast_or_null<TypeDecl>(Impl.importDecl(type->getDecl()));
      if (!decl)
        return nullptr;

      return decl->getDeclaredType();
    }

    ImportResult VisitEnumType(const clang::EnumType *type) {
      auto clangDecl = type->getDecl();
      switch (Impl.classifyEnum(clangDecl)) {
      case ClangImporter::Implementation::EnumKind::Constants: {
        auto clangDef = clangDecl->getDefinition();
        // Map anonymous enums with no fixed underlying type to Int /if/
        // they fit in an Int32. If not, this mapping isn't guaranteed to be
        // consistent for all platforms we care about.
        if (!clangDef->isFixed() &&
            clangDef->getNumPositiveBits() < 32 &&
            clangDef->getNumNegativeBits() <= 32)
          return Impl.getNamedSwiftType(Impl.getStdlibModule(), "Int");

        // Import the underlying integer type.
        return Visit(clangDecl->getIntegerType());
      }
      case ClangImporter::Implementation::EnumKind::Enum:
      case ClangImporter::Implementation::EnumKind::Unknown:
      case ClangImporter::Implementation::EnumKind::Options: {
        auto decl = dyn_cast_or_null<TypeDecl>(Impl.importDecl(clangDecl));
        if (!decl)
          return nullptr;

        return decl->getDeclaredType();
      }
      }
    }

    ImportResult VisitObjCObjectType(const clang::ObjCObjectType *type) {
      // If this is id<P> , turn this into a protocol type.
      // FIXME: What about Class<P>?
      if (type->isObjCQualifiedId()) {
        SmallVector<Type, 4> protocols;
        for (auto cp = type->qual_begin(), cpEnd = type->qual_end();
             cp != cpEnd; ++cp) {
          auto proto = cast_or_null<ProtocolDecl>(Impl.importDecl(*cp));
          if (!proto)
            return Type();

          protocols.push_back(proto->getDeclaredType());
        }

        return { ProtocolCompositionType::get(Impl.SwiftContext, protocols),
                 ImportHint::ObjCPointer };
      }

      // FIXME: Swift cannot express qualified object pointer types, e.g.,
      // NSObject<Proto>, so we drop the <Proto> part.
      return Visit(type->getBaseType());
    }

    ImportResult VisitObjCInterfaceType(const clang::ObjCInterfaceType *type) {
      auto imported = cast_or_null<ClassDecl>(Impl.importDecl(type->getDecl()));
      if (!imported)
        return nullptr;

      Type importedType = imported->getDeclaredType();

      if (imported->hasName() &&
          imported->getName().str() == "NSString") {
        return { importedType, ImportHint::NSString };
      }

      return { importedType, ImportHint::ObjCPointer };
    }

    ImportResult
    VisitObjCObjectPointerType(const clang::ObjCObjectPointerType *type) {
      // If this object pointer refers to an Objective-C class (possibly
      // qualified),
      if (auto interface = type->getInterfaceType()) {
        // FIXME: Swift cannot express qualified object pointer types, e.g.,
        // NSObject<Proto>, so we drop the <Proto> part.
        return VisitObjCInterfaceType(interface);
      }

      // If this is id<P>, turn this into a protocol type.
      // FIXME: What about Class<P>?
      if (type->isObjCQualifiedIdType()) {
        SmallVector<Type, 4> protocols;
        for (auto cp = type->qual_begin(), cpEnd = type->qual_end();
             cp != cpEnd; ++cp) {
          auto proto = cast_or_null<ProtocolDecl>(Impl.importDecl(*cp));
          if (!proto)
            return Type();

          protocols.push_back(proto->getDeclaredType());
        }

        return { ProtocolCompositionType::get(Impl.SwiftContext, protocols),
                 ImportHint::ObjCPointer };
      }
      
      // Beyond here, we're using AnyObject.
      auto proto = Impl.SwiftContext.getProtocol(
                     KnownProtocolKind::AnyObject);
      if (!proto)
        return Type();

      // id maps to AnyObject.
      if (type->isObjCIdType()) {
        return { proto->getDeclaredType(), ImportHint::ObjCPointer };
      }

      // Class maps to AnyObject.Type.
      assert(type->isObjCClassType() || type->isObjCQualifiedClassType());
      return { ExistentialMetatypeType::get(proto->getDeclaredType()),
               ImportHint::ObjCPointer };
    }
  };
}

/// Import a C pointer type as a function parameter.
///
/// This function assumes that the abstract imported type is either
/// COpaquePointer or UnsafePointer<T> for some T.
static Type importParameterPointerType(ClangImporter::Implementation &impl,
                                       clang::QualType clangType,
                                       Type abstractImportedType) {
  auto clangPointeeType = clangType->getPointeeType();
  clang::Qualifiers quals = clangPointeeType.getQualifiers();

  // Give no special treatment to volatile or __weak pointers.
  // FIXME: we really shouldn't even import __weak pointers as
  // UnsafePointer<>.
  if (quals.hasVolatile() ||
      quals.getObjCLifetime() == clang::Qualifiers::OCL_Weak)
    return abstractImportedType;

  if (clangPointeeType->isVoidType()) {
    // Pointers to unmappable or void types map to C*VoidPointer.
    if (quals.hasConst())
      return impl.getNamedSwiftType(impl.getStdlibModule(),
                                    "CConstVoidPointer");
    else
      return impl.getNamedSwiftType(impl.getStdlibModule(),
                                    "CMutableVoidPointer");
  }

  // Otherwise, we should have an UnsafePointer<T>.
  Type pointeeType =
    abstractImportedType->castTo<BoundGenericType>()->getGenericArgs()[0];

  // Const pointers map to CConstPointer<T>.
  if (quals.hasConst()) {
    return impl.getNamedSwiftTypeSpecialization(impl.getStdlibModule(),
                                                "CConstPointer", pointeeType);

  // Mutable pointers with __autoreleasing or __unsafe_unretained
  // ownership map to ObjCMutablePointer<T>.
  } else if (quals.getObjCLifetime() == clang::Qualifiers::OCL_Autoreleasing ||
             quals.getObjCLifetime() == clang::Qualifiers::OCL_ExplicitNone) {
    return impl.getNamedSwiftTypeSpecialization(impl.getStdlibModule(),
                                                "ObjCMutablePointer", pointeeType);

  // All other mutable pointers map to CMutablePointer<T>.
  } else {
    return impl.getNamedSwiftTypeSpecialization(impl.getStdlibModule(),
                                                "CMutablePointer", pointeeType);
  }
}

/// True if we're converting a function parameter, property type, or
/// function result type, and can thus safely apply representation
/// conversions for bridged types.
static bool canBridgeTypes(ImportTypeKind importKind) {
  return importKind == ImportTypeKind::Parameter ||
         importKind == ImportTypeKind::Result ||
         importKind == ImportTypeKind::Property;
}

/// Wrap a type in the Optional type appropriate to the import kind.
static Type getOptionalType(Type payloadType, ImportTypeKind kind) {
  // Import pointee types as true Optional.
  if (kind == ImportTypeKind::Pointee)
    return OptionalType::get(payloadType);
  // Otherwise, import as ImplicitlyUnwrappedOptional.
  return ImplicitlyUnwrappedOptionalType::get(payloadType);
}

/// Turn T into Unmanaged<T>.
static Type getUnmanagedType(ClangImporter::Implementation &impl,
                             Type payloadType) {
  Module *stdlib = impl.getStdlibModule();
  if (!stdlib) return payloadType;
  Type unmanagedType = impl.getNamedSwiftType(stdlib, "Unmanaged");
  if (!unmanagedType) return payloadType;
  auto unboundTy = unmanagedType->getAs<UnboundGenericType>();
  if (!unboundTy || unboundTy->getDecl()->getGenericParams()->size() != 1)
    return payloadType;

  Type unmanagedClassType =
    BoundGenericType::get(unboundTy->getDecl(), /*parent*/ Type(),
                          payloadType);
  return unmanagedClassType;
}

static Type adjustTypeForConcreteImport(ClangImporter::Implementation &impl,
                                        clang::QualType clangType,
                                        Type importedType,
                                        ImportTypeKind importKind,
                                        ImportHint hint) {
  if (importKind == ImportTypeKind::Abstract) {
    return importedType;
  }

  // 'void' can only be imported as a function result type.
  if (hint == ImportHint::Void &&
      importKind == ImportTypeKind::Result) {
    return impl.getNamedSwiftType(impl.getStdlibModule(), "Void");
  }

  // Reference types are only permitted as function parameter types.
  if (hint == ImportHint::Reference &&
      importKind == ImportTypeKind::Parameter) {
    auto refType = clangType->castAs<clang::ReferenceType>();
    // Import the underlying type.
    auto objectType = impl.importType(refType->getPointeeType(),
                                      ImportTypeKind::Pointee);
    if (!objectType)
      return nullptr;

    return InOutType::get(objectType);
  }

  // For anything else, if we completely failed to import the type
  // abstractly, give up now.
  if (!importedType)
    return Type();

  // Pointer parameters have a number of special cases.
  if (importKind == ImportTypeKind::Parameter &&
      hint == ImportHint::CPointer) {
    return importParameterPointerType(impl, clangType, importedType);
  }

  // Turn block pointer types back into normal function types in any
  // context where bridging is possible.
  if (hint == ImportHint::Block && canBridgeTypes(importKind)) {
    auto fTy = importedType->castTo<FunctionType>();
    FunctionType::ExtInfo einfo =
      fTy->getExtInfo().withRepresentation(FunctionType::Representation::Thick);
    importedType = FunctionType::get(fTy->getInput(), fTy->getResult(), einfo);
  }

  // Turn BOOL into Bool in contexts that can bridge types.
  if (hint == ImportHint::BOOL && canBridgeTypes(importKind)) {
    return impl.getNamedSwiftType(impl.getStdlibModule(), "Bool");
  }

  // When NSUInteger is used as an enum's underlying type, make sure
  // it stays unsigned.
  if (hint == ImportHint::NSUInteger && importKind == ImportTypeKind::Enum) {
    return impl.getNamedSwiftType(impl.getStdlibModule(), "UInt");
  }

  // Wrap CF pointers up as unmanaged types.
  if (hint == ImportHint::CFPointer) {
    importedType = getUnmanagedType(impl, importedType);
  }

  // When NSString* is the type of a function parameter or a function
  // result type, map it to String.
  if (hint == ImportHint::NSString && canBridgeTypes(importKind) &&
      impl.hasFoundationModule()) {
    importedType = impl.getNamedSwiftType(impl.getStdlibModule(), "String");
  }

  // Wrap class, class protocol, function, and metatype types in an
  // optional type.
  if (hint == ImportHint::NSString ||
      hint == ImportHint::ObjCPointer ||
      hint == ImportHint::CFPointer ||
      hint == ImportHint::Block) {
    importedType = getOptionalType(importedType, importKind);
  }

  return importedType;
}

Type ClangImporter::Implementation::importType(clang::QualType type,
                                               ImportTypeKind importKind) {
  if (type.isNull())
    return Type();

  // The "built-in" Objective-C types id, Class, and SEL can actually be (and
  // are) defined within the library. Clang tracks the redefinition types
  // separately, so it can provide fallbacks in certain cases. For Swift, we
  // map the redefinition types back to the equivalent of the built-in types.
  // This bans some trickery that the redefinition types enable, but is a more
  // sane model overall.
  auto &clangContext = getClangASTContext();
  if (clangContext.getLangOpts().ObjC1) {
    if (clangContext.hasSameUnqualifiedType(
          type, clangContext.getObjCIdRedefinitionType()) &&
        !clangContext.hasSameUnqualifiedType(
           clangContext.getObjCIdType(),
           clangContext.getObjCIdRedefinitionType()))
      type = clangContext.getObjCIdType();
    else if (clangContext.hasSameUnqualifiedType(
                type, clangContext.getObjCClassRedefinitionType()) &&
             !clangContext.hasSameUnqualifiedType(
                clangContext.getObjCClassType(),
                clangContext.getObjCClassRedefinitionType()))
      type = clangContext.getObjCClassType();
    else if (clangContext.hasSameUnqualifiedType(
               type, clangContext.getObjCSelRedefinitionType()) &&
             !clangContext.hasSameUnqualifiedType(
                clangContext.getObjCSelType(),
                clangContext.getObjCSelRedefinitionType()))
      type = clangContext.getObjCSelType();
  }
  
  // Perform abstract conversion, ignoring how the type is actually used.
  SwiftTypeConverter converter(*this);
  auto importResult = converter.Visit(type);

  // Now fix up the type based on we're concretely using it.
  return adjustTypeForConcreteImport(*this, type, importResult.AbstractType,
                                     importKind, importResult.Hint);
}

Type ClangImporter::Implementation::importFunctionType(
       clang::QualType resultType,
       ArrayRef<const clang::ParmVarDecl *> params,
       bool isVariadic, bool isNoReturn,
       SmallVectorImpl<Pattern*> &bodyPatterns) {

  // Cannot import variadic types.
  if (isVariadic)
    return Type();

  // Import the result type.
  auto swiftResultTy = importType(resultType, ImportTypeKind::Result);
  if (!swiftResultTy)
    return Type();

  // Import the parameters.
  SmallVector<TupleTypeElt, 4> swiftArgParams;
  SmallVector<TupleTypeElt, 4> swiftBodyParams;
  SmallVector<TuplePatternElt, 4> argPatternElts;
  SmallVector<TuplePatternElt, 4> bodyPatternElts;
  unsigned index = 0;
  for (auto param : params) {
    auto paramTy = param->getType();
    if (paramTy->isVoidType()) {
      ++index;
      continue;
    }

    // Import the parameter type into Swift.
    Type swiftParamTy = importType(paramTy, ImportTypeKind::Parameter);
    if (!swiftParamTy)
      return Type();

    // Figure out the name for this parameter.
    Identifier bodyName = importName(param->getDeclName());

    // Note: C functions never have argument names.
    Identifier name;

    // Compute the pattern to put into the body.
    Pattern *bodyPattern;
    if (bodyName.empty()) {
      bodyPattern = new (SwiftContext) AnyPattern(SourceLoc());
    } else {
      // It doesn't actually matter which DeclContext we use, so just use the
      // imported header unit.
      auto bodyVar
        = createDeclWithClangNode<ParamDecl>(param,
                                       /*IsLet*/ true,
                                       SourceLoc(), name,
                                       importSourceLoc(param->getLocation()),
                                       bodyName, swiftParamTy, 
                                       importedHeaderUnit);
      bodyPattern = new (SwiftContext) NamedPattern(bodyVar);
    }
    bodyPattern->setType(swiftParamTy);
    bodyPattern
      = new (SwiftContext) TypedPattern(bodyPattern,
                                        TypeLoc::withoutLoc(swiftParamTy));
    bodyPattern->setType(swiftParamTy);
    bodyPatternElts.push_back(TuplePatternElt(bodyPattern));
    
    // Add the tuple elements for the function types.
    swiftArgParams.push_back(TupleTypeElt(swiftParamTy, name));
    swiftBodyParams.push_back(TupleTypeElt(swiftParamTy, bodyName));
    ++index;
  }

  // Form the parameter tuples.
  auto bodyParamsTy = TupleType::get(swiftBodyParams, SwiftContext);

  // Form the body patterns.
  bodyPatterns.push_back(TuplePattern::create(SwiftContext, SourceLoc(),
                                              bodyPatternElts, SourceLoc()));
  bodyPatterns.back()->setType(bodyParamsTy);  
  
  FunctionType::ExtInfo extInfo;
  extInfo = extInfo.withIsNoReturn(isNoReturn);
  
  // Form the function type.
  auto argTy = TupleType::get(swiftArgParams, SwiftContext);
  return FunctionType::get(argTy, swiftResultTy, extInfo);
}

Type ClangImporter::Implementation::importMethodType(
       clang::QualType resultType,
       ArrayRef<const clang::ParmVarDecl *> params,
       bool isVariadic, bool isNoReturn,
       SmallVectorImpl<Pattern*> &bodyPatterns,
       DeclName methodName,
       SpecialMethodKind kind) {

  // Cannot import variadic types.
  if (isVariadic)
    return Type();

  // Import the result type.
  auto swiftResultTy = importType(resultType, ImportTypeKind::Result);
  if (!swiftResultTy)
    return Type();

  // Import the parameters.
  SmallVector<TupleTypeElt, 4> swiftArgParams;
  SmallVector<TupleTypeElt, 4> swiftBodyParams;
  SmallVector<TuplePatternElt, 4> bodyPatternElts;
  auto argNames = methodName.getArgumentNames();
  unsigned index = 0;
  for (auto param : params) {
    auto paramTy = param->getType();
    if (paramTy->isVoidType()) {
      ++index;
      continue;
    }

    // Import the parameter type into Swift.
    Type swiftParamTy;
    if (kind == SpecialMethodKind::NSDictionarySubscriptGetter &&
        paramTy->isObjCIdType()) {
      swiftParamTy = getOptionalType(getNSCopyingType(),
                                     ImportTypeKind::Parameter);
    }
    if (!swiftParamTy)
      swiftParamTy = importType(paramTy, ImportTypeKind::Parameter);
    if (!swiftParamTy)
      return Type();

    // Figure out the name for this parameter.
    Identifier bodyName = importName(param->getDeclName());

    // Figure out the name for this argument, which comes from the method name.
    Identifier name;
    if (index < argNames.size()) {
      name = argNames[index];
    }

    // Compute the pattern to put into the body.
    Pattern *bodyPattern;
    if (bodyName.empty()) {
      bodyPattern = new (SwiftContext) AnyPattern(SourceLoc());
    } else {
      // It doesn't actually matter which DeclContext we use, so just use the
      // imported header unit.
      auto bodyVar
        = createDeclWithClangNode<ParamDecl>(param,
                                       /*IsLet*/ true, SourceLoc(), name,
                                       importSourceLoc(param->getLocation()),
                                       bodyName, swiftParamTy, 
                                       importedHeaderUnit);
      bodyPattern = new (SwiftContext) NamedPattern(bodyVar);
    }
    bodyPattern->setType(swiftParamTy);
    bodyPattern
      = new (SwiftContext) TypedPattern(bodyPattern,
                                        TypeLoc::withoutLoc(swiftParamTy));
    bodyPattern->setType(swiftParamTy);
    bodyPatternElts.push_back(TuplePatternElt(bodyPattern));

    // Add the tuple elements for the function types.
    swiftArgParams.push_back(TupleTypeElt(swiftParamTy, name));
    swiftBodyParams.push_back(TupleTypeElt(swiftParamTy, bodyName));
    ++index;
  }

  // If we have a constructor with no parameters and a name with an
  // argument name, synthesize a Void parameter with that name.
  if (kind == SpecialMethodKind::Constructor && params.empty() && 
      argNames.size() == 1) {
    // It doesn't actually matter which DeclContext we use, so just use the
    // imported header unit.
    auto argName = argNames[0];
    auto type = TupleType::getEmpty(SwiftContext);
    auto var = new (SwiftContext) ParamDecl(/*IsLet*/ true,
                                            SourceLoc(), argName,
                                            SourceLoc(), argName, type,
                                            importedHeaderUnit);
    Pattern *pattern = new (SwiftContext) NamedPattern(var);
    pattern->setType(type);
    pattern = new (SwiftContext) TypedPattern(pattern,
                                              TypeLoc::withoutLoc(type));
    pattern->setType(type);
    
    bodyPatternElts.push_back(TuplePatternElt(pattern));
    swiftArgParams.push_back(TupleTypeElt(type, argName));
    swiftBodyParams.push_back(TupleTypeElt(type, argName));
  }

  // Form the parameter tuple.
  auto bodyParamsTy = TupleType::get(swiftBodyParams, SwiftContext);
  
  // Form the body patterns.
  bodyPatterns.push_back(TuplePattern::create(SwiftContext, SourceLoc(),
                                              bodyPatternElts, SourceLoc()));
  bodyPatterns.back()->setType(bodyParamsTy);
  
  FunctionType::ExtInfo extInfo;
  extInfo = extInfo.withIsNoReturn(isNoReturn);
  
  // Form the function type.
  auto argTy = TupleType::get(swiftArgParams, SwiftContext);
  return FunctionType::get(argTy, swiftResultTy, extInfo);
}

Module *ClangImporter::Implementation::getStdlibModule() {
  return SwiftContext.getStdlibModule();
}

Module *ClangImporter::Implementation::getNamedModule(StringRef name) {
  return SwiftContext.getLoadedModule(SwiftContext.getIdentifier(name));
}

bool ClangImporter::Implementation::hasFoundationModule() {
  if (!checkedFoundationModule) {
    Identifier name = SwiftContext.getIdentifier(FOUNDATION_MODULE_NAME);
    auto mod = SwiftContext.getModule({ {name, SourceLoc()} });
    checkedFoundationModule = (mod != nullptr);
  }
  return checkedFoundationModule.getValue();
}


Type ClangImporter::Implementation::getNamedSwiftType(Module *module,
                                                      StringRef name) {
  if (!module)
    return Type();

  // Look for the type.
  UnqualifiedLookup lookup(SwiftContext.getIdentifier(name), module,
                           getTypeResolver());
  if (auto type = lookup.getSingleTypeResult()) {
    return type->getDeclaredType();
  }

  return Type();
}

Type
ClangImporter::Implementation::
getNamedSwiftTypeSpecialization(Module *module, StringRef name,
                                ArrayRef<Type> args) {
  if (!module)
    return Type();

  UnqualifiedLookup lookup(SwiftContext.getIdentifier(name), module,
                           getTypeResolver());
  if (TypeDecl *typeDecl = lookup.getSingleTypeResult()) {
    if (auto nominalDecl = dyn_cast<NominalTypeDecl>(typeDecl)) {
      if (auto params = nominalDecl->getGenericParams()) {
        if (params->size() == args.size()) {
          auto *BGT = BoundGenericType::get(nominalDecl, Type(), args);
          // FIXME: How do we ensure that this type gets validated?
          // Instead of going through the type checker, we do this hack to
          // create substitutions.
          SwiftContext.createTrivialSubstitutions(
              BGT->getCanonicalType()->castTo<BoundGenericType>());
          return BGT;
        }
      }
    }
  }

  return Type();
}

Type ClangImporter::Implementation::getNSObjectType() {
  if (NSObjectTy)
    return NSObjectTy;

  auto &sema = Instance->getSema();

  // Map the name. If we can't represent the Swift name in Clang, bail out now.
  auto clangName = &getClangASTContext().Idents.get("NSObject");

  // Perform name lookup into the global scope.
  // FIXME: Map source locations over.
  clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                   clang::Sema::LookupOrdinaryName);
  if (!sema.LookupName(lookupResult, /*Scope=*/0)) {
    return Type();
  }

  for (auto decl : lookupResult) {
    if (auto swiftDecl = importDecl(decl->getUnderlyingDecl())) {
      if (auto classDecl = dyn_cast<ClassDecl>(swiftDecl)) {
        NSObjectTy = classDecl->getDeclaredType();
        return NSObjectTy;
      }
    }
  }

  return Type();
}

Type ClangImporter::Implementation::getNSCopyingType() {
  auto &sema = Instance->getSema();
  auto clangName = &getClangASTContext().Idents.get("NSCopying");
  assert(clangName);

  // Perform name lookup into the global scope.
  clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                   clang::Sema::LookupObjCProtocolName);
  if (!sema.LookupName(lookupResult, /*Scope=*/0))
    return Type();

  for (auto decl : lookupResult) {
    if (auto swiftDecl = importDecl(decl->getUnderlyingDecl())) {
      if (auto protoDecl = dyn_cast<ProtocolDecl>(swiftDecl)) {
        return protoDecl->getDeclaredType();
      }
    }
  }

  return Type();
}

