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
#include "swift/ClangImporter/ClangModule.h"
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
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"

using namespace swift;

/// Given that a type is the result of a special typedef import, was
/// it originally a CF pointer?
static bool isImportedCFPointer(clang::QualType clangType, Type type) {
  return (clangType->isPointerType() &&
          (type->is<ClassType>() || type->isClassExistentialType()));
}

namespace {
  /// Various types that we want to do something interesting to after
  /// importing them.
  struct ImportHint {
    enum ImportHintKind {
      /// There is nothing special about the source type.
      None,

      /// The source type is 'void'.
      Void,

      /// The source type is 'BOOL'.
      BOOL,

      /// The source type is 'NSString'.
      NSString,

      /// The source type is 'NSArray'.
      NSArray,

      /// The source type is 'NSDictionary'.
      NSDictionary,

      /// The source type is 'NSSet'.
      NSSet,

      /// The source type is 'NSUInteger'.
      NSUInteger,

      /// The source type is an Objective-C object pointer type.
      ObjCPointer,

      /// The source type is a CF object pointer type.
      CFPointer,

      /// The source type is a C++ reference type.
      Reference,

      /// The source type is a block pointer type.
      Block,
      
      /// The source type is a function pointer type.
      CFunctionPointer,
    };

    ImportHintKind Kind;

    // Type arguments, if provided.
    Type TypeArgs[2];

    /// Allow conversion from an import hint to an import hint kind,
    /// which is useful for switches and comparisons.
    operator ImportHintKind() const { return Kind; }

    /// Determine the number of type arguments we expect.
    static unsigned getNumTypeArgs(ImportHintKind kind) {
      switch (kind) {
      case None:
      case Void:
      case BOOL:
      case NSString:
      case NSUInteger:
      case ObjCPointer:
      case CFPointer:
      case Reference:
      case Block:
      case CFunctionPointer:
        return 0;

      case NSArray:
      case NSSet:
        return 1;

      case NSDictionary:
        return 2;
      }
    }

    ImportHint(ImportHintKind kind) : Kind(kind) {
      assert(getNumTypeArgs(kind) == 0 && "Wrong number of arguments");
    }

    ImportHint(ImportHintKind kind, Type typeArg1) : Kind(kind) {
      assert(getNumTypeArgs(kind) == 1 && "Wrong number of arguments");
      TypeArgs[0] = typeArg1;
    }

    ImportHint(ImportHintKind kind, Type typeArg1, Type typeArg2) : Kind(kind) {
      assert(getNumTypeArgs(kind) == 2 && "Wrong number of arguments");
      TypeArgs[0] = typeArg1;
      TypeArgs[1] = typeArg2;
    }
  };

  bool canImportAsOptional(ImportHint hint) {
    switch (hint) {
    case ImportHint::BOOL:
    case ImportHint::None:
    case ImportHint::NSUInteger:
    case ImportHint::Reference:
    case ImportHint::Void:
      return false;

    case ImportHint::Block:
    case ImportHint::CFPointer:
    case ImportHint::NSArray:
    case ImportHint::NSDictionary:
    case ImportHint::NSSet:
    case ImportHint::NSString:
    case ImportHint::ObjCPointer:
    case ImportHint::CFunctionPointer:
      return true;
    }
  }

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

  /// Wrap a type in the Optional type appropriate to the import kind.
  static Type
  getOptionalType(Type payloadType,
                  ImportTypeKind kind,
                  OptionalTypeKind OptKind = OTK_ImplicitlyUnwrappedOptional) {
    // Import pointee types as true Optional.
    if (kind == ImportTypeKind::Pointee)
      return OptionalType::get(payloadType);

    switch (OptKind) {
      case OTK_ImplicitlyUnwrappedOptional:
        return ImplicitlyUnwrappedOptionalType::get(payloadType);
      case OTK_None:
        return payloadType;
      case OTK_Optional:
        return OptionalType::get(payloadType);
    }
  }

  class SwiftTypeConverter :
    public clang::TypeVisitor<SwiftTypeConverter, ImportResult>
  {
    ClangImporter::Implementation &Impl;
    bool IsUsedInSystemModule;
    
  public:
    SwiftTypeConverter(ClangImporter::Implementation &impl,
                       bool isUsedInSystemModule)
      : Impl(impl), IsUsedInSystemModule(isUsedInSystemModule) {}

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
    
    // Given a loaded type like CInt, look through the name alias sugar that the
    // stdlib uses to show the underlying type.  We want to import the signature
    // of the exit(3) libc function as "func exit(Int32)", not as
    // "func exit(CInt)".
    static Type unwrapCType(Type T) {
      if (auto *NAT = dyn_cast_or_null<NameAliasType>(T.getPointer()))
        return NAT->getSinglyDesugaredType();
      return T;
    }
    
    ImportResult VisitBuiltinType(const clang::BuiltinType *type) {
      switch (type->getKind()) {
      case clang::BuiltinType::Void:
        return { Type(), ImportHint::Void };

#define MAP_BUILTIN_TYPE(CLANG_BUILTIN_KIND, SWIFT_TYPE_NAME)             \
      case clang::BuiltinType::CLANG_BUILTIN_KIND:                        \
        return unwrapCType(Impl.getNamedSwiftType(Impl.getStdlibModule(), \
                                        #SWIFT_TYPE_NAME));
          
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
      auto pointeeQualType = type->getPointeeType();

      // Special case for NSZone*, which has its own Swift wrapper.
      if (const clang::RecordType *pointee =
            pointeeQualType->getAsStructureType()) {
        if (pointee && !pointee->getDecl()->isCompleteDefinition() &&
            pointee->getDecl()->getName() == "_NSZone") {
          Module *objCModule = Impl.getNamedModule(OBJC_MODULE_NAME);
          Type wrapperTy = Impl.getNamedSwiftType(objCModule, "NSZone");
          if (wrapperTy)
            return wrapperTy;
        }
      }
      
      // All other C pointers to concrete types map to
      // UnsafeMutablePointer<T> or COpaquePointer (FIXME:, except in
      // parameter position under the pre-
      // intrinsic-pointer-conversion regime.)

      // With pointer conversions enabled, map to the normal pointer types
      // without special hints.
      Type pointeeType;
      if (pointeeQualType->isVoidType())
        pointeeType = Impl.getNamedSwiftType(Impl.getStdlibModule(), "Void");
      else
        pointeeType = Impl.importType(pointeeQualType,
                                      ImportTypeKind::Pointee,
                                      IsUsedInSystemModule);

      // If the pointed-to type is unrepresentable in Swift, import as
      // COpaquePointer.
      if (!pointeeType)
        return getOpaquePointerType();
      
      if (pointeeQualType->isFunctionType()) {
        if (Impl.SwiftContext.LangOpts.EnableCFunctionPointers) {
          auto funcTy = pointeeType->castTo<FunctionType>();
          return {
            FunctionType::get(funcTy->getInput(), funcTy->getResult(),
              funcTy->getExtInfo().withRepresentation(
                            AnyFunctionType::Representation::CFunctionPointer)),
            ImportHint::CFunctionPointer
          };
        }
      
        return Impl.getNamedSwiftTypeSpecialization(Impl.getStdlibModule(),
                                                    "CFunctionPointer",
                                                    pointeeType);
      }

      auto quals = pointeeQualType.getQualifiers();
      
      if (quals.hasConst())
        return {Impl.getNamedSwiftTypeSpecialization(Impl.getStdlibModule(),
                                                     "UnsafePointer",
                                                     pointeeType),
                ImportHint::None};
      // Mutable pointers with __autoreleasing or __unsafe_unretained
      // ownership map to AutoreleasingUnsafeMutablePointer<T>.
      else if (quals.getObjCLifetime() == clang::Qualifiers::OCL_Autoreleasing
            || quals.getObjCLifetime() == clang::Qualifiers::OCL_ExplicitNone)
        return {
          Impl.getNamedSwiftTypeSpecialization(
            Impl.getStdlibModule(), "AutoreleasingUnsafeMutablePointer",
            pointeeType),
            ImportHint::None};
      // All other mutable pointers map to UnsafeMutablePointer.
      return {Impl.getNamedSwiftTypeSpecialization(Impl.getStdlibModule(),
                                                   "UnsafeMutablePointer",
                                                   pointeeType),
              ImportHint::None};
    }

    Type getOpaquePointerType() {
      return Impl.getNamedSwiftType(Impl.getStdlibModule(), "COpaquePointer");
    }

    ImportResult VisitBlockPointerType(const clang::BlockPointerType *type) {
      // Block pointer types are mapped to function types.
      Type pointeeType = Impl.importType(type->getPointeeType(),
                                         ImportTypeKind::Abstract,
                                         IsUsedInSystemModule);
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
                                         ImportTypeKind::Pointee,
                                         IsUsedInSystemModule);
      if (!elementType)
        return Type();
      
      TupleTypeElt elt(elementType);
      SmallVector<TupleTypeElt, 8> elts;
      for (size_t i = 0, size = type->getSize().getZExtValue(); i < size; ++i)
        elts.push_back(elt);
      
      return TupleType::get(elts, elementType->getASTContext());
    }

    ImportResult VisitVectorType(const clang::VectorType *type) {
      // We can map this into a type from the SIMD module, if it exists.
      if (!Impl.SwiftContext.LangOpts.EnableSIMDImport)
        return Type();
      
      auto *SIMD = Impl.tryLoadSIMDModule();
      if (!SIMD)
        return Type();
      
      // Map the element type and count to a Swift name, such as
      // float x 3 => Float3.
      SmallString<16> name;
      {
        llvm::raw_svector_ostream names(name);
        
        if (auto builtinTy
              = dyn_cast<clang::BuiltinType>(type->getElementType())){
          switch (builtinTy->getKind()) {
#define MAP_SIMD_TYPE(TYPE_NAME, BUILTIN_KIND)   \
          case clang::BuiltinType::BUILTIN_KIND: \
            names << #TYPE_NAME;                 \
            break;
#include "swift/ClangImporter/SIMDMappedTypes.def"
          default:
            // A vector type we don't know how to map.
            return Type();
          }
        } else {
          return Type();
        }
        
        names << type->getNumElements();
        names.flush();
      }
      
      return Impl.getNamedSwiftType(SIMD, name);
    }

    ImportResult VisitFunctionProtoType(const clang::FunctionProtoType *type) {
      // C-style variadic functions cannot be called from Swift.
      if (type->isVariadic())
        return Type();

      // Import the result type.  We currently provide no mechanism
      // for this to be audited.
      auto resultTy = Impl.importType(type->getReturnType(),
                                      ImportTypeKind::Result,
                                      IsUsedInSystemModule);
      if (!resultTy)
        return Type();

      SmallVector<TupleTypeElt, 4> params;
      for (auto param = type->param_type_begin(),
             paramEnd = type->param_type_end();
           param != paramEnd; ++param) {
        auto swiftParamTy = Impl.importType(*param, ImportTypeKind::Parameter,
                                            IsUsedInSystemModule);
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
                                      ImportTypeKind::Result,
                                      IsUsedInSystemModule);
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
      // If the underlying declaration is an Objective-C type parameter,
      // import the underlying sugar instead.
      if (isa<clang::ObjCTypeParamDecl>(type->getDecl()))
        return Visit(type->desugar());

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
        } else if (isImportedCFPointer(type->desugar(), mappedType)) {
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
    SUGAR_TYPE(TypeOfExpr)
    SUGAR_TYPE(TypeOf)
    SUGAR_TYPE(Decltype)
    SUGAR_TYPE(UnaryTransform)
    SUGAR_TYPE(Elaborated)
    SUGAR_TYPE(SubstTemplateTypeParm)
    SUGAR_TYPE(TemplateSpecialization)
    SUGAR_TYPE(Auto)
    SUGAR_TYPE(Adjusted)
    SUGAR_TYPE(PackExpansion)

    ImportResult VisitAttributedType(const clang::AttributedType *type) {
      return Visit(type->desugar());
    }

    ImportResult VisitDecayedType(const clang::DecayedType *type) {
      clang::ASTContext &clangCtx = Impl.getClangASTContext();
      if (clangCtx.hasSameType(type->getOriginalType(),
                               clangCtx.getBuiltinVaListType()))
        return Impl.getNamedSwiftType(Impl.getStdlibModule(), "CVaListPointer");
      return Visit(type->desugar());
    }

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
      // We only handle pointers to objects.
      return nullptr;
    }

    ImportResult
    VisitObjCObjectPointerType(const clang::ObjCObjectPointerType *type) {
      // If this object pointer refers to an Objective-C class (possibly
      // qualified),
      if (auto objcClass = type->getInterfaceDecl()) {
        auto imported = cast_or_null<ClassDecl>(Impl.importDecl(objcClass));
        if (!imported)
          return nullptr;

        Type importedType = imported->getDeclaredType();

        if (imported->hasName() &&
            imported->getName().str() == "NSString") {
          return { importedType, ImportHint::NSString };
        }

        if (imported->hasName() && imported->getName().str() == "NSArray") {
          // If we have type arguments, import them.
          ArrayRef<clang::QualType> typeArgs = type->getTypeArgs();
          if (typeArgs.size() == 1) {
            Type elementType = Impl.importType(typeArgs[0],
                                               ImportTypeKind::BridgedValue,
                                               IsUsedInSystemModule,
                                               OTK_None);
            return { importedType,
                     ImportHint(ImportHint::NSArray, elementType) };
          }

          return { importedType, ImportHint(ImportHint::NSArray, Type()) };
        }

        if (imported->hasName() && imported->getName().str() == "NSDictionary") {
          // If we have type arguments, import them.
          ArrayRef<clang::QualType> typeArgs = type->getTypeArgs();
          if (typeArgs.size() == 2) {
            Type keyType = Impl.importType(typeArgs[0],
                                           ImportTypeKind::BridgedValue,
                                           IsUsedInSystemModule,
                                           OTK_None);
            Type objectType = Impl.importType(typeArgs[1],
                                              ImportTypeKind::BridgedValue,
                                              IsUsedInSystemModule,
                                              OTK_None);
            if (keyType.isNull() != objectType.isNull()) {
              keyType = nullptr;
              objectType = nullptr;
            }

            return { importedType,
                     ImportHint(ImportHint::NSDictionary,
                                keyType, objectType) };
          }
          return { importedType,
                   ImportHint(ImportHint::NSDictionary, Type(), Type()) };
        }

        if (imported->hasName() && imported->getName().str() == "NSSet") {
          // If we have type arguments, import them.
          ArrayRef<clang::QualType> typeArgs = type->getTypeArgs();
          if (typeArgs.size() == 1) {
            Type elementType = Impl.importType(typeArgs[0],
                                               ImportTypeKind::BridgedValue,
                                               IsUsedInSystemModule,
                                               OTK_None);
            return { importedType,
                     ImportHint(ImportHint::NSSet, elementType) };
          }

          return { importedType, ImportHint(ImportHint::NSSet, Type()) };
        }

        return { importedType, ImportHint::ObjCPointer };
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

/// True if we're converting a function parameter, property type, or
/// function result type, and can thus safely apply representation
/// conversions for bridged types.
static bool canBridgeTypes(ImportTypeKind importKind) {
  switch (importKind) {
  case ImportTypeKind::Abstract:
  case ImportTypeKind::Value:
  case ImportTypeKind::Variable:
  case ImportTypeKind::AuditedVariable:
  case ImportTypeKind::Pointee:
  case ImportTypeKind::Enum:
  case ImportTypeKind::RecordField:
    return false;
  case ImportTypeKind::Result:
  case ImportTypeKind::AuditedResult:
  case ImportTypeKind::Parameter:
  case ImportTypeKind::CFRetainedOutParameter:
  case ImportTypeKind::CFUnretainedOutParameter:
  case ImportTypeKind::Property:
  case ImportTypeKind::PropertyAccessor:
  case ImportTypeKind::BridgedValue:
    return true;
  }
}

/// True if the type has known CoreFoundation reference counting semantics.
static bool isCFAudited(ImportTypeKind importKind) {
  switch (importKind) {
  case ImportTypeKind::Abstract:
  case ImportTypeKind::Value:
  case ImportTypeKind::BridgedValue:
  case ImportTypeKind::Variable:
  case ImportTypeKind::Result:
  case ImportTypeKind::Pointee:
  case ImportTypeKind::Enum:
  case ImportTypeKind::RecordField:
    return false;
  case ImportTypeKind::AuditedVariable:
  case ImportTypeKind::AuditedResult:
  case ImportTypeKind::Parameter:
  case ImportTypeKind::CFRetainedOutParameter:
  case ImportTypeKind::CFUnretainedOutParameter:
  case ImportTypeKind::Property:
  case ImportTypeKind::PropertyAccessor:
    return true;
  }
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
                                        ImportHint hint,
                                        bool isUsedInSystemModule,
                                        OptionalTypeKind optKind) {
  if (importKind == ImportTypeKind::Abstract) {
    return importedType;
  }

  // 'void' can only be imported as a function result type.
  if (hint == ImportHint::Void &&
      (importKind == ImportTypeKind::AuditedResult ||
       importKind == ImportTypeKind::Result ||
       importKind == ImportTypeKind::PropertyAccessor)) {
    return impl.getNamedSwiftType(impl.getStdlibModule(), "Void");
  }

  // Import NSString * globals as String.
  if (hint == ImportHint::NSString &&
      (importKind == ImportTypeKind::Variable ||
       importKind == ImportTypeKind::AuditedVariable)) {
    return impl.getNamedSwiftType(impl.getStdlibModule(), "String");
  }

  // Reference types are only permitted as function parameter types.
  if (hint == ImportHint::Reference &&
      importKind == ImportTypeKind::Parameter) {
    auto refType = clangType->castAs<clang::ReferenceType>();
    // Import the underlying type.
    auto objectType = impl.importType(refType->getPointeeType(),
                                      ImportTypeKind::Pointee,
                                      isUsedInSystemModule);
    if (!objectType)
      return nullptr;

    return InOutType::get(objectType);
  }

  // For anything else, if we completely failed to import the type
  // abstractly, give up now.
  if (!importedType)
    return Type();

  // Special case AutoreleasingUnsafeMutablePointer<NSError?> parameters.
  auto maybeImportNSErrorPointer = [&]() -> Type {
    if (importKind != ImportTypeKind::Parameter)
      return Type();

    PointerTypeKind PTK;
    auto elementType = importedType->getAnyPointerElementType(PTK);
    if (!elementType || PTK != PTK_AutoreleasingUnsafeMutablePointer)
      return Type();

    auto elementObj = elementType->getAnyOptionalObjectType();
    if (!elementObj)
      return Type();

    auto elementClass = elementObj->getClassOrBoundGenericClass();
    if (!elementClass)
      return Type();

    // FIXME: Avoid string comparison by caching this identifier.
    if (elementClass->getName().str() != "NSError")
      return Type();

    Module *foundationModule = impl.tryLoadFoundationModule();
    if (!foundationModule ||
        foundationModule->getName()
          != elementClass->getModuleContext()->getName())
      return Type();

    return impl.getNamedSwiftType(foundationModule, "NSErrorPointer");
  };
  if (Type result = maybeImportNSErrorPointer())
    return result;

  auto maybeImportCFOutParameter = [&]() -> Type {
    if (importKind != ImportTypeKind::CFRetainedOutParameter &&
        importKind != ImportTypeKind::CFUnretainedOutParameter) {
      return Type();
    }

    PointerTypeKind PTK;
    auto elementType = importedType->getAnyPointerElementType(PTK);
    if (!elementType || PTK != PTK_UnsafeMutablePointer)
      return Type();

    OptionalTypeKind OTK;
    auto insideOptionalType = elementType->getAnyOptionalObjectType(OTK);
    if (!insideOptionalType)
      insideOptionalType = elementType;

    auto boundGenericTy = insideOptionalType->getAs<BoundGenericType>();
    if (!boundGenericTy)
      return Type();

    auto boundGenericBase = boundGenericTy->getDecl();
    if (boundGenericBase->getName().str() != "Unmanaged" ||
        boundGenericBase->getModuleContext() != impl.getStdlibModule()) {
      return Type();
    }

    assert(boundGenericTy->getGenericArgs().size() == 1 &&
           "signature of Unmanaged has changed");

    auto resultTy = boundGenericTy->getGenericArgs().front();
    if (OTK != OTK_None)
      resultTy = OptionalType::get(OTK, resultTy);

    StringRef pointerName;
    if (importKind == ImportTypeKind::CFRetainedOutParameter)
      pointerName = "UnsafeMutablePointer";
    else
      pointerName = "AutoreleasingUnsafeMutablePointer";

    resultTy = impl.getNamedSwiftTypeSpecialization(impl.getStdlibModule(),
                                                    pointerName,
                                                    resultTy);
    return resultTy;
  };
  if (Type outParamTy = maybeImportCFOutParameter()) {
    importedType = outParamTy;
  }

  // Turn block pointer types back into normal function types in any
  // context where bridging is possible, unless the block has a typedef.
  if (hint == ImportHint::Block && canBridgeTypes(importKind) &&
      !isa<NameAliasType>(importedType.getPointer())) {
    auto fTy = importedType->castTo<FunctionType>();
    FunctionType::ExtInfo einfo =
      fTy->getExtInfo().withRepresentation(FunctionType::Representation::Swift);
    importedType = FunctionType::get(fTy->getInput(), fTy->getResult(), einfo);
  }

  // Turn BOOL into Bool in contexts that can bridge types.
  if (hint == ImportHint::BOOL && canBridgeTypes(importKind)) {
    return impl.getNamedSwiftType(impl.getStdlibModule(), "Bool");
  }

  // When NSUInteger is used as an enum's underlying type or if it does not come
  // from a system module, make sure it stays unsigned.
  if (hint == ImportHint::NSUInteger)
     if (importKind == ImportTypeKind::Enum || !isUsedInSystemModule) {
    return impl.getNamedSwiftType(impl.getStdlibModule(), "UInt");
  }

  // Wrap CF pointers up as unmanaged types, unless this is an audited
  // context.
  if (hint == ImportHint::CFPointer && !isCFAudited(importKind)) {
    importedType = getUnmanagedType(impl, importedType);
  }

  // When NSString* is the type of a function parameter or a function
  // result type, map it to String.
  // FIXME: It's not really safe to do this when Foundation is missing.
  // We do it anyway for ImportForwardDeclarations mode so that generated
  // interfaces are correct, but trying to use the resulting declarations
  // may result in compiler crashes further down the line.
  if (hint == ImportHint::NSString && canBridgeTypes(importKind) &&
      (impl.tryLoadFoundationModule() || impl.ImportForwardDeclarations)) {
    importedType = impl.getNamedSwiftType(impl.getStdlibModule(), "String");
  }


  // When NSArray* is the type of a function parameter or a function
  // result type, map it to [AnyObject].
  if (hint == ImportHint::NSArray && canBridgeTypes(importKind) &&
      impl.tryLoadFoundationModule()) {
    Type elementType = hint.TypeArgs[0];
    if (elementType.isNull())
      elementType = impl.getNamedSwiftType(impl.getStdlibModule(), "AnyObject");
    importedType = ArraySliceType::get(elementType);
  }

  // When NSDictionary* is the type of a function parameter or a function
  // result type, map it to [K : V].
  if (hint == ImportHint::NSDictionary && canBridgeTypes(importKind) &&
      impl.tryLoadFoundationModule()) {
    Type keyType = hint.TypeArgs[0];
    Type objectType = hint.TypeArgs[1];

    // If no key type was provided, or the key doesn't match the 'NSObject'
    // bound required by the Swift Dictionary key, substitute in 'NSObject'.
    if (keyType.isNull() || !impl.matchesNSObjectBound(keyType)) {
      keyType = impl.getNSObjectType();
    }

    if (objectType.isNull()) {
      objectType = impl.getNamedSwiftType(impl.getStdlibModule(), "AnyObject");
    }

    importedType = DictionaryType::get(keyType, objectType);
  }

  // When NSSet* is the type of a function parameter or a function
  // result type, map it to Set<T>.
  if (hint == ImportHint::NSSet && canBridgeTypes(importKind) &&
      impl.tryLoadFoundationModule()) {
    Type elementType = hint.TypeArgs[0];

    // If no element type was provided, or the element type doesn't match the
    // 'NSObject' bound required by the Swift Set, substitute in 'NSObject'.
    if (elementType.isNull() || !impl.matchesNSObjectBound(elementType))
      elementType = impl.getNSObjectType();

    importedType = impl.getNamedSwiftTypeSpecialization(impl.getStdlibModule(),
                                                        "Set",
                                                        elementType);
  }

  // Wrap class, class protocol, function, and metatype types in an
  // optional type.
  if (importedType && canImportAsOptional(hint)) {
    importedType = getOptionalType(importedType, importKind, optKind);
  }

  return importedType;
}

Type ClangImporter::Implementation::importType(clang::QualType type,
                                               ImportTypeKind importKind,
                                               bool isUsedInSystemModule,
                                               OptionalTypeKind optionality) {
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
  
  // If nullability is provided as part of the type, that overrides
  // optionality provided externally.
  if (auto nullability = type->getNullability(clangContext)) {
    optionality = translateNullability(*nullability);
  }

  // Perform abstract conversion, ignoring how the type is actually used.
  SwiftTypeConverter converter(*this, isUsedInSystemModule);
  auto importResult = converter.Visit(type);

  // Now fix up the type based on we're concretely using it.
  return adjustTypeForConcreteImport(*this, type, importResult.AbstractType,
                                     importKind, importResult.Hint,
                                     isUsedInSystemModule,
                                     optionality);
}

bool ClangImporter::Implementation::shouldImportGlobalAsLet(
       clang::QualType type)
{
  // Const variables should be imported as 'let'.
  if (type.isConstQualified()) {
    return true;
  }
  // Globals of type NSString * should be imported as 'let'.
  if (auto ptrType = type->getAs<clang::ObjCObjectPointerType>()) {
    if (auto interfaceType = ptrType->getInterfaceType()) {
      if (interfaceType->getDecl()->getName() == "NSString") {
        return true;
      }
    }
  }
  return false;
}

Type ClangImporter::Implementation::importPropertyType(
       const clang::ObjCPropertyDecl *decl,
       bool isFromSystemModule) {
  OptionalTypeKind optionality = OTK_ImplicitlyUnwrappedOptional;
  if (auto info = getKnownObjCProperty(decl)) {
    if (auto nullability = info->getNullability())
      optionality = translateNullability(*nullability);
  }
  return importType(decl->getType(), ImportTypeKind::Property,
                    isFromSystemModule, optionality);
}

/// Get a bit vector indicating which arguments are non-null for a
/// given function or method.
static llvm::SmallBitVector getNonNullArgs(
                              const clang::Decl *decl,
                              ArrayRef<const clang::ParmVarDecl *> params) {
  llvm::SmallBitVector result;
  if (!decl)
    return result;

  for (const auto *nonnull : decl->specific_attrs<clang::NonNullAttr>()) {
    if (!nonnull->args_size()) {
      // Easy case: all pointer arguments are non-null.
      if (result.empty())
        result.resize(params.size(), true);
      else
        result.set(0, params.size());

      return result;
    }

    // Mark each of the listed parameters as non-null.
    if (result.empty())
      result.resize(params.size(), false);

    for (unsigned idx : nonnull->args()) {
      if (idx < result.size())
        result.set(idx);
    }
  }

  return result;
}

/// Apply the @noescape attribute
static Type applyNoEscape(Type type) {
  // Recurse into optional types.
  OptionalTypeKind optKind;
  if (Type objectType = type->getAnyOptionalObjectType(optKind)) {
    return OptionalType::get(optKind, applyNoEscape(objectType));
  }

  // Apply @noescape to function types.
  if (auto funcType = type->getAs<FunctionType>()) {
    return FunctionType::get(funcType->getInput(), funcType->getResult(),
                             funcType->getExtInfo().withNoEscape());
  }

  return type;
}

Type ClangImporter::Implementation::importFunctionType(
       const clang::FunctionDecl *clangDecl,
       clang::QualType resultType,
       ArrayRef<const clang::ParmVarDecl *> params,
       bool isVariadic, bool isNoReturn,
       bool isFromSystemModule,
       SmallVectorImpl<Pattern*> &bodyPatterns) {

  // Cannot import variadic types.
  if (isVariadic)
    return Type();

  // CF function results can be managed if they are audited or
  // the ownership convention is explicitly declared.
  bool isAuditedResult =
    (clangDecl &&
     (clangDecl->hasAttr<clang::CFAuditedTransferAttr>() ||
      clangDecl->hasAttr<clang::CFReturnsRetainedAttr>() ||
      clangDecl->hasAttr<clang::CFReturnsNotRetainedAttr>()));

  // Check if we know more about the type from our whitelists.
  Optional<api_notes::GlobalFunctionInfo> knownFn;
  if (auto knownFnTmp = getKnownGlobalFunction(clangDecl))
    if (knownFnTmp->NullabilityAudited)
      knownFn = knownFnTmp;

  OptionalTypeKind OptionalityOfReturn;
  if (clangDecl->hasAttr<clang::ReturnsNonNullAttr>()) {
    OptionalityOfReturn = OTK_None;
  } else if (knownFn) {
    OptionalityOfReturn = translateNullability(knownFn->getReturnTypeInfo());
  } else {
    OptionalityOfReturn = OTK_ImplicitlyUnwrappedOptional;
  }

  // Import the result type.
  auto swiftResultTy = importType(resultType,
                                  (isAuditedResult
                                    ? ImportTypeKind::AuditedResult
                                    : ImportTypeKind::Result),
                                  isFromSystemModule,
                                  OptionalityOfReturn);
  if (!swiftResultTy)
    return Type();

  // Import the parameters.
  SmallVector<TupleTypeElt, 4> swiftArgParams;
  SmallVector<TupleTypeElt, 4> swiftBodyParams;
  SmallVector<TuplePatternElt, 4> argPatternElts;
  SmallVector<TuplePatternElt, 4> bodyPatternElts;
  unsigned index = 0;
  llvm::SmallBitVector nonNullArgs = getNonNullArgs(clangDecl, params);
  for (auto param : params) {
    auto paramTy = param->getType();
    if (paramTy->isVoidType()) {
      ++index;
      continue;
    }

    // Check nullability of the parameter.
    OptionalTypeKind OptionalityOfParam = OTK_ImplicitlyUnwrappedOptional;

    // If the parameter type has explicit nullability, it takes precedence.
    if (param->getType()->getNullability(param->getASTContext())) {
      OptionalityOfParam = OTK_None;
    } else if (!nonNullArgs.empty() && nonNullArgs[index]) {
      // Fall back to API notes.
      OptionalityOfParam = OTK_None;
    } else if (param->hasAttr<clang::NonNullAttr>()) {
      OptionalityOfParam = OTK_None;
    } else if (knownFn) {
      // Fall back to API notes.
      OptionalityOfParam = translateNullability(
                             knownFn->getParamTypeInfo(index));
    }

    ImportTypeKind importKind = ImportTypeKind::Parameter;
    if (param->hasAttr<clang::CFReturnsRetainedAttr>())
      importKind = ImportTypeKind::CFRetainedOutParameter;
    else if (param->hasAttr<clang::CFReturnsNotRetainedAttr>())
      importKind = ImportTypeKind::CFUnretainedOutParameter;

    // Import the parameter type into Swift.
    Type swiftParamTy = importType(paramTy, importKind,
                                   isFromSystemModule, OptionalityOfParam);
    if (!swiftParamTy)
      return Type();

    // Map __attribute__((noescape)) to @noescape.
    bool addNoEscapeAttr = false;
    if (param->hasAttr<clang::NoEscapeAttr>()) {
      Type newParamTy = applyNoEscape(swiftParamTy);
      if (newParamTy.getPointer() != swiftParamTy.getPointer()) {
        swiftParamTy = newParamTy;
        addNoEscapeAttr = true;
      }
    }

    // Figure out the name for this parameter.
    Identifier bodyName = importName(param->getDeclName());

    // Note: C functions never have argument names.
    Identifier name;

    // Compute the pattern to put into the body.
    Pattern *bodyPattern;
    // It doesn't actually matter which DeclContext we use, so just use the
    // imported header unit.
    auto bodyVar
      = createDeclWithClangNode<ParamDecl>(param,
                                     /*IsLet*/ true,
                                     SourceLoc(), name,
                                     importSourceLoc(param->getLocation()),
                                     bodyName, swiftParamTy, 
                                     ImportedHeaderUnit);

    if (addNoEscapeAttr) {
      bodyVar->getAttrs().add(
        new (SwiftContext) NoEscapeAttr(/*IsImplicit=*/false));
    }

    bodyPattern = new (SwiftContext) NamedPattern(bodyVar);
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

static bool isObjCMethodResultAudited(const clang::Decl *decl) {
  if (!decl)
    return false;
  return (decl->hasAttr<clang::CFReturnsRetainedAttr>() ||
          decl->hasAttr<clang::CFReturnsNotRetainedAttr>() ||
          decl->hasAttr<clang::ObjCReturnsInnerPointerAttr>());
}

namespace {
  struct ErrorImportInfo {
    ForeignErrorConvention::Kind Kind;
    ForeignErrorConvention::IsOwned_t IsOwned;
    ForeignErrorConvention::IsReplaced_t ReplaceParamWithVoid;
    unsigned ParamIndex;
    CanType ParamType;
    CanType OrigResultType;

    ForeignErrorConvention asForeignErrorConvention() const {
      assert(ParamType && "not fully initialized!");
      using FEC = ForeignErrorConvention;
      switch (Kind) {
      case FEC::ZeroResult:
        return FEC::getZeroResult(ParamIndex, IsOwned, ReplaceParamWithVoid,
                                  ParamType, OrigResultType);
      case FEC::NonZeroResult:
        return FEC::getNonZeroResult(ParamIndex, IsOwned, ReplaceParamWithVoid,
                                     ParamType, OrigResultType);
      case FEC::NilResult:
        return FEC::getNilResult(ParamIndex, IsOwned, ReplaceParamWithVoid,
                                 ParamType);
      case FEC::NonNilError:
        return FEC::getNonNilError(ParamIndex, IsOwned, ReplaceParamWithVoid,
                                   ParamType);
      }
      llvm_unreachable("bad error convention");
    }
  };
}

static bool isBlockParameter(const clang::ParmVarDecl *param) {
  return param->getType()->isBlockPointerType();
}

static bool isErrorOutParameter(const clang::ParmVarDecl *param,
                         ForeignErrorConvention::IsOwned_t &isErrorOwned) {
  clang::QualType type = param->getType();

  // Must be a pointer.
  auto ptrType = type->getAs<clang::PointerType>();
  if (!ptrType) return false;
  type = ptrType->getPointeeType();

  // For NSError**, take ownership from the qualifier.
  if (auto objcPtrType = type->getAs<clang::ObjCObjectPointerType>()) {
    auto iface = objcPtrType->getInterfaceDecl();
    if (iface && iface->getName() == "NSError") {
      switch (type.getObjCLifetime()) {
      case clang::Qualifiers::OCL_None:
        llvm_unreachable("not in ARC?");

      case clang::Qualifiers::OCL_ExplicitNone:
      case clang::Qualifiers::OCL_Autoreleasing:
        isErrorOwned = ForeignErrorConvention::IsNotOwned;
        return true;

      case clang::Qualifiers::OCL_Weak:
        // We just don't know how to handle this.
        return false;

      case clang::Qualifiers::OCL_Strong:
        isErrorOwned = ForeignErrorConvention::IsOwned;
        return false;
      }
      llvm_unreachable("bad error ownership");
    }
  }
  return false;
}

static Optional<ForeignErrorConvention::Kind>
classifyMethodErrorHandling(ClangImporter::Implementation &importer,
                            const clang::ObjCMethodDecl *clangDecl,
                            Type importedResultType) {
  // TODO: opt out any non-standard methods here?

  // For bool results, a zero value is an error.
  if (auto nominalType = importedResultType->getAs<NominalType>()) {
    if (nominalType->getDecl() == importer.SwiftContext.getBoolDecl()) {
      return ForeignErrorConvention::ZeroResult;
    }
  }

  // For optional reference results, a nil value is normally an error.
  if (importedResultType->getAnyOptionalObjectType()) {
    return ForeignErrorConvention::NilResult;
  }

  return None;
}

static const char ErrorSuffix[] = "AndReturnError";

/// Look for a method that will import to have the same name as the
/// given method after importing the Nth parameter as an elided error
/// parameter.
static bool hasErrorMethodNameCollision(const clang::ObjCMethodDecl *method,
                                        unsigned paramIndex,
                                        bool stripErrorSuffix) {
  // Copy the existing selector pieces into an array.
  auto selector = method->getSelector();
  unsigned numArgs = selector.getNumArgs();
  assert(numArgs > 0);

  SmallVector<clang::IdentifierInfo *, 4> chunks;
  for (unsigned i = 0, e = selector.getNumArgs(); i != e; ++i) {
    chunks.push_back(selector.getIdentifierInfoForSlot(i));
  }

  auto &ctx = method->getASTContext();
  if (paramIndex == 0 && stripErrorSuffix) {
    assert(stripErrorSuffix);
    StringRef name = chunks[0]->getName();
    assert(name.endswith(ErrorSuffix));
    name = name.drop_back(sizeof(ErrorSuffix) - 1);
    chunks[0] = &ctx.Idents.get(name);
  } else if (paramIndex != 0) {
    chunks.erase(chunks.begin() + paramIndex);
  }

  auto newSelector = ctx.Selectors.getSelector(numArgs - 1, chunks.data());
  const clang::ObjCMethodDecl *conflict;
  if (auto iface = method->getClassInterface()) {
    conflict = iface->lookupMethod(newSelector, method->isInstanceMethod());
  } else {
    auto protocol = cast<clang::ObjCProtocolDecl>(method->getDeclContext());
    conflict = protocol->getMethod(newSelector, method->isInstanceMethod());
  }

  return (conflict != nullptr);
}


static Optional<ErrorImportInfo>
considerErrorImport(ClangImporter::Implementation &importer,
                    const clang::ObjCMethodDecl *clangDecl,
                    DeclName &methodName,
                    ArrayRef<const clang::ParmVarDecl *> params,
                    Type &importedResultType,
                    SpecialMethodKind methodKind) {
  // Respect the compiler option.
  if (!importer.ErrorHandling) return None;

  // If the declaration name isn't parallel to the actual parameter
  // list (e.g. if the method has C-style parameter declarations),
  // don't try to apply error conventions.
  auto paramNames = methodName.getArgumentNames();
  bool expectsToRemoveError = paramNames.size() + 1 == params.size();
  if (!expectsToRemoveError && paramNames.size() != params.size())
    return None;

  for (unsigned index = params.size(); index-- != 0; ) {
    // Allow an arbitrary number of trailing blocks.
    if (isBlockParameter(params[index]))
      continue;

    // Otherwise, require the last parameter to be an out-parameter.
    auto isErrorOwned = ForeignErrorConvention::IsNotOwned;
    if (!isErrorOutParameter(params[index], isErrorOwned))
      break;

    auto errorKind =
      classifyMethodErrorHandling(importer, clangDecl, importedResultType);
    if (!errorKind) return None;

    // Consider adjusting the imported declaration name to remove the
    // parameter.
    bool adjustName = !expectsToRemoveError;

    // Never do this if it's the first parameter of a constructor.
    if (methodKind == SpecialMethodKind::Constructor && index == 0) {
      adjustName = false;
    }

    // If the error parameter is the first parameter, try removing the
    // standard error suffix from the base name.
    bool stripErrorSuffix = false;
    Identifier newBaseName = methodName.getBaseName();
    if (adjustName && index == 0 && paramNames[0].empty()) {
      StringRef baseNameStr = newBaseName.str();
      if (baseNameStr.endswith(ErrorSuffix)) {
        baseNameStr = baseNameStr.drop_back(sizeof(ErrorSuffix) - 1);
        if (baseNameStr.empty() || importer.isSwiftReservedName(baseNameStr)) {
          adjustName = false;
        } else {
          newBaseName = importer.SwiftContext.getIdentifier(baseNameStr);
          stripErrorSuffix = true;
        }
      }
    }

    // Also suppress name changes if there's a collision.
    // TODO: this logic doesn't really work with init methods
    // TODO: this privileges the old API over the new one
    if (adjustName &&
        hasErrorMethodNameCollision(clangDecl, index, stripErrorSuffix)) {
      // If there was a conflict on the first argument, and this was
      // the first argument and we're not stripping error suffixes, just
      // give up completely on error import.
      if (index == 0 && !stripErrorSuffix) {
        return None;

      // If there was a conflict stripping an error suffix, adjust the
      // name but don't change the base name.  This avoids creating a
      // spurious _: () argument.
      } else if (index == 0 && stripErrorSuffix) {
        stripErrorSuffix = false;
        newBaseName = methodName.getBaseName();

      // Otherwise, give up on adjusting the name.
      } else {
        adjustName = false;
      }
    }

    auto replaceWithVoid = ForeignErrorConvention::IsNotReplaced;
    if (!adjustName && !expectsToRemoveError)
      replaceWithVoid = ForeignErrorConvention::IsReplaced;
    ErrorImportInfo errorInfo = {
      *errorKind, isErrorOwned, replaceWithVoid, index, CanType(),
      importedResultType->getCanonicalType()
    };
    
    // Adjust the return type.
    switch (*errorKind) {
    case ForeignErrorConvention::ZeroResult:
    case ForeignErrorConvention::NonZeroResult:
      importedResultType = TupleType::getEmpty(importer.SwiftContext);
      break;

    case ForeignErrorConvention::NilResult:
      importedResultType = importedResultType->getAnyOptionalObjectType();
      assert(importedResultType &&
             "result type of NilResult convention was not imported as optional");
      break;

    case ForeignErrorConvention::NonNilError:
      break;
    }

    if (!adjustName)
      return errorInfo;

    // Build the new declaration name.
    SmallVector<Identifier, 8> newParamNames;
    newParamNames.append(paramNames.begin(),
                         paramNames.begin() + index);
    newParamNames.append(paramNames.begin() + index + 1,
                         paramNames.end());
    methodName = DeclName(importer.SwiftContext, newBaseName, newParamNames);

    return errorInfo;
  }

  // Didn't find an error parameter.
  return None;
}

Type ClangImporter::Implementation::importMethodType(
       const clang::ObjCMethodDecl *clangDecl,
       clang::QualType resultType,
       ArrayRef<const clang::ParmVarDecl *> params,
       bool isVariadic, bool isNoReturn,
       bool isFromSystemModule,
       SmallVectorImpl<Pattern*> &bodyPatterns,
       DeclName &methodName,
       Optional<ForeignErrorConvention> &foreignErrorInfo,
       SpecialMethodKind kind) {

  // Cannot import variadic types unless specially handled before calling this
  // function.
  if (isVariadic || clangDecl->sel_param_end() != clangDecl->param_end())
    return Type();

  // Clang doesn't provide pragmas for auditing the CF behavior of
  // ObjC methods, but it does have attributes for declaring
  // return-type management:
  //   - cf_returns_retained and cf_returns_not_retained are obvious
  //   - objc_returns_inner_pointer is sometimes used on methods
  //     returning CF types as a workaround for ARC not managing CF
  //     objects
  ImportTypeKind resultKind;
  if (kind == SpecialMethodKind::PropertyAccessor)
    resultKind = ImportTypeKind::PropertyAccessor;
  else if (isObjCMethodResultAudited(clangDecl))
    resultKind = ImportTypeKind::AuditedResult;
  else
    resultKind = ImportTypeKind::Result;

  // Check if we know more about the type from our whitelists.
  Optional<api_notes::ObjCMethodInfo> knownMethod;
  if (auto knownMethodTmp = getKnownObjCMethod(clangDecl)) {
    if (knownMethodTmp->NullabilityAudited)
      knownMethod = knownMethodTmp;
  }

  // Determine if the method is a property getter/setter.
  const clang::ObjCPropertyDecl *property = nullptr;
  bool isPropertyGetter = false;
  bool isPropertySetter = false;
  if (clangDecl->isPropertyAccessor()) {
    property = clangDecl->findPropertyDecl();
    if (property) {
      if (property->getGetterMethodDecl() == clangDecl) {
        isPropertyGetter = true;
      } else if (property->getSetterMethodDecl() == clangDecl) {
        isPropertySetter = true;
      }
    }
  }

  // Import the result type.
  Type swiftResultTy;
  if (isPropertyGetter) {
    swiftResultTy = importPropertyType(property, isFromSystemModule);
  } else {
    OptionalTypeKind OptionalityOfReturn;
    if (clangDecl->hasAttr<clang::ReturnsNonNullAttr>()) {
      OptionalityOfReturn = OTK_None;
    } else if (knownMethod) {
      OptionalityOfReturn = translateNullability(
                              knownMethod->getReturnTypeInfo());
    } else {
      OptionalityOfReturn = OTK_ImplicitlyUnwrappedOptional;
    }

    swiftResultTy = importType(resultType, resultKind,
                               isFromSystemModule, OptionalityOfReturn);
  }
  if (!swiftResultTy)
    return Type();

  auto errorInfo = considerErrorImport(*this, clangDecl, methodName, params,
                                       swiftResultTy, kind);

  // Import the parameters.
  SmallVector<TupleTypeElt, 4> swiftArgParams;
  SmallVector<TupleTypeElt, 4> swiftBodyParams;
  SmallVector<TuplePatternElt, 4> bodyPatternElts;

  auto addEmptyTupleParameter = [&](Identifier argName) {
    // It doesn't actually matter which DeclContext we use, so just
    // use the imported header unit.
    auto type = TupleType::getEmpty(SwiftContext);
    auto var = new (SwiftContext) ParamDecl(/*IsLet*/ true,
                                            SourceLoc(), argName,
                                            SourceLoc(), argName, type,
                                            ImportedHeaderUnit);
    Pattern *pattern = new (SwiftContext) NamedPattern(var);
    pattern->setType(type);
    pattern = new (SwiftContext) TypedPattern(pattern,
                                              TypeLoc::withoutLoc(type));
    pattern->setType(type);

    bodyPatternElts.push_back(TuplePatternElt(pattern));
    swiftArgParams.push_back(TupleTypeElt(type, argName));
    swiftBodyParams.push_back(TupleTypeElt(type, argName));
  };

  auto argNames = methodName.getArgumentNames();
  llvm::SmallBitVector nonNullArgs = getNonNullArgs(clangDecl, params);
  unsigned nameIndex = 0;
  for (size_t paramIndex = 0; paramIndex != params.size(); paramIndex++) {
    auto param = params[paramIndex];
    auto paramTy = param->getType();
    if (paramTy->isVoidType()) {
      assert(!errorInfo || paramIndex != errorInfo->ParamIndex);
      ++nameIndex;
      continue;
    }

    if (kind == SpecialMethodKind::NSDictionarySubscriptGetter)
      nonNullArgs.empty();

    // Import the parameter type into Swift.

    // Check nullability of the parameter.
    OptionalTypeKind optionalityOfParam = OTK_ImplicitlyUnwrappedOptional;

    // If the parameter type has explicit nullability, it takes precedence.
    if (param->getType()->getNullability(param->getASTContext())) {
      optionalityOfParam = OTK_None;
    } else if (!nonNullArgs.empty() && nonNullArgs[paramIndex]) {
      // Fall back to API notes.
      optionalityOfParam = OTK_None;
    } else if (param->hasAttr<clang::NonNullAttr>()) {
      optionalityOfParam = OTK_None;
    } else if (knownMethod) {
      // Fall back to API notes.
      optionalityOfParam =
        translateNullability(knownMethod->getParamTypeInfo(paramIndex));
    }

    Type swiftParamTy;
    if (paramIndex == 0 && isPropertySetter) {
      swiftParamTy = importPropertyType(property, isFromSystemModule);
    } else if (kind == SpecialMethodKind::NSDictionarySubscriptGetter &&
               paramTy->isObjCIdType()) {
      swiftParamTy = getOptionalType(getNSCopyingType(),
                                     ImportTypeKind::Parameter,
                                     optionalityOfParam);
    } else if (kind == SpecialMethodKind::PropertyAccessor) {
      swiftParamTy = importType(paramTy,
                                ImportTypeKind::PropertyAccessor,
                                isFromSystemModule,
                                optionalityOfParam);
    } else {
      ImportTypeKind importKind = ImportTypeKind::Parameter;
      if (param->hasAttr<clang::CFReturnsRetainedAttr>())
        importKind = ImportTypeKind::CFRetainedOutParameter;
      else if (param->hasAttr<clang::CFReturnsNotRetainedAttr>())
        importKind = ImportTypeKind::CFUnretainedOutParameter;
      
      swiftParamTy = importType(paramTy, importKind, isFromSystemModule,
                                optionalityOfParam);
    }
    if (!swiftParamTy)
      return Type();

    // If this is the error parameter, remember it, but don't build it
    // into the parameter type.
    if (errorInfo && paramIndex == errorInfo->ParamIndex) {
      errorInfo->ParamType = swiftParamTy->getCanonicalType();

      // ...unless we're supposed to replace it with ().
      if (errorInfo->ReplaceParamWithVoid) {
        addEmptyTupleParameter(argNames[nameIndex]);
        ++nameIndex;
      }
      continue;
    }

    // Map __attribute__((noescape)) to @noescape.
    bool addNoEscapeAttr = false;
    if (param->hasAttr<clang::NoEscapeAttr>()) {
      Type newParamTy = applyNoEscape(swiftParamTy);
      if (newParamTy.getPointer() != swiftParamTy.getPointer()) {
        swiftParamTy = newParamTy;
        addNoEscapeAttr = true;
      }
    }

    // Figure out the name for this parameter.
    Identifier bodyName = importName(param->getDeclName());

    // Figure out the name for this argument, which comes from the method name.
    Identifier name;
    if (nameIndex < argNames.size()) {
      name = argNames[nameIndex];
    }
    ++nameIndex;

    // Compute the pattern to put into the body.
    Pattern *bodyPattern;
    // It doesn't actually matter which DeclContext we use, so just use the
    // imported header unit.
    auto bodyVar
      = createDeclWithClangNode<ParamDecl>(param,
                                     /*IsLet*/ true, SourceLoc(), name,
                                     importSourceLoc(param->getLocation()),
                                     bodyName, swiftParamTy, 
                                     ImportedHeaderUnit);

    if (addNoEscapeAttr) {
      bodyVar->getAttrs().add(
        new (SwiftContext) NoEscapeAttr(/*IsImplicit=*/false));
    }

    bodyPattern = new (SwiftContext) NamedPattern(bodyVar);
    bodyPattern->setType(swiftParamTy);
    bodyPattern
      = new (SwiftContext) TypedPattern(bodyPattern,
                                        TypeLoc::withoutLoc(swiftParamTy));
    bodyPattern->setType(swiftParamTy);
    bodyPatternElts.push_back(TuplePatternElt(bodyPattern));

    // Add the tuple elements for the function types.
    swiftArgParams.push_back(TupleTypeElt(swiftParamTy, name));
    swiftBodyParams.push_back(TupleTypeElt(swiftParamTy, bodyName));
  }

  // If we have a constructor with no parameters and a name with an
  // argument name, synthesize a Void parameter with that name.
  if (kind == SpecialMethodKind::Constructor && params.empty() && 
      argNames.size() == 1) {
    addEmptyTupleParameter(argNames[0]);
  }

  // Form the parameter tuple.
  auto bodyParamsTy = TupleType::get(swiftBodyParams, SwiftContext);
  
  // Form the body patterns.
  bodyPatterns.push_back(TuplePattern::create(SwiftContext, SourceLoc(),
                                              bodyPatternElts, SourceLoc()));
  bodyPatterns.back()->setType(bodyParamsTy);
  
  FunctionType::ExtInfo extInfo;
  extInfo = extInfo.withIsNoReturn(isNoReturn);

  if (errorInfo) {
    foreignErrorInfo = errorInfo->asForeignErrorConvention();

    // Mark that the function type throws.
    extInfo = extInfo.withThrows(true);
  }
  
  // Form the function type.
  auto argTy = TupleType::get(swiftArgParams, SwiftContext);
  return FunctionType::get(argTy, swiftResultTy, extInfo);
}

Module *ClangImporter::Implementation::getStdlibModule() {
  return SwiftContext.getStdlibModule(true);
}

Module *ClangImporter::Implementation::getNamedModule(StringRef name) {
  return SwiftContext.getLoadedModule(SwiftContext.getIdentifier(name));
}

static Module *tryLoadModule(ASTContext &C,
                             Identifier name,
                             bool importForwardDeclarations,
                             Optional<Module *> &cache) {
  if (!cache.hasValue()) {
    // If we're synthesizing forward declarations, we don't want to pull in
    // the module too eagerly.
    if (importForwardDeclarations)
      cache = C.getLoadedModule(name);
    else
      cache = C.getModule({ {name, SourceLoc()} });
  }

  return cache.getValue();
}

Module *ClangImporter::Implementation::tryLoadFoundationModule() {
  return tryLoadModule(SwiftContext, SwiftContext.Id_Foundation,
                       ImportForwardDeclarations, checkedFoundationModule);
}

Module *ClangImporter::Implementation::tryLoadSIMDModule() {
  return tryLoadModule(SwiftContext, SwiftContext.Id_simd,
                       ImportForwardDeclarations, checkedSIMDModule);
}

Type ClangImporter::Implementation::getNamedSwiftType(Module *module,
                                                      StringRef name) {
  if (!module)
    return Type();

  // Look for the type.
  SmallVector<ValueDecl *, 2> results;
  module->lookupValue({ }, SwiftContext.getIdentifier(name),
                      NLKind::UnqualifiedLookup, results);
  if (results.size() != 1)
    return Type();

  auto type = dyn_cast<TypeDecl>(results.front());
  if (!type)
    return Type();

  assert(!type->hasClangNode() && "picked up the original type?");

  if (auto *typeResolver = getTypeResolver())
    typeResolver->resolveDeclSignature(type);
  return type->getDeclaredType();
}

Type
ClangImporter::Implementation::
getNamedSwiftTypeSpecialization(Module *module, StringRef name,
                                ArrayRef<Type> args) {
  if (!module)
    return Type();

  // Look for the type.
  SmallVector<ValueDecl *, 2> results;
  module->lookupValue({ }, SwiftContext.getIdentifier(name),
                      NLKind::UnqualifiedLookup, results);
  if (results.size() == 1) {
    if (auto nominalDecl = dyn_cast<NominalTypeDecl>(results.front())) {
      if (auto *typeResolver = getTypeResolver())
        typeResolver->resolveDeclSignature(nominalDecl);
      if (auto params = nominalDecl->getGenericParams()) {
        if (params->size() == args.size()) {
          // When we form the bound generic type, make sure we get the
          // substitutions.
          auto *BGT = BoundGenericType::get(nominalDecl, Type(), args);
          return BGT;
        }
      }
    }
  }

  return Type();
}

Decl *ClangImporter::Implementation::importDeclByName(StringRef name) {
  auto &sema = Instance->getSema();

  // Map the name. If we can't represent the Swift name in Clang, bail out now.
  auto clangName = &getClangASTContext().Idents.get(name);

  // Perform name lookup into the global scope.
  // FIXME: Map source locations over.
  clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                   clang::Sema::LookupOrdinaryName);
  if (!sema.LookupName(lookupResult, /*Scope=*/0)) {
    return nullptr;
  }

  for (auto decl : lookupResult) {
    if (auto swiftDecl = importDecl(decl->getUnderlyingDecl())) {
      return swiftDecl;
    }
  }

  return nullptr;
}

Type ClangImporter::Implementation::getNSObjectType() {
  if (NSObjectTy)
    return NSObjectTy;

  if (auto decl = dyn_cast_or_null<ClassDecl>(importDeclByName("NSObject"))) {
    NSObjectTy = decl->getDeclaredType();
    return NSObjectTy;
  }

  return Type();
}

bool ClangImporter::Implementation::matchesNSObjectBound(Type type) {
  Type NSObjectType = getNSObjectType();
  if (!NSObjectType)
    return false;

  // Class type or existential that inherits from NSObject.
  if (NSObjectType->isSuperclassOf(type, getTypeResolver()))
    return true;

  // Struct or enum type must have been bridged.
  if (type->getStructOrBoundGenericStruct() ||
      type->getEnumOrBoundGenericEnum())
    return true;

  return false;
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

Type ClangImporter::Implementation::getCFStringRefType() {
  if (auto decl = dyn_cast_or_null<TypeDecl>(importDeclByName("CFStringRef")))
    return decl->getDeclaredType();
  return Type();
}

