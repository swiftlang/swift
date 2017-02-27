//===--- ImportType.cpp - Import Clang Types ------------------------------===//
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
//
// This file implements support for importing Clang types as Swift types.
//
//===----------------------------------------------------------------------===//

#include "CFTypeInfo.h"
#include "ImporterImpl.h"
#include "ClangDiagnosticConsumer.h"
#include "swift/Strings.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/Token.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/TypeVisitor.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Compiler.h"

using namespace swift;
using namespace importer;

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

      /// The source type is 'Boolean'.
      Boolean,

      /// The source type is an Objective-C class type bridged to a Swift
      /// type.
      ObjCBridged,

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

      /// The source type is any other pointer type.
      OtherPointer,

      /// The source type created a new Swift type, using swift_newtype
      SwiftNewtype,

      /// The source type created a new Swift type, using swift_newtype, of an
      /// original underlying CFPointer. This distinction is necessary to
      /// trigger audit-checking.
      SwiftNewtypeFromCFPointer,
    };

    ImportHintKind Kind;

    /// The type to which the imported type is bridged.
    Type BridgedType;

    /// Allow conversion from an import hint to an import hint kind,
    /// which is useful for switches and comparisons.
    operator ImportHintKind() const { return Kind; }

    ImportHint(ImportHintKind kind) : Kind(kind) {
      assert(kind != ObjCBridged &&
             "Bridged entry point requires a bridged type");
    }

    ImportHint(ImportHintKind kind, Type bridgedType)
        : Kind(kind), BridgedType(bridgedType) {
      assert(kind == ImportHint::ObjCBridged && "Wrong kind for bridged type");
    }
  };

  bool canImportAsOptional(ImportHint hint) {
    // See also ClangImporter.cpp's canImportAsOptional.
    switch (hint) {
    case ImportHint::None:
    case ImportHint::BOOL:
    case ImportHint::Boolean:
    case ImportHint::NSUInteger:
    case ImportHint::Reference:
    case ImportHint::Void:
      return false;

    case ImportHint::Block:
    case ImportHint::CFPointer:
    case ImportHint::ObjCBridged:
    case ImportHint::ObjCPointer:
    case ImportHint::CFunctionPointer:
    case ImportHint::OtherPointer:
    case ImportHint::SwiftNewtype:
    case ImportHint::SwiftNewtypeFromCFPointer:
      return true;
    }

    llvm_unreachable("Invalid ImportHint.");
  }

  struct ImportResult {
    Type AbstractType;
    ImportHint Hint;

    /*implicit*/ ImportResult(Type type = Type(),
                              ImportHint hint = ImportHint::None)
      : AbstractType(type), Hint(hint) {}

    /*implicit*/ ImportResult(TypeBase *type,
                              ImportHint hint = ImportHint::None)
      : AbstractType(type), Hint(hint) {}

    explicit operator bool() const { return (bool) AbstractType; }
  };

  /// Wrap a type in the Optional type appropriate to the import kind.
  static Type
  getOptionalType(Type payloadType,
                  ImportTypeKind kind,
                  OptionalTypeKind OptKind = OTK_ImplicitlyUnwrappedOptional) {
    switch (OptKind) {
    case OTK_None:
      return payloadType;
    case OTK_Optional:
      return OptionalType::get(payloadType);
    case OTK_ImplicitlyUnwrappedOptional:
      // Import pointee types as true Optional.
      if (kind == ImportTypeKind::Pointee)
        return OptionalType::get(payloadType);
      return ImplicitlyUnwrappedOptionalType::get(payloadType);
    }

    llvm_unreachable("Invalid OptionalTypeKind.");
  }

  class SwiftTypeConverter :
    public clang::TypeVisitor<SwiftTypeConverter, ImportResult>
  {
    ClangImporter::Implementation &Impl;
    bool AllowNSUIntegerAsInt;
    bool CanFullyBridgeTypes;

  public:
    SwiftTypeConverter(ClangImporter::Implementation &impl,
                       bool allowNSUIntegerAsInt,
                       bool canFullyBridgeTypes)
      : Impl(impl), AllowNSUIntegerAsInt(allowNSUIntegerAsInt),
        CanFullyBridgeTypes(canFullyBridgeTypes) {}

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
      case clang::BuiltinType::Float128:
      case clang::BuiltinType::NullPtr:
        return Type();

      // Objective-C types that aren't mapped directly; rather, pointers to
      // these types will be mapped.
      case clang::BuiltinType::ObjCClass:
      case clang::BuiltinType::ObjCId:
      case clang::BuiltinType::ObjCSel:
        return Type();

      // OpenCL types that don't have Swift equivalents.
      case clang::BuiltinType::OCLImage1dRO:
      case clang::BuiltinType::OCLImage1dRW:
      case clang::BuiltinType::OCLImage1dWO:
      case clang::BuiltinType::OCLImage1dArrayRO:
      case clang::BuiltinType::OCLImage1dArrayRW:
      case clang::BuiltinType::OCLImage1dArrayWO:
      case clang::BuiltinType::OCLImage1dBufferRO:
      case clang::BuiltinType::OCLImage1dBufferRW:
      case clang::BuiltinType::OCLImage1dBufferWO:
      case clang::BuiltinType::OCLImage2dRO:
      case clang::BuiltinType::OCLImage2dRW:
      case clang::BuiltinType::OCLImage2dWO:
      case clang::BuiltinType::OCLImage2dArrayRO:
      case clang::BuiltinType::OCLImage2dArrayRW:
      case clang::BuiltinType::OCLImage2dArrayWO:
      case clang::BuiltinType::OCLImage2dDepthRO:
      case clang::BuiltinType::OCLImage2dDepthRW:
      case clang::BuiltinType::OCLImage2dDepthWO:
      case clang::BuiltinType::OCLImage2dArrayDepthRO:
      case clang::BuiltinType::OCLImage2dArrayDepthRW:
      case clang::BuiltinType::OCLImage2dArrayDepthWO:
      case clang::BuiltinType::OCLImage2dMSAARO:
      case clang::BuiltinType::OCLImage2dMSAARW:
      case clang::BuiltinType::OCLImage2dMSAAWO:
      case clang::BuiltinType::OCLImage2dArrayMSAARO:
      case clang::BuiltinType::OCLImage2dArrayMSAARW:
      case clang::BuiltinType::OCLImage2dArrayMSAAWO:
      case clang::BuiltinType::OCLImage2dMSAADepthRO:
      case clang::BuiltinType::OCLImage2dMSAADepthRW:
      case clang::BuiltinType::OCLImage2dMSAADepthWO:
      case clang::BuiltinType::OCLImage2dArrayMSAADepthRO:
      case clang::BuiltinType::OCLImage2dArrayMSAADepthRW:
      case clang::BuiltinType::OCLImage2dArrayMSAADepthWO:
      case clang::BuiltinType::OCLImage3dRO:
      case clang::BuiltinType::OCLImage3dRW:
      case clang::BuiltinType::OCLImage3dWO:
      case clang::BuiltinType::OCLSampler:
      case clang::BuiltinType::OCLEvent:
      case clang::BuiltinType::OCLClkEvent:
      case clang::BuiltinType::OCLQueue:
      case clang::BuiltinType::OCLReserveID:
        return Type();

      // OpenMP types that don't have Swift equivalents.
      case clang::BuiltinType::OMPArraySection:
        return Type();
      }

      llvm_unreachable("Invalid BuiltinType.");
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
      auto quals = pointeeQualType.getQualifiers();

      // Special case for NSZone*, which has its own Swift wrapper.
      if (const clang::RecordType *pointee =
            pointeeQualType->getAsStructureType()) {
        if (pointee && !pointee->getDecl()->isCompleteDefinition() &&
            pointee->getDecl()->getName() == "_NSZone") {
          Identifier Id_ObjectiveC = Impl.SwiftContext.Id_ObjectiveC;
          ModuleDecl *objCModule = Impl.SwiftContext.getLoadedModule(Id_ObjectiveC);
          Type wrapperTy = Impl.getNamedSwiftType(
                             objCModule,
                             Impl.SwiftContext.getSwiftName(
                               KnownFoundationEntity::NSZone));
          if (wrapperTy)
            return {wrapperTy, ImportHint::OtherPointer};
        }
      }

      // Import 'void*' as 'UnsafeMutableRawPointer' and 'const void*' as
      // 'UnsafeRawPointer'. This is Swift's version of an untyped pointer. Note
      // that 'Unsafe[Mutable]Pointer<T>' implicitly converts to
      // 'Unsafe[Mutable]RawPointer' for interoperability.
      if (pointeeQualType->isVoidType()) {
        return {
          (quals.hasConst() ? Impl.SwiftContext.getUnsafeRawPointerDecl()
           : Impl.SwiftContext.getUnsafeMutableRawPointerDecl())
            ->getDeclaredType(),
            ImportHint::OtherPointer};
      }

      // All other C pointers to concrete types map to
      // UnsafeMutablePointer<T> or OpaquePointer (FIXME:, except in
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
                                      AllowNSUIntegerAsInt,
                                      /*isFullyBridgeable*/false);

      // If the pointed-to type is unrepresentable in Swift, import as
      // OpaquePointer.
      if (!pointeeType)
        return {Impl.SwiftContext.getOpaquePointerDecl()->getDeclaredType(),
                ImportHint::OtherPointer};
      
      if (pointeeQualType->isFunctionType()) {
        auto funcTy = pointeeType->castTo<FunctionType>();
        return {
          FunctionType::get(funcTy->getInput(), funcTy->getResult(),
            funcTy->getExtInfo().withRepresentation(
                          AnyFunctionType::Representation::CFunctionPointer)),
          ImportHint::CFunctionPointer
        };
      }

      if (quals.hasConst()) {
        return {Impl.getNamedSwiftTypeSpecialization(Impl.getStdlibModule(),
                                                     "UnsafePointer",
                                                     pointeeType),
                ImportHint::OtherPointer};
      }

      // Mutable pointers with __autoreleasing or __unsafe_unretained
      // ownership map to AutoreleasingUnsafeMutablePointer<T>.
      if (quals.getObjCLifetime() == clang::Qualifiers::OCL_Autoreleasing ||
          quals.getObjCLifetime() == clang::Qualifiers::OCL_ExplicitNone) {
        return {
          Impl.getNamedSwiftTypeSpecialization(
            Impl.getStdlibModule(), "AutoreleasingUnsafeMutablePointer",
            pointeeType),
          ImportHint::OtherPointer};
      }

      // All other mutable pointers map to UnsafeMutablePointer.
      return {Impl.getNamedSwiftTypeSpecialization(Impl.getStdlibModule(),
                                                   "UnsafeMutablePointer",
                                                   pointeeType),
              ImportHint::OtherPointer};
    }

    ImportResult VisitBlockPointerType(const clang::BlockPointerType *type) {
      // Block pointer types are mapped to function types.
      Type pointeeType = Impl.importType(type->getPointeeType(),
                                         ImportTypeKind::Abstract,
                                         AllowNSUIntegerAsInt,
                                         CanFullyBridgeTypes);
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
                                         AllowNSUIntegerAsInt,
                                         /*isFullyBridgeable*/false);
      if (!elementType)
        return Type();
      
      TupleTypeElt elt(elementType);
      SmallVector<TupleTypeElt, 8> elts;
      for (size_t i = 0, size = type->getSize().getZExtValue(); i < size; ++i)
        elts.push_back(elt);
      
      return TupleType::get(elts, elementType->getASTContext());
    }

    ImportResult VisitVectorType(const clang::VectorType *type) {
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
#define MAP_SIMD_TYPE(TYPE_NAME, __, BUILTIN_KIND)   \
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
                                      AllowNSUIntegerAsInt,
                                      CanFullyBridgeTypes,
                                      OTK_Optional);
      if (!resultTy)
        return Type();

      SmallVector<TupleTypeElt, 4> params;
      for (auto param = type->param_type_begin(),
             paramEnd = type->param_type_end();
           param != paramEnd; ++param) {
        auto swiftParamTy = Impl.importType(*param, ImportTypeKind::Parameter,
                                            AllowNSUIntegerAsInt,
                                            CanFullyBridgeTypes,
                                            OTK_Optional);
        if (!swiftParamTy)
          return Type();

        // FIXME: If we were walking TypeLocs, we could actually get parameter
        // names. The probably doesn't matter outside of a FuncDecl, which
        // we'll have to special-case, but it's an interesting bit of data loss.
        // We also lose `noescape`. <https://bugs.swift.org/browse/SR-2529>
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
                                      AllowNSUIntegerAsInt,
                                      CanFullyBridgeTypes,
                                      OTK_Optional);
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

    /// Imports the type defined by \p objcTypeParamDecl.
    ///
    /// If the type parameter is not imported for some reason, returns \c None.
    /// This is different from a failure; it means the caller should try
    /// importing the underlying type instead.
    Optional<ImportResult>
    importObjCTypeParamDecl(const clang::ObjCTypeParamDecl *objcTypeParamDecl) {
      // Pull the corresponding generic type parameter from the imported class.
      const auto *typeParamContext = objcTypeParamDecl->getDeclContext();
      GenericSignature *genericSig = nullptr;
      GenericEnvironment *genericEnv = nullptr;
      if (auto *category =
            dyn_cast<clang::ObjCCategoryDecl>(typeParamContext)) {
        auto ext = cast_or_null<ExtensionDecl>(
            Impl.importDecl(category, Impl.CurrentVersion));
        if (!ext)
          return ImportResult();
        genericSig = ext->getGenericSignature();
        genericEnv = ext->getGenericEnvironment();
      } else if (auto *interface =
          dyn_cast<clang::ObjCInterfaceDecl>(typeParamContext)) {
        auto cls = cast_or_null<ClassDecl>(
            Impl.importDecl(interface, Impl.CurrentVersion));
        if (!cls)
          return ImportResult();
        genericSig = cls->getGenericSignature();
        genericEnv = cls->getGenericEnvironment();
      }
      unsigned index = objcTypeParamDecl->getIndex();
      // Pull the generic param decl out of the imported class.
      if (!genericSig) {
        // The ObjC type param didn't get imported, possibly because it was
        // suppressed. Treat it as a typedef.
        return None;
      }
      if (index > genericSig->getGenericParams().size()) {
        return ImportResult();
      }
      auto *paramDecl = genericSig->getGenericParams()[index];
      return ImportResult(genericEnv->mapTypeIntoContext(paramDecl),
                          ImportHint::ObjCPointer);
    }

    ImportResult VisitObjCTypeParamType(const clang::ObjCTypeParamType *type) {
      // FIXME: This drops any added protocols on the floor, which is the whole
      // point of ObjCTypeParamType. When not in Swift 3 compatibility mode, we
      // should adjust the resulting type accordingly.
      if (auto result = importObjCTypeParamDecl(type->getDecl()))
        return result.getValue();
      // Fall back to importing the desugared type, which uses the parameter's
      // bound. This isn't perfect but it's better than dropping the type.
      return Visit(type->getLocallyUnqualifiedSingleStepDesugaredType());
    }

    ImportResult VisitTypedefType(const clang::TypedefType *type) {
      // If the underlying declaration is an Objective-C type parameter,
      // pull the corresponding generic type parameter from the imported class.
      if (auto *objcTypeParamDecl =
            dyn_cast<clang::ObjCTypeParamDecl>(type->getDecl())) {
        if (auto result = importObjCTypeParamDecl(objcTypeParamDecl))
          return result.getValue();
        return Visit(type->getLocallyUnqualifiedSingleStepDesugaredType());
      }

      // Import the underlying declaration.
      auto decl = dyn_cast_or_null<TypeDecl>(
          Impl.importDecl(type->getDecl(), Impl.CurrentVersion));

      // If that fails, fall back on importing the underlying type.
      if (!decl) return Visit(type->desugar());

      Type mappedType = getAdjustedTypeDeclReferenceType(decl);
      ImportHint hint = ImportHint::None;

      if (getSwiftNewtypeAttr(type->getDecl(), Impl.CurrentVersion)) {
        if (isCFTypeDecl(type->getDecl()))
          hint = ImportHint::SwiftNewtypeFromCFPointer;
        else
          hint = ImportHint::SwiftNewtype;

        // If the underlying type was bridged, the wrapper type is
        // only useful in bridged cases.
        auto underlying = Visit(type->getDecl()->getUnderlyingType());
        if (underlying.Hint == ImportHint::ObjCBridged) {
          return { underlying.AbstractType,
                   ImportHint(ImportHint::ObjCBridged, mappedType) };
        }

      // For certain special typedefs, we don't want to use the imported type.
      } else if (auto specialKind = Impl.getSpecialTypedefKind(type->getDecl())) {
        switch (specialKind.getValue()) {
        case MappedTypeNameKind::DoNothing:
        case MappedTypeNameKind::DefineAndUse:
          break;
        case MappedTypeNameKind::DefineOnly:
          if (auto typealias = dyn_cast<TypeAliasDecl>(decl))
            mappedType = typealias->getDeclaredInterfaceType()
              ->getDesugaredType();
          break;
        }

        if (type->getDecl()->getName() == "BOOL") {
          hint = ImportHint::BOOL;
        } else if (type->getDecl()->getName() == "Boolean") {
          // FIXME: Darwin only?
          hint = ImportHint::Boolean;
        } else if (type->getDecl()->getName() == "NSUInteger") {
          hint = ImportHint::NSUInteger;
        } else if (isImportedCFPointer(type->desugar(), mappedType)) {
          hint = ImportHint::CFPointer;
        } else if (mappedType->isAnyExistentialType()) { // id, Class
          hint = ImportHint::ObjCPointer;
        } else if (type->isPointerType() || type->isBlockPointerType()) {
          hint = ImportHint::OtherPointer;
        }
        // Any other interesting mapped types should be hinted here.

      // Otherwise, recurse on the underlying type in order to compute
      // the hint correctly.
      } else {
        SwiftTypeConverter innerConverter(Impl, AllowNSUIntegerAsInt,
                                          /*can fully bridge*/false);
        auto underlyingResult = innerConverter.Visit(type->desugar());
#ifndef NDEBUG
        switch (underlyingResult.Hint) {
        case ImportHint::Block:
          // Blocks change in all sorts of ways, due to bridging.
          break;
        case ImportHint::NSUInteger:
          // NSUInteger might be imported as Int rather than UInt depending
          // on where the import lives.
          if (underlyingResult.AbstractType->getAnyNominal() ==
              Impl.SwiftContext.getIntDecl())
            break;
          LLVM_FALLTHROUGH;
        default:
          if (!underlyingResult.AbstractType->isEqual(mappedType)) {
            underlyingResult.AbstractType->dump();
            mappedType->dump();
          }
          assert(underlyingResult.AbstractType->isEqual(mappedType) &&
                 "typedef without special typedef kind was mapped "
                 "differently from its underlying type?");
        }
#endif
        hint = underlyingResult.Hint;

        // If the imported typealias is unavailable, return the
        // underlying type.
        if (decl->getAttrs().isUnavailable(Impl.SwiftContext))
          mappedType = underlyingResult.AbstractType;
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
      auto decl = dyn_cast_or_null<TypeDecl>(
          Impl.importDecl(type->getDecl(), Impl.CurrentVersion));
      if (!decl)
        return nullptr;

      return decl->getDeclaredInterfaceType();
    }

    /// Retrieve the 'Code' type for a bridged NSError, or nullptr if
    /// this is not a bridged NSError type.
    static TypeDecl *getBridgedNSErrorCode(TypeDecl *decl) {
      auto nominal = dyn_cast<NominalTypeDecl>(decl);
      if (!nominal) return nullptr;

      for (auto attr : decl->getAttrs().getAttributes<SynthesizedProtocolAttr,
                                                      false>()) {
        if (attr->getProtocolKind() ==
            KnownProtocolKind::BridgedStoredNSError) {
          auto &ctx = nominal->getASTContext();
          auto lookup = nominal->lookupDirect(ctx.Id_Code,
                                              /*ignoreNewExtensions=*/true);
          for (auto found : lookup) {
            if (auto codeDecl = dyn_cast<TypeDecl>(found))
              return codeDecl;
          }
          llvm_unreachable("couldn't find 'Code' within bridged error type");
        }
      }

      return nullptr;
    }

    /// Retrieve the adjusted type of a reference to the given type declaration.
    static Type getAdjustedTypeDeclReferenceType(TypeDecl *type) {
      // If the imported declaration is a bridged NSError, dig out
      // the Code nested type. References to the enum type from C
      // code need to map to the code type (which is ABI compatible with C),
      // and the bridged error type is used elsewhere.
      if (auto codeDecl = getBridgedNSErrorCode(type))
        return codeDecl->getDeclaredInterfaceType();

      return type->getDeclaredInterfaceType();
    }

    ImportResult VisitEnumType(const clang::EnumType *type) {
      auto clangDecl = type->getDecl();
      switch (Impl.getEnumKind(clangDecl)) {
      case EnumKind::Constants: {
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
      case EnumKind::Enum:
      case EnumKind::Unknown:
      case EnumKind::Options: {
        auto decl = dyn_cast_or_null<TypeDecl>(
            Impl.importDecl(clangDecl, Impl.CurrentVersion));
        if (!decl)
          return nullptr;

        return getAdjustedTypeDeclReferenceType(decl);
      }
      }

      llvm_unreachable("Invalid EnumKind.");
    }

    ImportResult VisitObjCObjectType(const clang::ObjCObjectType *type) {
      // We only handle pointers to objects.
      return nullptr;
    }

    /// Map the Clang swift_bridge attribute to a specific type.
    Type mapSwiftBridgeAttr(const clang::NamedDecl *clangDecl) {
      // Check whether there is a swift_bridge attribute.
      if (Impl.DisableSwiftBridgeAttr)
        return Type();
      auto bridgeAttr = clangDecl->getAttr<clang::SwiftBridgeAttr>();
      if (!bridgeAttr) return Type();

      // Determine the module and Swift declaration names.
      StringRef moduleName;
      StringRef name = bridgeAttr->getSwiftType();
      auto dotPos = name.find('.');
      if (dotPos == StringRef::npos) {
        // Determine the module name from the Clang declaration.
        if (auto module = clangDecl->getImportedOwningModule())
          moduleName = module->getTopLevelModuleName();
        else
          moduleName = clangDecl->getASTContext().getLangOpts().CurrentModule;
      } else {
        // The string is ModuleName.TypeName.
        moduleName = name.substr(0, dotPos);
        name = name.substr(dotPos + 1);
      }

      return Impl.getNamedSwiftType(moduleName, name);
    }

    ImportResult
    VisitObjCObjectPointerType(const clang::ObjCObjectPointerType *type) {
      // If this object pointer refers to an Objective-C class (possibly
      // qualified),
      if (auto objcClass = type->getInterfaceDecl()) {
        auto imported = cast_or_null<ClassDecl>(
            Impl.importDecl(objcClass, Impl.CurrentVersion));
        if (!imported)
          return nullptr;

        Type importedType;
        // If the objc type has any generic args, convert them and bind them to
        // the imported class type.
        if (imported->getGenericParams()) {
          unsigned typeParamCount = imported->getGenericParams()->size();
          auto typeArgs = type->getObjectType()->getTypeArgs();
          assert(typeArgs.empty() || typeArgs.size() == typeParamCount);
          llvm::SmallVector<Type, 2> importedTypeArgs;
          for (unsigned i = 0; i < typeParamCount; i++) {
            Type importedTypeArg;
            auto typeParam = imported->getGenericParams()->getParams()[i];
            if (!typeArgs.empty()) {
              auto subresult = Visit(typeArgs[i]);
              if (!subresult) {
                return nullptr;
              }
              importedTypeArg = subresult.AbstractType;
            } else if (typeParam->getSuperclass()) {
              importedTypeArg = typeParam->getSuperclass();
            } else {
              auto protocols = typeParam->getConformingProtocols();
              assert(!protocols.empty() &&
                  "objc imported type param should have either superclass or "
                  "protocol requirement");
              SmallVector<Type, 4> protocolTypes;
              for (auto protocolDecl : protocols) {
                protocolTypes.push_back(protocolDecl->getDeclaredType());
              }
              importedTypeArg = ProtocolCompositionType::get(
                  Impl.SwiftContext, protocolTypes);
            }
            importedTypeArgs.push_back(importedTypeArg);
          }
          assert(importedTypeArgs.size() == typeParamCount);
          importedType = BoundGenericClassType::get(
            imported, nullptr, importedTypeArgs);
        } else {
          importedType = imported->getDeclaredType();
        }
 
        if (!type->qual_empty()) {
          // As a special case, turn 'NSObject <NSCopying>' into
          // 'id <NSObject, NSCopying>', which can be imported more usefully.
          Type nsObjectTy = Impl.getNSObjectType();
          if (!nsObjectTy) {
            // Input is malformed
            return {};
          }
          if (nsObjectTy && importedType->isEqual(nsObjectTy)) {
            SmallVector<clang::ObjCProtocolDecl *, 4> protocols{
              type->qual_begin(), type->qual_end()
            };
            auto *nsObjectProto =
                Impl.getNSObjectProtocolType()->getAnyNominal();
            if (!nsObjectProto) {
              // Input is malformed
              return {};
            }
            auto *clangProto =
                cast<clang::ObjCProtocolDecl>(nsObjectProto->getClangDecl());
            protocols.push_back(
                const_cast<clang::ObjCProtocolDecl *>(clangProto));

            clang::ASTContext &clangCtx = Impl.getClangASTContext();
            clang::QualType protosOnlyType =
                clangCtx.getObjCObjectType(clangCtx.ObjCBuiltinIdTy,
                                           /*type args*/{},
                                           protocols,
                                           /*kindof*/false);
            return Visit(clangCtx.getObjCObjectPointerType(protosOnlyType));
          }
        }

        // Determine whether this Objective-C class type is bridged to
        // a Swift type.
        Type bridgedType;
        if (auto objcClassDef = objcClass->getDefinition())
          bridgedType = mapSwiftBridgeAttr(objcClassDef);
        else
          bridgedType = mapSwiftBridgeAttr(objcClass);

        if (bridgedType) {
          // Gather the type arguments.
          SmallVector<Type, 2> importedTypeArgs;
          ArrayRef<clang::QualType> typeArgs = type->getTypeArgs();
          SmallVector<clang::QualType, 2> typeArgsScratch;

          // If we have an unspecialized form of a parameterized
          // Objective-C class type, fill in the defaults.
          if (typeArgs.empty()) {
            if (auto objcGenericParams = objcClass->getTypeParamList()) {
              objcGenericParams->gatherDefaultTypeArgs(typeArgsScratch);
              typeArgs = typeArgsScratch;
            }
          }

          // Convert the type arguments.
          for (auto typeArg : typeArgs) {
            Type importedTypeArg = Impl.importType(typeArg,
                                                   ImportTypeKind::BridgedValue,
                                                   AllowNSUIntegerAsInt,
                                                   CanFullyBridgeTypes,
                                                   OTK_None);
            if (!importedTypeArg) {
              importedTypeArgs.clear();
              break;
            }

            importedTypeArgs.push_back(importedTypeArg);
          }

          // If we have an unbound generic bridged type, get the arguments.
          if (auto unboundType = bridgedType->getAs<UnboundGenericType>()) {
            auto unboundDecl = unboundType->getDecl();
            auto bridgedSig = unboundDecl->getGenericSignature();
            assert(bridgedSig && "Bridged signature");
            unsigned numExpectedTypeArgs = bridgedSig->getGenericParams().size();
            if (importedTypeArgs.size() != numExpectedTypeArgs)
              return Type();

            // The first type argument for Dictionary or Set needs
            // to be Hashable. Everything that inherits NSObject has a
            // -hash code in ObjC, but if something isn't NSObject, fall back
            // to AnyHashable as a key type.
            if (unboundDecl == Impl.SwiftContext.getDictionaryDecl() ||
                unboundDecl == Impl.SwiftContext.getSetDecl()) {
              auto &keyType = importedTypeArgs[0];
              if (!Impl.matchesNSObjectBound(keyType)) {
                if (auto anyHashable = Impl.SwiftContext.getAnyHashableDecl())
                  keyType = anyHashable->getDeclaredType();
                else
                  keyType = Type();
              }
            }

            // Form the specialized type.
            if (unboundDecl == Impl.SwiftContext.getArrayDecl()) {
              // Type sugar for arrays.
              assert(importedTypeArgs.size() == 1);
              bridgedType = ArraySliceType::get(importedTypeArgs[0]);
            } else if (unboundDecl == Impl.SwiftContext.getDictionaryDecl()) {
              // Type sugar for dictionaries.
              assert(importedTypeArgs.size() == 2);
              bridgedType = DictionaryType::get(importedTypeArgs[0],
                                                importedTypeArgs[1]);
            } else {
              // Everything else.
              bridgedType =
                  BoundGenericType::get(cast<NominalTypeDecl>(unboundDecl),
                                        Type(), importedTypeArgs);
            }
          }

          return { importedType,
                   ImportHint(ImportHint::ObjCBridged, bridgedType) };
        }

        return { importedType, ImportHint::ObjCPointer };
      }

      // If this is id<P> or Class<P>, turn this into a protocol type.
      if (type->isObjCQualifiedIdType() || type->isObjCQualifiedClassType()) {
        SmallVector<Type, 4> protocols;
        for (auto cp = type->qual_begin(), cpEnd = type->qual_end();
             cp != cpEnd; ++cp) {
          auto proto = cast_or_null<ProtocolDecl>(
              Impl.importDecl(*cp, Impl.CurrentVersion));
          if (!proto)
            return Type();

          protocols.push_back(proto->getDeclaredType());
        }

        Type result = ProtocolCompositionType::get(Impl.SwiftContext,
                                                   protocols);
        if (type->isObjCQualifiedClassType())
          result = ExistentialMetatypeType::get(result);

        return { result, ImportHint::ObjCPointer };
      }
      
      // Beyond here, we're using AnyObject.
      auto proto = Impl.SwiftContext.getProtocol(KnownProtocolKind::AnyObject);
      if (!proto)
        return Type();

      // id maps to Any in bridgeable contexts, AnyObject otherwise.
      if (type->isObjCIdType()) {
        return {proto->getDeclaredType(),
                ImportHint(ImportHint::ObjCBridged,
                           Impl.SwiftContext.TheAnyType)};
      }

      // Class maps to AnyObject.Type.
      assert(type->isObjCClassType());
      return { ExistentialMetatypeType::get(proto->getDeclaredType()),
               ImportHint::ObjCPointer };
    }
  };
} // end anonymous namespace

/// True if we're converting a function parameter, property type, or
/// function result type, and can thus safely apply representation
/// conversions for bridged types.
static bool canBridgeTypes(ImportTypeKind importKind) {
  switch (importKind) {
  case ImportTypeKind::Abstract:
  case ImportTypeKind::Typedef:
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
  case ImportTypeKind::PropertyWithReferenceSemantics:
  case ImportTypeKind::BridgedValue:
    return true;
  }

  llvm_unreachable("Invalid ImportTypeKind.");
}

/// True if the type has known CoreFoundation reference counting semantics.
static bool isCFAudited(ImportTypeKind importKind) {
  switch (importKind) {
  case ImportTypeKind::Abstract:
  case ImportTypeKind::Typedef:
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
  case ImportTypeKind::PropertyWithReferenceSemantics:
    return true;
  }

  llvm_unreachable("Invalid ImportTypeKind.");
}

/// Turn T into Unmanaged<T>.
static Type getUnmanagedType(ClangImporter::Implementation &impl,
                             Type payloadType) {
  NominalTypeDecl *unmanagedDecl = impl.SwiftContext.getUnmanagedDecl();
  if (!unmanagedDecl || unmanagedDecl->getGenericParams()->size() != 1)
    return payloadType;

  Type unmanagedClassType = BoundGenericType::get(unmanagedDecl,
                                                  /*parent*/ Type(),
                                                  payloadType);
  return unmanagedClassType;
}

/// Determine whether type is 'NSString.
static bool isNSString(Type type) {
  if (auto classType = type->getAs<ClassType>()) {
    if (auto clangDecl = classType->getDecl()->getClangDecl()) {
      if (auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl)) {
        return objcClass->getName() == "NSString";
      }
    }
  }

  return false;
}

static Type adjustTypeForConcreteImport(ClangImporter::Implementation &impl,
                                        clang::QualType clangType,
                                        Type importedType,
                                        ImportTypeKind importKind,
                                        ImportHint hint,
                                        bool allowNSUIntegerAsInt,
                                        bool canFullyBridgeTypes,
                                        OptionalTypeKind optKind) {
  if (importKind == ImportTypeKind::Abstract) {
    return importedType;
  }

  // 'void' can only be imported as a function result type.
  if (hint == ImportHint::Void &&
      (importKind == ImportTypeKind::AuditedResult ||
       importKind == ImportTypeKind::Result)) {
    return impl.getNamedSwiftType(impl.getStdlibModule(), "Void");
  }

  // Import NSString * globals as String.
  if (hint == ImportHint::ObjCBridged && isNSString(importedType) &&
      (importKind == ImportTypeKind::Variable ||
       importKind == ImportTypeKind::AuditedVariable)) {
    return hint.BridgedType;
  }

  // Reference types are only permitted as function parameter types.
  if (hint == ImportHint::Reference &&
      importKind == ImportTypeKind::Parameter) {
    auto refType = clangType->castAs<clang::ReferenceType>();
    // Import the underlying type.
    auto objectType = impl.importType(refType->getPointeeType(),
                                      ImportTypeKind::Pointee,
                                      allowNSUIntegerAsInt,
                                      canFullyBridgeTypes);
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
    if (elementClass->getName().str() !=
          impl.SwiftContext.getSwiftName(KnownFoundationEntity::NSError))
      return Type();

    ModuleDecl *foundationModule = impl.tryLoadFoundationModule();
    if (!foundationModule ||
        foundationModule->getName()
          != elementClass->getModuleContext()->getName())
      return Type();

    return impl.getNamedSwiftType(
            foundationModule,
            impl.SwiftContext.getSwiftName(
              KnownFoundationEntity::NSErrorPointer));
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
    if (boundGenericBase != impl.SwiftContext.getUnmanagedDecl())
      return Type();

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
  if (hint == ImportHint::Block) {
    if (!canFullyBridgeTypes) {
      if (auto typedefType = clangType->getAs<clang::TypedefType>()) {
        // In non-bridged contexts, drop the typealias sugar for blocks.
        // FIXME: This will do the wrong thing if there's any adjustment to do
        // besides optionality.
        Type underlyingTy = impl.importType(typedefType->desugar(),
                                            importKind,
                                            allowNSUIntegerAsInt,
                                            canFullyBridgeTypes,
                                            OTK_None);
        if (Type unwrappedTy = underlyingTy->getAnyOptionalObjectType())
          underlyingTy = unwrappedTy;
        if (!underlyingTy->isEqual(importedType))
          importedType = underlyingTy;
      }
    }

    if (canBridgeTypes(importKind) || importKind == ImportTypeKind::Typedef) {
      auto fTy = importedType->castTo<FunctionType>();
      FunctionType::ExtInfo einfo = fTy->getExtInfo();
      if (einfo.getRepresentation() != FunctionTypeRepresentation::Swift) {
        einfo = einfo.withRepresentation(FunctionTypeRepresentation::Swift);
        importedType = fTy->withExtInfo(einfo);
      }
    }
  }

  // Turn BOOL and DarwinBoolean into Bool in contexts that can bridge types
  // losslessly.
  if ((hint == ImportHint::BOOL || hint == ImportHint::Boolean) &&
      canFullyBridgeTypes && canBridgeTypes(importKind)) {
    return impl.SwiftContext.getBoolDecl()->getDeclaredType();
  }

  // When NSUInteger is used as an enum's underlying type or if it does not come
  // from a system module, make sure it stays unsigned.
  if (hint == ImportHint::NSUInteger) {
    if (importKind == ImportTypeKind::Enum || !allowNSUIntegerAsInt) {
      return importedType = impl.SwiftContext.getUIntDecl()->getDeclaredType();
    }
  }

  // Wrap CF pointers up as unmanaged types, unless this is an audited
  // context.
  if (hint == ImportHint::CFPointer && !isCFAudited(importKind)) {
    importedType = getUnmanagedType(impl, importedType);
  }

  // For types we import as new types in Swift, if the use is CF un-audited,
  // then we have to force it to be unmanaged
  if (hint == ImportHint::SwiftNewtypeFromCFPointer &&
      !isCFAudited(importKind)) {
    auto underlyingType = importedType->getSwiftNewtypeUnderlyingType();
    if (underlyingType)
      importedType = getUnmanagedType(impl, underlyingType);
  }

  // If we have a bridged Objective-C type and we are allowed to
  // bridge, do so.
  if (hint == ImportHint::ObjCBridged && canBridgeTypes(importKind) &&
      importKind != ImportTypeKind::PropertyWithReferenceSemantics) {
    // id and Any can be bridged without Foundation. There would be
    // bootstrapping issues with the ObjectiveC module otherwise.
    if (hint.BridgedType->isAny()
        || impl.tryLoadFoundationModule()
        || impl.ImportForwardDeclarations) {
      importedType = hint.BridgedType;
    }
  }

  if (!importedType)
    return importedType;

  if (importKind == ImportTypeKind::RecordField &&
      importedType->isAnyClassReferenceType()) {
    // Wrap retainable struct fields in Unmanaged.
    // FIXME: Eventually we might get C++-like support for strong pointers in
    // structs, at which point we should really be checking the lifetime
    // qualifiers.
    // FIXME: This should apply to blocks as well, but Unmanaged is constrained
    // to AnyObject.
    importedType = getUnmanagedType(impl, importedType);
  }

  // Treat va_list specially: null-unspecified va_list parameters should be
  // assumed to be non-optional. (Most people don't even think of va_list as a
  // pointer, and it's not a portable assumption anyway.)
  if (importKind == ImportTypeKind::Parameter &&
      optKind == OTK_ImplicitlyUnwrappedOptional) {
    if (auto *nominal = importedType->getNominalOrBoundGenericNominal()) {
      if (nominal->getName().str() == "CVaListPointer" &&
          nominal->getParentModule()->isStdlibModule()) {
        optKind = OTK_None;
      }
    }
  }

  // Wrap class, class protocol, function, and metatype types in an
  // optional type.
  if (importKind != ImportTypeKind::Typedef && canImportAsOptional(hint)) {
    importedType = getOptionalType(importedType, importKind, optKind);
  }

  return importedType;
}

Type ClangImporter::Implementation::importType(clang::QualType type,
                                               ImportTypeKind importKind,
                                               bool allowNSUIntegerAsInt,
                                               bool canFullyBridgeTypes,
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
  SwiftTypeConverter converter(*this, allowNSUIntegerAsInt,canFullyBridgeTypes);
  auto importResult = converter.Visit(type);

  // Now fix up the type based on we're concretely using it.
  return adjustTypeForConcreteImport(*this, type, importResult.AbstractType,
                                     importKind, importResult.Hint,
                                     allowNSUIntegerAsInt,
                                     canFullyBridgeTypes,
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
  if (isNSString(type))
    return true;

  return false;
}

/// Returns true if \p name contains the substring "Unsigned" or "unsigned".
static bool nameContainsUnsigned(StringRef name) {
  size_t pos = name.find("nsigned");
  if (pos == StringRef::npos || pos == 0)
    return false;
  --pos;
  return (name[pos] == 'u') || (name[pos] == 'U');
}

bool ClangImporter::Implementation::shouldAllowNSUIntegerAsInt(
    bool isFromSystemModule, const clang::NamedDecl *decl) {
  if (isFromSystemModule)
    if (auto identInfo = decl->getIdentifier())
      return !nameContainsUnsigned(identInfo->getName());
  return false;
}

Type ClangImporter::Implementation::importPropertyType(
       const clang::ObjCPropertyDecl *decl,
       bool isFromSystemModule) {
  const auto assignOrUnsafeUnretained =
      clang::ObjCPropertyDecl::OBJC_PR_assign |
      clang::ObjCPropertyDecl::OBJC_PR_unsafe_unretained;

  ImportTypeKind importKind;
  // HACK: Certain decls are always imported using bridged types,
  // because that's what a standalone method would do.
  if (shouldImportPropertyAsAccessors(decl)) {
    importKind = ImportTypeKind::Property;
  } else {
    switch (decl->getSetterKind()) {
    case clang::ObjCPropertyDecl::Assign:
      // If it's readonly, this might just be returned as a default.
      if (decl->isReadOnly() &&
          (decl->getPropertyAttributes() & assignOrUnsafeUnretained) == 0) {
        importKind = ImportTypeKind::Property;
      } else {
        importKind = ImportTypeKind::PropertyWithReferenceSemantics;
      }
      break;
    case clang::ObjCPropertyDecl::Retain:
    case clang::ObjCPropertyDecl::Copy:
      importKind = ImportTypeKind::Property;
      break;
    case clang::ObjCPropertyDecl::Weak:
      importKind = ImportTypeKind::PropertyWithReferenceSemantics;
      break;
    }
  }

  OptionalTypeKind optionality = OTK_ImplicitlyUnwrappedOptional;
  return importType(decl->getType(), importKind,
                    shouldAllowNSUIntegerAsInt(isFromSystemModule, decl),
                    /*isFullyBridgeable*/ true, optionality);
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

Type ClangImporter::Implementation::importFunctionReturnType(
    DeclContext *dc,
    const clang::FunctionDecl *clangDecl,
    bool allowNSUIntegerAsInt) {
  // CF function results can be managed if they are audited or
  // the ownership convention is explicitly declared.
  assert(clangDecl && "expected to have a decl to import");
  bool isAuditedResult =
    (clangDecl->hasAttr<clang::CFAuditedTransferAttr>() ||
     clangDecl->hasAttr<clang::CFReturnsRetainedAttr>() ||
     clangDecl->hasAttr<clang::CFReturnsNotRetainedAttr>());

  // Check if we know more about the type from our whitelists.
  OptionalTypeKind OptionalityOfReturn;
  if (clangDecl->hasAttr<clang::ReturnsNonNullAttr>()) {
    OptionalityOfReturn = OTK_None;
  } else {
    OptionalityOfReturn = OTK_ImplicitlyUnwrappedOptional;
  }

  // Import the result type.
  auto type = importType(clangDecl->getReturnType(),
                         (isAuditedResult ? ImportTypeKind::AuditedResult
                                          : ImportTypeKind::Result),
                         allowNSUIntegerAsInt,
                         /*isFullyBridgeable*/ true, OptionalityOfReturn);
  if (!type)
    return type;

  return dc->mapTypeOutOfContext(type);
}

Type ClangImporter::Implementation::
importFunctionType(DeclContext *dc,
                   const clang::FunctionDecl *clangDecl,
                   ArrayRef<const clang::ParmVarDecl *> params,
                   bool isVariadic, bool isFromSystemModule,
                   DeclName name, ParameterList *&parameterList) {

  bool allowNSUIntegerAsInt =
      shouldAllowNSUIntegerAsInt(isFromSystemModule, clangDecl);

  auto swiftResultTy =
      importFunctionReturnType(dc, clangDecl, allowNSUIntegerAsInt);
  if (!swiftResultTy)
    return Type();

  ArrayRef<Identifier> argNames = name.getArgumentNames();
  parameterList = importFunctionParameterList(dc, clangDecl, params, isVariadic,
                                              allowNSUIntegerAsInt, argNames);
  if (!parameterList)
    return Type();

  if (clangDecl->isNoReturn())
    swiftResultTy = SwiftContext.getNeverType();

  // Form the function type.
  auto argTy = parameterList->getType(SwiftContext);
  return FunctionType::get(argTy, swiftResultTy);
}

ParameterList *ClangImporter::Implementation::importFunctionParameterList(
    DeclContext *dc, const clang::FunctionDecl *clangDecl,
    ArrayRef<const clang::ParmVarDecl *> params, bool isVariadic,
    bool allowNSUIntegerAsInt, ArrayRef<Identifier> argNames) {
  // Import the parameters.
  SmallVector<ParamDecl *, 4> parameters;
  unsigned index = 0;
  llvm::SmallBitVector nonNullArgs = getNonNullArgs(clangDecl, params);

  for (auto param : params) {
    auto paramTy = param->getType();
    if (paramTy->isVoidType()) {
      ++index;
      continue;
    }

    // Check nullability of the parameter.
    OptionalTypeKind OptionalityOfParam =
        getParamOptionality(SwiftContext.LangOpts.EffectiveLanguageVersion,
                            param, !nonNullArgs.empty() && nonNullArgs[index]);

    ImportTypeKind importKind = ImportTypeKind::Parameter;
    if (param->hasAttr<clang::CFReturnsRetainedAttr>())
      importKind = ImportTypeKind::CFRetainedOutParameter;
    else if (param->hasAttr<clang::CFReturnsNotRetainedAttr>())
      importKind = ImportTypeKind::CFUnretainedOutParameter;

    // Import the parameter type into Swift.
    Type swiftParamTy =
        importType(paramTy, importKind, allowNSUIntegerAsInt,
                   /*isFullyBridgeable*/ true, OptionalityOfParam);
    if (!swiftParamTy)
      return nullptr;

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
    Identifier bodyName = importFullName(param, CurrentVersion)
                              .getDeclName()
                              .getBaseName();

    // Retrieve the argument name.
    Identifier name;
    if (index < argNames.size())
      name = argNames[index];

    // It doesn't actually matter which DeclContext we use, so just use the
    // imported header unit.
    auto paramInfo = createDeclWithClangNode<ParamDecl>(
        param, Accessibility::Private,
        /*IsLet*/ true, SourceLoc(), SourceLoc(), name,
        importSourceLoc(param->getLocation()), bodyName, swiftParamTy,
        ImportedHeaderUnit);
    paramInfo->setInterfaceType(
        dc->mapTypeOutOfContext(swiftParamTy));

    if (addNoEscapeAttr)
      paramInfo->getAttrs().add(new (SwiftContext)
                                  NoEscapeAttr(/*IsImplicit=*/false));

    parameters.push_back(paramInfo);
    ++index;
  }

  // Append an additional argument to represent varargs.
  if (isVariadic) {
    auto paramTy =
        BoundGenericType::get(SwiftContext.getArrayDecl(), Type(),
                              {SwiftContext.TheAnyType});
    auto name = SwiftContext.getIdentifier("varargs");
    auto param = new (SwiftContext)
        ParamDecl(true, SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                  name, paramTy, ImportedHeaderUnit);
    param->setInterfaceType(paramTy);

    param->setVariadic();
    parameters.push_back(param);
  }

  // Form the parameter list.
  return ParameterList::create(SwiftContext, parameters);
}

static bool isObjCMethodResultAudited(const clang::Decl *decl) {
  if (!decl)
    return false;
  return (decl->hasAttr<clang::CFReturnsRetainedAttr>() ||
          decl->hasAttr<clang::CFReturnsNotRetainedAttr>() ||
          decl->hasAttr<clang::ObjCReturnsInnerPointerAttr>());
}

DefaultArgumentKind ClangImporter::Implementation::inferDefaultArgument(
    clang::QualType type, OptionalTypeKind clangOptionality,
    Identifier baseName, unsigned numParams, StringRef argumentLabel,
    bool isFirstParameter, bool isLastParameter, NameImporter &nameImporter) {
  // Don't introduce a default argument for setters with only a single
  // parameter.
  if (numParams == 1 && camel_case::getFirstWord(baseName.str()) == "set")
    return DefaultArgumentKind::None;

  // Some nullable parameters default to 'nil'.
  if (clangOptionality == OTK_Optional) {
    // Nullable trailing closure parameters default to 'nil'.
    if (isLastParameter &&
        (type->isFunctionPointerType() || type->isBlockPointerType()))
      return DefaultArgumentKind::Nil;

    // NSZone parameters default to 'nil'.
    if (auto ptrType = type->getAs<clang::PointerType>()) {
      if (auto recType
            = ptrType->getPointeeType()->getAs<clang::RecordType>()) {
        if (recType->isStructureOrClassType() &&
            recType->getDecl()->getName() == "_NSZone")
          return DefaultArgumentKind::Nil;
      }
    }
  }

  // Don't introduce an empty options default arguments for setters.
  if (isFirstParameter && camel_case::getFirstWord(baseName.str()) == "set")
    return DefaultArgumentKind::None;

  // Option sets default to "[]" if they have "Options" in their name.
  if (const clang::EnumType *enumTy = type->getAs<clang::EnumType>())
    if (nameImporter.getEnumKind(enumTy->getDecl()) == EnumKind::Options) {
      auto enumName = enumTy->getDecl()->getName();
      for (auto word : reversed(camel_case::getWords(enumName))) {
        if (camel_case::sameWordIgnoreFirstCase(word, "options"))
          return DefaultArgumentKind::EmptyArray;
      }
    }

  // NSDictionary arguments default to [:] (or nil, if nullable) if "options",
  // "attributes", or "userInfo" occur in the argument label or (if there is no
  // argument label) at the end of the base name.
  if (auto objcPtrTy = type->getAs<clang::ObjCObjectPointerType>()) {
    if (auto objcClass = objcPtrTy->getInterfaceDecl()) {
      if (objcClass->getName() == "NSDictionary") {
        StringRef searchStr = argumentLabel;
        if (searchStr.empty() && !baseName.empty())
          searchStr = baseName.str();

        auto emptyDictionaryKind = DefaultArgumentKind::EmptyDictionary;
        if (clangOptionality == OTK_Optional)
          emptyDictionaryKind = DefaultArgumentKind::Nil;

        bool sawInfo = false;
        for (auto word : reversed(camel_case::getWords(searchStr))) {
          if (camel_case::sameWordIgnoreFirstCase(word, "options"))
            return emptyDictionaryKind;

          if (camel_case::sameWordIgnoreFirstCase(word, "attributes"))
            return emptyDictionaryKind;

          if (camel_case::sameWordIgnoreFirstCase(word, "info")) {
            sawInfo = true;
            continue;
          }

          if (sawInfo && camel_case::sameWordIgnoreFirstCase(word, "user"))
            return emptyDictionaryKind;

          if (argumentLabel.empty())
            break;

          sawInfo = false;
        }
      }
    }
  }

  return DefaultArgumentKind::None;
}

/// Adjust the result type of a throwing function based on the
/// imported error information.
static Type
adjustResultTypeForThrowingFunction(ForeignErrorConvention::Info errorInfo,
                                    Type resultTy) {
  switch (errorInfo.TheKind) {
  case ForeignErrorConvention::ZeroResult:
  case ForeignErrorConvention::NonZeroResult:
    return TupleType::getEmpty(resultTy->getASTContext());

  case ForeignErrorConvention::NilResult:
    resultTy = resultTy->getAnyOptionalObjectType();
    assert(resultTy &&
           "result type of NilResult convention was not imported as optional");
    return resultTy;

  case ForeignErrorConvention::ZeroPreservedResult:
  case ForeignErrorConvention::NonNilError:
    return resultTy;
  }

  llvm_unreachable("Invalid ForeignErrorConvention.");
}
                                     
/// Produce the foreign error convention from the imported error info,
/// error parameter type, and original result type.
static ForeignErrorConvention
getForeignErrorInfo(ForeignErrorConvention::Info errorInfo,
                    CanType errorParamTy, CanType origResultTy) {
  assert(errorParamTy && "not fully initialized!");
  using FEC = ForeignErrorConvention;
  auto ParamIndex = errorInfo.ErrorParameterIndex;
  auto IsOwned = (FEC::IsOwned_t) errorInfo.ErrorIsOwned;
  auto ReplaceParamWithVoid = errorInfo.ErrorParameterIsReplaced
                                ? FEC::IsReplaced
                                : FEC::IsNotReplaced;
  switch (errorInfo.TheKind) {
  case FEC::ZeroResult:
    return FEC::getZeroResult(ParamIndex, IsOwned, ReplaceParamWithVoid,
                              errorParamTy, origResultTy);
  case FEC::NonZeroResult:
    return FEC::getNonZeroResult(ParamIndex, IsOwned, ReplaceParamWithVoid,
                                 errorParamTy, origResultTy);
  case FEC::ZeroPreservedResult:
    return FEC::getZeroPreservedResult(ParamIndex, IsOwned,
                                       ReplaceParamWithVoid, errorParamTy);
  case FEC::NilResult:
    return FEC::getNilResult(ParamIndex, IsOwned, ReplaceParamWithVoid,
                             errorParamTy);
  case FEC::NonNilError:
    return FEC::getNonNilError(ParamIndex, IsOwned, ReplaceParamWithVoid,
                               errorParamTy);
  }
  llvm_unreachable("bad error convention");
}

Type ClangImporter::Implementation::importMethodType(
       const DeclContext *dc,
       const clang::ObjCMethodDecl *clangDecl,
       ArrayRef<const clang::ParmVarDecl *> params,
       bool isVariadic,
       bool isFromSystemModule,
       ParameterList **bodyParams,
       ImportedName importedName,
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
  if (isObjCMethodResultAudited(clangDecl))
    resultKind = ImportTypeKind::AuditedResult;
  else
    resultKind = ImportTypeKind::Result;

  // The member was defined in 'origDC', but is being imported into 'dc'.
  // 'dc' must be a subclass or a type conforming to protocol.
  DeclContext *origDC = importDeclContextOf(clangDecl,
                                            clangDecl->getDeclContext());
  assert(origDC);
  auto mapTypeIntoContext = [&](Type type) -> Type {
    if (dc != origDC) {
      // Replace origDC's archetypes with interface types.
      type = origDC->mapTypeOutOfContext(type);

      // Get the substitutions that we need to access a member of
      // 'origDC' on 'dc'.
      auto subs = dc->getDeclaredTypeInContext()
          ->getContextSubstitutionMap(dc->getParentModule(), origDC);

      // Apply them to the interface type to produce the final
      // substituted type.
      type = type.subst(subs);
    }
    return type;
  };

  // Import the result type.
  CanType origSwiftResultTy;
  Type swiftResultTy;
  Optional<ForeignErrorConvention::Info> errorInfo =
      importedName.getErrorInfo();
  OptionalTypeKind OptionalityOfReturn;
  if (clangDecl->hasAttr<clang::ReturnsNonNullAttr>()) {
    OptionalityOfReturn = OTK_None;
  } else {
    OptionalityOfReturn = OTK_ImplicitlyUnwrappedOptional;
  }

  bool allowNSUIntegerAsIntInResult = isFromSystemModule;
  if (allowNSUIntegerAsIntInResult) {
    clang::Selector sel = clangDecl->getSelector();
    StringRef name = sel.getNameForSlot(0);
    if (!name.empty()) {
      allowNSUIntegerAsIntInResult = !nameContainsUnsigned(name);
    }
  }

  clang::QualType resultType = clangDecl->getReturnType();
  swiftResultTy = importType(resultType, resultKind,
                             allowNSUIntegerAsIntInResult,
                             /*isFullyBridgeable*/true,
                             OptionalityOfReturn);
  // Adjust the result type for a throwing function.
  if (swiftResultTy && errorInfo) {

    // Get the original unbridged result type.
    origSwiftResultTy = importType(resultType, resultKind,
                               allowNSUIntegerAsIntInResult,
                               /*isFullyBridgeable*/false,
                               OptionalityOfReturn)
                            ->getCanonicalType();

    swiftResultTy = adjustResultTypeForThrowingFunction(*errorInfo,
                                                        swiftResultTy);
  }

  if (swiftResultTy &&
      clangDecl->getMethodFamily() == clang::OMF_performSelector) {
    // performSelector methods that return 'id' should be imported into Swift
    // as returning Unmanaged<AnyObject>.
    Type nonOptionalTy =
        swiftResultTy->getAnyOptionalObjectType(OptionalityOfReturn);
    if (!nonOptionalTy)
      nonOptionalTy = swiftResultTy;

    // Undo 'Any' bridging.
    if (nonOptionalTy->isAny())
      nonOptionalTy = SwiftContext.getProtocol(KnownProtocolKind::AnyObject)
        ->getDeclaredType();

    if (nonOptionalTy->isAnyClassReferenceType()) {
      swiftResultTy = getUnmanagedType(*this, nonOptionalTy);
      if (OptionalityOfReturn != OTK_None)
        swiftResultTy = OptionalType::get(OptionalityOfReturn, swiftResultTy);
    }
  }
  if (!swiftResultTy)
    return Type();
  swiftResultTy = mapTypeIntoContext(swiftResultTy);

  CanType errorParamType;

  llvm::SmallBitVector nonNullArgs = getNonNullArgs(clangDecl, params);

  // Import the parameters.
  SmallVector<ParamDecl*, 4> swiftParams;

  auto addEmptyTupleParameter = [&](Identifier argName) {
    // It doesn't actually matter which DeclContext we use, so just
    // use the imported header unit.
    auto type = TupleType::getEmpty(SwiftContext);
    auto var = new (SwiftContext) ParamDecl(/*IsLet*/ true, SourceLoc(),
                                            SourceLoc(), argName,
                                            SourceLoc(), argName, type,
                                            ImportedHeaderUnit);
    var->setInterfaceType(type);
    swiftParams.push_back(var);
  };

  // Determine the number of parameters.
  unsigned numEffectiveParams = params.size();
  if (errorInfo) --numEffectiveParams;

  auto argNames = importedName.getDeclName().getArgumentNames();
  unsigned nameIndex = 0;
  for (size_t paramIndex = 0; paramIndex != params.size(); paramIndex++) {
    auto param = params[paramIndex];
    auto paramTy = param->getType();
    if (paramTy->isVoidType()) {
      assert(!errorInfo || paramIndex != errorInfo->ErrorParameterIndex);
      ++nameIndex;
      continue;
    }

    if (kind == SpecialMethodKind::NSDictionarySubscriptGetter)
      nonNullArgs.empty();

    // Import the parameter type into Swift.

    // Check nullability of the parameter.
    OptionalTypeKind optionalityOfParam
        = getParamOptionality(SwiftContext.LangOpts.EffectiveLanguageVersion,
                              param,
                              !nonNullArgs.empty() && nonNullArgs[paramIndex]);

    bool allowNSUIntegerAsIntInParam = isFromSystemModule;
    if (allowNSUIntegerAsIntInParam) {
      StringRef name;
      clang::Selector sel = clangDecl->getSelector();
      if (nameIndex < sel.getNumArgs())
        name = sel.getNameForSlot(nameIndex);
      if (name.empty() && nameIndex == 0)
        name = sel.getNameForSlot(0);
      if (!name.empty())
        allowNSUIntegerAsIntInParam = !nameContainsUnsigned(name);
    }

    // Special case for NSDictionary's subscript.
    Type swiftParamTy;
    if (kind == SpecialMethodKind::NSDictionarySubscriptGetter &&
        paramTy->isObjCIdType()) {
      swiftParamTy = getOptionalType(getNSCopyingType(),
                                     ImportTypeKind::Parameter,
                                     optionalityOfParam);
    } else {
      ImportTypeKind importKind = ImportTypeKind::Parameter;
      if (param->hasAttr<clang::CFReturnsRetainedAttr>())
        importKind = ImportTypeKind::CFRetainedOutParameter;
      else if (param->hasAttr<clang::CFReturnsNotRetainedAttr>())
        importKind = ImportTypeKind::CFUnretainedOutParameter;
      
      swiftParamTy = importType(paramTy, importKind,
                                allowNSUIntegerAsIntInParam,
                                /*isFullyBridgeable*/true,
                                optionalityOfParam);
    }
    if (!swiftParamTy)
      return Type();

    // If this is the error parameter, remember it, but don't build it
    // into the parameter type.
    if (errorInfo && paramIndex == errorInfo->ErrorParameterIndex) {
      errorParamType = swiftParamTy->getCanonicalType();

      // ...unless we're supposed to replace it with ().
      if (errorInfo->ErrorParameterIsReplaced) {
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
    Identifier bodyName = importFullName(param, CurrentVersion)
                              .getDeclName()
                              .getBaseName();

    // Figure out the name for this argument, which comes from the method name.
    Identifier name;
    if (nameIndex < argNames.size()) {
      name = argNames[nameIndex];
    }
    ++nameIndex;

    // It doesn't actually matter which DeclContext we use, so just use the
    // imported header unit.
    swiftParamTy = mapTypeIntoContext(swiftParamTy);

    // Set up the parameter info.
    auto paramInfo
      = createDeclWithClangNode<ParamDecl>(param, Accessibility::Private,
                                           /*IsLet*/ true,
                                           SourceLoc(), SourceLoc(), name,
                                           importSourceLoc(param->getLocation()),
                                           bodyName, swiftParamTy,
                                           ImportedHeaderUnit);
    paramInfo->setInterfaceType(
        dc->mapTypeOutOfContext(swiftParamTy));

    if (addNoEscapeAttr) {
      paramInfo->getAttrs().add(
        new (SwiftContext) NoEscapeAttr(/*IsImplicit=*/false));
    }

    // Determine whether we have a default argument.
    if (kind == SpecialMethodKind::Regular ||
        kind == SpecialMethodKind::Constructor) {
      bool isLastParameter =
          (paramIndex == params.size() - 1) ||
          (paramIndex == params.size() - 2 && errorInfo &&
           errorInfo->ErrorParameterIndex == params.size() - 1);

      auto defaultArg = inferDefaultArgument(
          param->getType(), optionalityOfParam,
          importedName.getDeclName().getBaseName(), numEffectiveParams,
          name.empty() ? StringRef() : name.str(), paramIndex == 0,
          isLastParameter, getNameImporter());
      if (defaultArg != DefaultArgumentKind::None)
        paramInfo->setDefaultArgumentKind(defaultArg);
    }
    swiftParams.push_back(paramInfo);
  }

  // If we have a constructor with no parameters and a name with an
  // argument name, synthesize a Void parameter with that name.
  if (kind == SpecialMethodKind::Constructor && params.empty() && 
      argNames.size() == 1) {
    addEmptyTupleParameter(argNames[0]);
  }

  if (importedName.hasCustomName() && argNames.size() != swiftParams.size()) {
    // Note carefully: we're emitting a warning in the /Clang/ buffer.
    auto &srcMgr = getClangASTContext().getSourceManager();
    auto &rawDiagClient = Instance->getDiagnosticClient();
    auto &diagClient = static_cast<ClangDiagnosticConsumer &>(rawDiagClient);
    SourceLoc methodLoc =
        diagClient.resolveSourceLocation(srcMgr, clangDecl->getLocation());
    if (methodLoc.isValid()) {
      SwiftContext.Diags.diagnose(methodLoc, diag::invalid_swift_name_method,
                                  swiftParams.size() < argNames.size(),
                                  swiftParams.size(), argNames.size());
    }
    return Type();
  }

  
  // Form the parameter list.
  *bodyParams = ParameterList::create(SwiftContext, swiftParams);

  if (clangDecl->hasAttr<clang::NoReturnAttr>()) {
    origSwiftResultTy = SwiftContext.getNeverType();
    swiftResultTy = SwiftContext.getNeverType();
  }

  FunctionType::ExtInfo extInfo;

  if (errorInfo) {
    foreignErrorInfo = getForeignErrorInfo(*errorInfo, errorParamType,
                                           origSwiftResultTy);

    // Mark that the function type throws.
    extInfo = extInfo.withThrows(true);
  }
 
  swiftResultTy = dc->mapTypeOutOfContext(swiftResultTy);

  // Form the function type.
  return FunctionType::get(
      (*bodyParams)->getInterfaceType(SwiftContext),
      swiftResultTy, extInfo);
}

Type ClangImporter::Implementation::importAccessorMethodType(
    const DeclContext *dc,
    const clang::ObjCPropertyDecl *property,
    const clang::ObjCMethodDecl *clangDecl,
    bool isFromSystemModule,
    ImportedName functionName,
    swift::ParameterList **params) {
  // Note: We're using a pointer instead of a reference here to make it clear
  // at the call site that this is an out-parameter.
  assert(params && "'params' is a required out-parameter");

  // Determine if the method is a property getter or setter.
  bool isGetter;
  if (clangDecl->parameters().size() == 0)
    isGetter = true;
  else if (clangDecl->parameters().size() == 1)
    isGetter = false;
  else
    llvm_unreachable("not a valid accessor");

  // The member was defined in 'origDC', but is being imported into 'dc'.
  // 'dc' must be a subclass or a type conforming to protocol.
  // FIXME: Duplicated from importMethodType.
  DeclContext *origDC = importDeclContextOf(property,
                                            property->getDeclContext());
  assert(origDC);
  auto mapTypeIntoContext = [&](Type type) -> Type {
    if (dc != origDC) {
      // Replace origDC's archetypes with interface types.
      type = origDC->mapTypeOutOfContext(type);

      // Get the substitutions that we need to access a member of
      // 'origDC' on 'dc'.
      auto subs = dc->getDeclaredTypeInContext()
          ->getContextSubstitutionMap(dc->getParentModule(), origDC);

      // Apply them to the interface type to produce the final
      // substituted type.
      type = type.subst(subs);
    }
    return type;
  };

  // Import the property type, independent of what kind of accessor this is.
  Type propertyTy = importPropertyType(property, isFromSystemModule);
  if (!propertyTy)
    return Type();
  propertyTy = mapTypeIntoContext(propertyTy);
  Type propertyInterfaceTy = dc->mapTypeOutOfContext(propertyTy);

  // Now build up the resulting FunctionType and parameters.
  Type resultTy;
  if (isGetter) {
    *params = ParameterList::createEmpty(SwiftContext);
    resultTy = propertyInterfaceTy;

  } else {
    const clang::ParmVarDecl *param = clangDecl->parameters().front();
    ImportedName fullBodyName = importFullName(param, CurrentVersion);
    Identifier bodyName = fullBodyName.getDeclName().getBaseName();
    SourceLoc nameLoc = importSourceLoc(param->getLocation());
    Identifier argLabel = functionName.getDeclName().getArgumentNames().front();
    auto paramInfo
      = createDeclWithClangNode<ParamDecl>(param, Accessibility::Private,
                                           /*IsLet*/true,
                                           /*let loc*/SourceLoc(),
                                           /*label loc*/SourceLoc(),
                                           argLabel, nameLoc, bodyName,
                                           propertyTy,
                                           /*dummy DC*/ImportedHeaderUnit);
    paramInfo->setInterfaceType(propertyInterfaceTy);

    *params = ParameterList::create(SwiftContext, paramInfo);
    resultTy = SwiftContext.getVoidDecl()->getDeclaredInterfaceType();
  }

  return FunctionType::get((*params)->getInterfaceType(SwiftContext), resultTy);
}


ModuleDecl *ClangImporter::Implementation::getStdlibModule() {
  return SwiftContext.getStdlibModule(true);
}

ModuleDecl *ClangImporter::Implementation::getNamedModule(StringRef name) {
  return SwiftContext.getLoadedModule(SwiftContext.getIdentifier(name));
}

static ModuleDecl *tryLoadModule(ASTContext &C,
                             Identifier moduleName,
                             bool importForwardDeclarations,
                             llvm::DenseMap<Identifier, ModuleDecl *>
                               &checkedModules) {
  // If we've already done this check, return the cached result.
  auto known = checkedModules.find(moduleName);
  if (known != checkedModules.end())
    return known->second;

  ModuleDecl *module;

  // If we're synthesizing forward declarations, we don't want to pull in
  // the module too eagerly.
  if (importForwardDeclarations)
    module = C.getLoadedModule(moduleName);
  else
    module = C.getModule({ {moduleName, SourceLoc()} });

  checkedModules[moduleName] = module;
  return module;
}

ModuleDecl *ClangImporter::Implementation::tryLoadFoundationModule() {
  return tryLoadModule(SwiftContext, SwiftContext.Id_Foundation,
                       ImportForwardDeclarations, checkedModules);
}

ModuleDecl *ClangImporter::Implementation::tryLoadSIMDModule() {
  return tryLoadModule(SwiftContext, SwiftContext.Id_simd,
                       ImportForwardDeclarations, checkedModules);
}

Type ClangImporter::Implementation::getNamedSwiftType(ModuleDecl *module,
                                                      StringRef name) {
  if (!module)
    return Type();

  // Look for the type.
  Identifier identifier = SwiftContext.getIdentifier(name);
  SmallVector<ValueDecl *, 2> results;

  // Check if the lookup we're about to perform a lookup within is
  // a Clang module.
  for (auto *file : module->getFiles()) {
    if (auto clangUnit = dyn_cast<ClangModuleUnit>(file)) {
      // If we have an overlay, look in the overlay. Otherwise, skip
      // the lookup to avoid infinite recursion.
      if (auto module = clangUnit->getAdapterModule())
        module->lookupValue({ }, identifier,
                          NLKind::UnqualifiedLookup, results);
    } else {
      file->lookupValue({ }, identifier,
                        NLKind::UnqualifiedLookup, results);
    }
  }

  if (results.size() != 1)
    return Type();

  auto decl = dyn_cast<TypeDecl>(results.front());
  if (!decl)
    return Type();

  assert(!decl->hasClangNode() && "picked up the original type?");

  if (auto *typeResolver = getTypeResolver())
    typeResolver->resolveDeclSignature(decl);

  if (auto *nominalDecl = dyn_cast<NominalTypeDecl>(decl))
    return nominalDecl->getDeclaredType();
  return decl->getDeclaredInterfaceType();
}

Type ClangImporter::Implementation::getNamedSwiftType(StringRef moduleName,
                                                      StringRef name) {
  // Try to load the module.
  auto module = tryLoadModule(SwiftContext,
                              SwiftContext.getIdentifier(moduleName),
                              ImportForwardDeclarations, checkedModules);
  if (!module) return Type();

  return getNamedSwiftType(module, name);
}

Type
ClangImporter::Implementation::
getNamedSwiftTypeSpecialization(ModuleDecl *module, StringRef name,
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
  lookupResult.setAllowHidden(true);
  if (!sema.LookupName(lookupResult, /*Scope=*/nullptr)) {
    return nullptr;
  }

  for (auto decl : lookupResult) {
    if (auto swiftDecl =
            importDecl(decl->getUnderlyingDecl(), CurrentVersion)) {
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
  if (NSObjectType->isExactSuperclassOf(type, getTypeResolver()))
    return true;

  // Struct or enum type must have been bridged.
  // TODO: Check that the bridged type is Hashable?
  if (type->getStructOrBoundGenericStruct() ||
      type->getEnumOrBoundGenericEnum())
    return true;

  return false;
}

static Type getNamedProtocolType(ClangImporter::Implementation &impl,
                                 StringRef name) {
  auto &sema = impl.getClangSema();
  auto clangName = &sema.getASTContext().Idents.get(name);
  assert(clangName);

  // Perform name lookup into the global scope.
  clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                   clang::Sema::LookupObjCProtocolName);
  lookupResult.setAllowHidden(true);
  if (!sema.LookupName(lookupResult, /*Scope=*/nullptr))
    return Type();

  for (auto decl : lookupResult) {
    if (auto swiftDecl =
            impl.importDecl(decl->getUnderlyingDecl(), impl.CurrentVersion)) {
      if (auto protoDecl = dyn_cast<ProtocolDecl>(swiftDecl)) {
        return protoDecl->getDeclaredType();
      }
    }
  }

  return Type();
}

Type ClangImporter::Implementation::getNSCopyingType() {
  return getNamedProtocolType(*this, "NSCopying");
}

Type ClangImporter::Implementation::getNSObjectProtocolType() {
  return getNamedProtocolType(*this, "NSObject");
}
