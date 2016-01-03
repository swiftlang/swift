//===--- ImportType.cpp - Import Clang Types ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "ClangDiagnosticConsumer.h"
#include "swift/Strings.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsClangImporter.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/Token.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/TypeVisitor.h"
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

      /// The source type is 'Boolean'.
      Boolean,

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

      /// The source type is a specially-handled pointer type (usually a mapped
      /// typedef) that nonetheless needs to preserve nullability.
      CustomNullablePointer,
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
      case Boolean:
      case NSString:
      case NSUInteger:
      case ObjCPointer:
      case CFPointer:
      case Reference:
      case Block:
      case CFunctionPointer:
      case CustomNullablePointer:
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
    case ImportHint::NSArray:
    case ImportHint::NSDictionary:
    case ImportHint::NSSet:
    case ImportHint::NSString:
    case ImportHint::ObjCPointer:
    case ImportHint::CFunctionPointer:
    case ImportHint::CustomNullablePointer:
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
      case clang::BuiltinType::OCLImage2dDepth:
      case clang::BuiltinType::OCLImage2dArrayDepth:
      case clang::BuiltinType::OCLImage2dMSAA:
      case clang::BuiltinType::OCLImage2dArrayMSAA:
      case clang::BuiltinType::OCLImage2dMSAADepth:
      case clang::BuiltinType::OCLImage2dArrayMSAADepth:
      case clang::BuiltinType::OCLImage3d:
      case clang::BuiltinType::OCLSampler:
      case clang::BuiltinType::OCLEvent:
      case clang::BuiltinType::OCLClkEvent:
      case clang::BuiltinType::OCLQueue:
      case clang::BuiltinType::OCLNDRange:
      case clang::BuiltinType::OCLReserveID:
        return Type();

      // OpenMP types that don't have Swift equivalents.
      case clang::BuiltinType::OMPArraySection:
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
          Identifier Id_ObjectiveC = Impl.SwiftContext.Id_ObjectiveC;
          Module *objCModule = Impl.SwiftContext.getLoadedModule(Id_ObjectiveC);
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
                                      AllowNSUIntegerAsInt,
                                      /*can fully bridge*/false);

      // If the pointed-to type is unrepresentable in Swift, import as
      // COpaquePointer.
      if (!pointeeType)
        return getOpaquePointerType();
      
      if (pointeeQualType->isFunctionType()) {
        auto funcTy = pointeeType->castTo<FunctionType>();
        return {
          FunctionType::get(funcTy->getInput(), funcTy->getResult(),
            funcTy->getExtInfo().withRepresentation(
                          AnyFunctionType::Representation::CFunctionPointer)),
          ImportHint::CFunctionPointer
        };
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
                                         /*can fully bridge*/false);
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
                                      CanFullyBridgeTypes);
      if (!resultTy)
        return Type();

      SmallVector<TupleTypeElt, 4> params;
      for (auto param = type->param_type_begin(),
             paramEnd = type->param_type_end();
           param != paramEnd; ++param) {
        auto swiftParamTy = Impl.importType(*param, ImportTypeKind::Parameter,
                                            AllowNSUIntegerAsInt,
                                            CanFullyBridgeTypes);
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
                                      AllowNSUIntegerAsInt,
                                      CanFullyBridgeTypes);
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
        } else if (type->getDecl()->getName() == "Boolean") {
          // FIXME: Darwin only?
          hint = ImportHint::Boolean;
        } else if (type->getDecl()->getName() == "NSUInteger") {
          hint = ImportHint::NSUInteger;
        } else if (isImportedCFPointer(type->desugar(), mappedType)) {
          hint = ImportHint::CFPointer;
        } else if (mappedType->isAnyExistentialType()) { // id, Class
          hint = ImportHint::ObjCPointer;
        } else if (type->isBlockPointerType()) {
          // FIXME: This should eventually be "isAnyPointerType", but right now
          // non-object, non-block pointers are never Optional in Swift; they
          // just can have a value of 'nil' themselves.
          hint = ImportHint::CustomNullablePointer;
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
          SWIFT_FALLTHROUGH;
        default:
          assert(underlyingResult.AbstractType->isEqual(mappedType) &&
                 "typedef without special typedef kind was mapped "
                 "differently from its underlying type?");
        }
#endif
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
      switch (Impl.classifyEnum(Impl.getClangPreprocessor(), clangDecl)) {
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

        if (!type->qual_empty()) {
          // As a special case, turn 'NSObject <NSCopying>' into
          // 'id <NSObject, NSCopying>', which can be imported more usefully.
          Type nsObjectTy = Impl.getNSObjectType();
          if (nsObjectTy && importedType->isEqual(nsObjectTy)) {
            SmallVector<clang::ObjCProtocolDecl *, 4> protocols{
              type->qual_begin(), type->qual_end()
            };
            auto *nsObjectProto =
                Impl.getNSObjectProtocolType()->getAnyNominal();
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
                                               AllowNSUIntegerAsInt,
                                               CanFullyBridgeTypes,
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
                                           AllowNSUIntegerAsInt,
                                           CanFullyBridgeTypes,
                                           OTK_None);
            Type objectType = Impl.importType(typeArgs[1],
                                              ImportTypeKind::BridgedValue,
                                              AllowNSUIntegerAsInt,
                                              CanFullyBridgeTypes,
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
                                               AllowNSUIntegerAsInt,
                                               CanFullyBridgeTypes,
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
  case ImportTypeKind::PropertyAccessor:
  case ImportTypeKind::BridgedValue:
    return true;
  }
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
  case ImportTypeKind::PropertyAccessor:
    return true;
  }
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

  // When NSString* is the type of a function parameter or a function
  // result type, map it to String.
  // FIXME: It's not really safe to do this when Foundation is missing.
  // We do it anyway for ImportForwardDeclarations mode so that generated
  // interfaces are correct, but trying to use the resulting declarations
  // may result in compiler crashes further down the line.
  if (hint == ImportHint::NSString && canBridgeTypes(importKind) &&
      (impl.tryLoadFoundationModule() || impl.ImportForwardDeclarations)) {
    importedType = impl.SwiftContext.getStringDecl()->getDeclaredType();
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
  if (auto ptrType = type->getAs<clang::ObjCObjectPointerType>()) {
    if (auto interfaceType = ptrType->getInterfaceType()) {
      if (interfaceType->getDecl()->getName() == "NSString") {
        return true;
      }
    }
  }
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

Type ClangImporter::Implementation::importPropertyType(
       const clang::ObjCPropertyDecl *decl,
       bool isFromSystemModule) {
  OptionalTypeKind optionality = OTK_ImplicitlyUnwrappedOptional;
  if (auto info = getKnownObjCProperty(decl)) {
    if (auto nullability = info->getNullability())
      optionality = translateNullability(*nullability);
  }

  bool allowNSUIntegerAsInt = isFromSystemModule;
  if (allowNSUIntegerAsInt)
    allowNSUIntegerAsInt = !nameContainsUnsigned(decl->getName());

  return importType(decl->getType(), ImportTypeKind::Property,
                    allowNSUIntegerAsInt, /*isFullyBridgeable*/true,
                    optionality);
}

/// Get a bit vector indicating which arguments are non-null for a
/// given function or method.
llvm::SmallBitVector ClangImporter::Implementation::getNonNullArgs(
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

/// Determine the optionality of the given Clang parameter.
///
/// \param param The Clang parameter.
///
/// \param knownNonNull Whether a function- or method-level "nonnull" attribute
/// applies to this parameter.
///
/// \param knownNullability When API notes describe the nullability of this
/// parameter, that nullability.
static OptionalTypeKind getParamOptionality(
                          const clang::ParmVarDecl *param,
                          bool knownNonNull,
                          Optional<clang::NullabilityKind> knownNullability) {
  auto &clangCtx = param->getASTContext();

  // If nullability is available on the type, use it.
  if (auto nullability = param->getType()->getNullability(clangCtx)) {
    return ClangImporter::Implementation::translateNullability(*nullability);
  }

  // If it's known non-null, use that.
  if (knownNonNull || param->hasAttr<clang::NonNullAttr>())
    return OTK_None;

  // If API notes gives us nullability, use that.
  if (knownNullability)
    return ClangImporter::Implementation::translateNullability(
             *knownNullability);

  // Default to implicitly unwrapped optionals.
  return OTK_ImplicitlyUnwrappedOptional;
}

Type ClangImporter::Implementation::
importFunctionType(const clang::FunctionDecl *clangDecl,
                   clang::QualType resultType,
                   ArrayRef<const clang::ParmVarDecl *> params,
                   bool isVariadic, bool isNoReturn,
                   bool isFromSystemModule, bool hasCustomName,
                   ParameterList **parameterList, DeclName &name) {

  bool allowNSUIntegerAsInt = isFromSystemModule;
  if (allowNSUIntegerAsInt) {
    if (const clang::IdentifierInfo *clangNameID = clangDecl->getIdentifier()) {
      allowNSUIntegerAsInt = !nameContainsUnsigned(clangNameID->getName());
    }
  }

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
                                  allowNSUIntegerAsInt,
                                  /*isFullyBridgeable*/true,
                                  OptionalityOfReturn);
  if (!swiftResultTy)
    return Type();

  // Import the parameters.
  SmallVector<ParamDecl*, 4> parameters;
  unsigned index = 0;
  llvm::SmallBitVector nonNullArgs = getNonNullArgs(clangDecl, params);
  ArrayRef<Identifier> argNames = name.getArgumentNames();
  for (auto param : params) {
    auto paramTy = param->getType();
    if (paramTy->isVoidType()) {
      ++index;
      continue;
    }

    // Check nullability of the parameter.
    OptionalTypeKind OptionalityOfParam
      = getParamOptionality(param, !nonNullArgs.empty() && nonNullArgs[index],
                            knownFn
                              ? Optional<clang::NullabilityKind>(
                                  knownFn->getParamTypeInfo(index))
                              : None);

    ImportTypeKind importKind = ImportTypeKind::Parameter;
    if (param->hasAttr<clang::CFReturnsRetainedAttr>())
      importKind = ImportTypeKind::CFRetainedOutParameter;
    else if (param->hasAttr<clang::CFReturnsNotRetainedAttr>())
      importKind = ImportTypeKind::CFUnretainedOutParameter;

    // Import the parameter type into Swift.
    Type swiftParamTy = importType(paramTy, importKind,
                                   allowNSUIntegerAsInt,
                                   /*isFullyBridgeable*/true,
                                   OptionalityOfParam);
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
    Identifier bodyName = importFullName(param).Imported.getBaseName();

    // Retrieve the argument name.
    Identifier name;
    if (index < argNames.size())
      name = argNames[index];

    // It doesn't actually matter which DeclContext we use, so just use the
    // imported header unit.
    auto bodyVar
      = createDeclWithClangNode<ParamDecl>(param,
                                     /*IsLet*/ true,
                                     SourceLoc(), name,
                                     importSourceLoc(param->getLocation()),
                                     bodyName, swiftParamTy, 
                                     ImportedHeaderUnit);

    if (addNoEscapeAttr)
      bodyVar->getAttrs().add(
        new (SwiftContext) NoEscapeAttr(/*IsImplicit=*/false));

    parameters.push_back(bodyVar);
    ++index;
  }

  // Append an additional argument to represent varargs.
  if (isVariadic) {
    auto paramTy =  BoundGenericType::get(SwiftContext.getArrayDecl(), Type(),
      {SwiftContext.getAnyDecl()->getDeclaredType()});
    auto name = SwiftContext.getIdentifier("varargs");
    auto param = new (SwiftContext) ParamDecl(true, SourceLoc(),
                                                Identifier(),
                                                SourceLoc(), name, paramTy,
                                                ImportedHeaderUnit);

    param->setVariadic();
    parameters.push_back(param);
  }

  // Form the parameter list.
  *parameterList = ParameterList::create(SwiftContext, parameters);
  
  FunctionType::ExtInfo extInfo;
  extInfo = extInfo.withIsNoReturn(isNoReturn);
  
  // Form the function type.
  auto argTy = (*parameterList)->getType(SwiftContext);
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
      case FEC::ZeroPreservedResult:
        return FEC::getZeroPreservedResult(ParamIndex, IsOwned,
                                           ReplaceParamWithVoid, ParamType);
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

/// Determine whether this is the name of an Objective-C collection
/// with a single element type.
static bool isObjCCollectionName(StringRef typeName) {
  auto lastWord = camel_case::getLastWord(typeName);
  return lastWord == "Array" || lastWord == "Set";
}

/// Retrieve the name of the given Clang type for use when omitting
/// needless words.
OmissionTypeName ClangImporter::Implementation::getClangTypeNameForOmission(
                   clang::ASTContext &ctx, clang::QualType type) {
  if (type.isNull())
    return OmissionTypeName();

  // Dig through the type, looking for a typedef-name and stripping
  // references along the way.
  StringRef lastTypedefName;
  do {
    // The name of a typedef-name.
    auto typePtr = type.getTypePtr();
    if (auto typedefType = dyn_cast<clang::TypedefType>(typePtr)) {
      auto name = typedefType->getDecl()->getName();

      // For Objective-C type parameters, drop the "Type" suffix if
      // present.
      if (isa<clang::ObjCTypeParamDecl>(typedefType->getDecl())) {
        if (camel_case::getLastWord(name) == "Type") {
          name = name.drop_back(4);
        }

        return name;
      }

      // Objective-C selector type.
      if (ctx.hasSameUnqualifiedType(type, ctx.getObjCSelType()) &&
          name == "SEL")
        return "Selector";

      // Objective-C "id" type.
      if (type->isObjCIdType() && name == "id")
        return "Object";

      // Objective-C "Class" type.
      if (type->isObjCClassType() && name == "Class")
        return "Class";

      // Objective-C "BOOL" type.
      if (name == "BOOL")
        return OmissionTypeName("Bool", OmissionTypeFlags::Boolean);

      // If this is an imported CF type, use that name.
      StringRef CFName = getCFTypeName(typedefType->getDecl());
      if (!CFName.empty())
        return CFName;

      // If we have NS(U)Integer or CGFloat, return it.
      if (name == "NSInteger" || name == "NSUInteger" || name == "CGFloat")
        return name;

      // Otherwise, desugar one level...
      lastTypedefName = name;
      type = typedefType->getDecl()->getUnderlyingType();
      continue;
    }

    // Look through reference types.
    if (auto refType = dyn_cast<clang::ReferenceType>(typePtr)) {
      type = refType->getPointeeTypeAsWritten();
      continue;
    }

    // Look through pointer types.
    if (auto ptrType = dyn_cast<clang::PointerType>(typePtr)) {
      type = ptrType->getPointeeType();
      continue;
    }

    // Try to desugar one level...
    clang::QualType desugared = type.getSingleStepDesugaredType(ctx);
    if (desugared.getTypePtr() == type.getTypePtr())
      break;

    type = desugared;
  } while (true);

  // Objective-C object pointers.
  if (auto objcObjectPtr = type->getAs<clang::ObjCObjectPointerType>()) {
    auto objcClass = objcObjectPtr->getInterfaceDecl();

    // For id<Proto> or NSObject<Proto>, retrieve the name of "Proto".
    if (objcObjectPtr->getNumProtocols() == 1 &&
        (!objcClass || objcClass->getName() == "NSObject"))
      return (*objcObjectPtr->qual_begin())->getName();

    // If there is a class, use it.
    if (objcClass) {
      // If this isn't the name of an Objective-C collection, we're done.
      auto className = objcClass->getName();
      if (!isObjCCollectionName(className))
        return className;

      // If we don't have type arguments, the collection element type
      // is "Object".
      auto typeArgs = objcObjectPtr->getTypeArgs();
      if (typeArgs.empty())
        return OmissionTypeName(className, None, "Object");

      return OmissionTypeName(className, None,
                              getClangTypeNameForOmission(ctx,
                                                          typeArgs[0]).Name);
    }

    // Objective-C "id" type.
    if (objcObjectPtr->isObjCIdType())
      return "Object";

    // Objective-C "Class" type.
    if (objcObjectPtr->isObjCClassType())
      return "Class";

    return StringRef();
  }

  // Handle builtin types by importing them and getting the Swift name.
  if (auto builtinTy = type->getAs<clang::BuiltinType>()) {
    // Names of integer types.
    static const char *intTypeNames[] = {
      "UInt8",
      "UInt16",
      "UInt32",
      "UInt64",
      "UInt128"
    };

    /// Retrieve the name for an integer type based on its size.
    auto getIntTypeName = [&](bool isSigned) -> StringRef {
      switch (ctx.getTypeSize(builtinTy)) {
      case 8: return StringRef(intTypeNames[0]).substr(isSigned ? 1 : 0);
      case 16: return StringRef(intTypeNames[1]).substr(isSigned ? 1 : 0);
      case 32: return StringRef(intTypeNames[2]).substr(isSigned ? 1 : 0);
      case 64: return StringRef(intTypeNames[3]).substr(isSigned ? 1 : 0);
      case 128: return StringRef(intTypeNames[4]).substr(isSigned ? 1 : 0);
      default: llvm_unreachable("bad integer type size");
      }
    };

    switch (builtinTy->getKind()) {
    case clang::BuiltinType::Void:
      return "Void";

    case clang::BuiltinType::Bool:
      return OmissionTypeName("Bool", OmissionTypeFlags::Boolean);

    case clang::BuiltinType::Float:
      return "Float";

    case clang::BuiltinType::Double:
      return "Double";

    case clang::BuiltinType::Char16:
      return "UInt16";

    case clang::BuiltinType::Char32:
      return "UnicodeScalar";

    case clang::BuiltinType::Char_U:
    case clang::BuiltinType::UChar:
    case clang::BuiltinType::UShort:
    case clang::BuiltinType::UInt:
    case clang::BuiltinType::ULong:
    case clang::BuiltinType::ULongLong:
    case clang::BuiltinType::UInt128:
    case clang::BuiltinType::WChar_U:
      return getIntTypeName(false);

    case clang::BuiltinType::Char_S:
    case clang::BuiltinType::SChar:
    case clang::BuiltinType::Short:
    case clang::BuiltinType::Int:
    case clang::BuiltinType::Long:
    case clang::BuiltinType::LongLong:
    case clang::BuiltinType::Int128:
    case clang::BuiltinType::WChar_S:
      return getIntTypeName(true);

    // Types that cannot be mapped into Swift, and probably won't ever be.
    case clang::BuiltinType::Dependent:
    case clang::BuiltinType::ARCUnbridgedCast:
    case clang::BuiltinType::BoundMember:
    case clang::BuiltinType::BuiltinFn:
    case clang::BuiltinType::Overload:
    case clang::BuiltinType::PseudoObject:
    case clang::BuiltinType::UnknownAny:
      return OmissionTypeName();

    // FIXME: Types that can be mapped, but aren't yet.
    case clang::BuiltinType::Half:
    case clang::BuiltinType::LongDouble:
    case clang::BuiltinType::NullPtr:
      return OmissionTypeName();

    // Objective-C types that aren't mapped directly; rather, pointers to
    // these types will be mapped.
    case clang::BuiltinType::ObjCClass:
    case clang::BuiltinType::ObjCId:
    case clang::BuiltinType::ObjCSel:
      return OmissionTypeName();

    // OpenCL types that don't have Swift equivalents.
    case clang::BuiltinType::OCLImage1d:
    case clang::BuiltinType::OCLImage1dArray:
    case clang::BuiltinType::OCLImage1dBuffer:
    case clang::BuiltinType::OCLImage2d:
    case clang::BuiltinType::OCLImage2dArray:
    case clang::BuiltinType::OCLImage2dDepth:
    case clang::BuiltinType::OCLImage2dArrayDepth:
    case clang::BuiltinType::OCLImage2dMSAA:
    case clang::BuiltinType::OCLImage2dArrayMSAA:
    case clang::BuiltinType::OCLImage2dMSAADepth:
    case clang::BuiltinType::OCLImage2dArrayMSAADepth:
    case clang::BuiltinType::OCLImage3d:
    case clang::BuiltinType::OCLSampler:
    case clang::BuiltinType::OCLEvent:
    case clang::BuiltinType::OCLClkEvent:
    case clang::BuiltinType::OCLQueue:
    case clang::BuiltinType::OCLNDRange:
    case clang::BuiltinType::OCLReserveID:
      return OmissionTypeName();

    // OpenMP types that don't have Swift equivalents.
    case clang::BuiltinType::OMPArraySection:
      return OmissionTypeName();
    }
  }

  // Tag types.
  if (auto tagType = type->getAs<clang::TagType>()) {
    if (tagType->getDecl()->getName().empty())
      return lastTypedefName;

    return tagType->getDecl()->getName();
  }

  // Block pointers.
  if (type->getAs<clang::BlockPointerType>())
    return "Block";

  // Function pointers.
  if (type->isFunctionType())
    return "Function";

  return StringRef();
}

/// Attempt to omit needless words from the given function name.
bool ClangImporter::Implementation::omitNeedlessWordsInFunctionName(
       clang::Sema &clangSema,
       StringRef &baseName,
       SmallVectorImpl<StringRef> &argumentNames,
       ArrayRef<const clang::ParmVarDecl *> params,
       clang::QualType resultType,
       const clang::DeclContext *dc,
       const llvm::SmallBitVector &nonNullArgs,
       const Optional<api_notes::ObjCMethodInfo> &knownMethod,
       Optional<unsigned> errorParamIndex,
       bool returnsSelf,
       bool isInstanceMethod,
       StringScratchSpace &scratch) {
  clang::ASTContext &clangCtx = clangSema.Context;

  // Collect the parameter type names.
  StringRef firstParamName;
  SmallVector<OmissionTypeName, 4> paramTypes;
  for (unsigned i = 0, n = params.size(); i != n; ++i) {
    auto param = params[i];

    // Capture the first parameter name.
    if (i == 0)
      firstParamName = param->getName();

    // Determine the number of parameters.
    unsigned numParams = params.size();
    if (errorParamIndex) --numParams;

    bool isLastParameter
      = (i == params.size() - 1) ||
        (i == params.size() - 2 &&
         errorParamIndex && *errorParamIndex == params.size() - 1);

    // Figure out whether there will be a default argument for this
    // parameter.
    bool hasDefaultArg
      = canInferDefaultArgument(
          clangSema.PP,
          param->getType(),
          getParamOptionality(param,
                              !nonNullArgs.empty() && nonNullArgs[i],
                              knownMethod && knownMethod->NullabilityAudited
                                ? Optional<clang::NullabilityKind>(
                                    knownMethod->getParamTypeInfo(i))
                                : None),
          SwiftContext.getIdentifier(baseName), numParams,
          isLastParameter);

    paramTypes.push_back(getClangTypeNameForOmission(clangCtx, param->getType())
                            .withDefaultArgument(hasDefaultArg));
  }

  // Find the property names.
  const InheritedNameSet *allPropertyNames = nullptr;
  auto contextType = getClangDeclContextType(dc);
  if (!contextType.isNull()) {
    if (auto objcPtrType = contextType->getAsObjCInterfacePointerType())
      if (auto objcClassDecl = objcPtrType->getInterfaceDecl())
        allPropertyNames = SwiftContext.getAllPropertyNames(objcClassDecl,
                                                            isInstanceMethod);
  }

  // Omit needless words.
  return omitNeedlessWords(baseName, argumentNames, firstParamName,
                           getClangTypeNameForOmission(clangCtx, resultType),
                           getClangTypeNameForOmission(clangCtx, contextType),
                           paramTypes, returnsSelf, /*isProperty=*/false,
                           allPropertyNames, scratch);
}

/// Retrieve the instance type of the given Clang declaration context.
clang::QualType ClangImporter::Implementation::getClangDeclContextType(
                  const clang::DeclContext *dc) {
  auto &ctx = getClangASTContext();
  if (auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(dc))
    return ctx.getObjCObjectPointerType(ctx.getObjCInterfaceType(objcClass));

  if (auto objcCategory = dyn_cast<clang::ObjCCategoryDecl>(dc)) {
    return ctx.getObjCObjectPointerType(
             ctx.getObjCInterfaceType(
               objcCategory->getClassInterface()));
  }

  if (auto tag = dyn_cast<clang::TagDecl>(dc)) {
    return ctx.getTagDeclType(tag);
  }

  return clang::QualType();
}

bool ClangImporter::Implementation::canInferDefaultArgument(
       clang::Preprocessor &pp, clang::QualType type,
       OptionalTypeKind clangOptionality, Identifier baseName,
       unsigned numParams, bool isLastParameter) {
  // Don't introduce a default argument for setters with only a single
  // parameter.
  if (numParams == 1 && camel_case::getFirstWord(baseName.str()) == "set")
    return false;

  // Some nullable parameters default to 'nil'.
  if (clangOptionality == OTK_Optional) {
    // Nullable trailing closure parameters default to 'nil'.
    if (isLastParameter &&
        (type->isFunctionPointerType() || type->isBlockPointerType()))
    return true;

    // NSZone parameters default to 'nil'.
    if (auto ptrType = type->getAs<clang::PointerType>()) {
      if (auto recType
            = ptrType->getPointeeType()->getAs<clang::RecordType>()) {
        if (recType->isStructureOrClassType() &&
            recType->getDecl()->getName() == "_NSZone")
          return true;
      }
    }
  }

  // Option sets default to "[]" if they have "Options" in their name.
  if (const clang::EnumType *enumTy = type->getAs<clang::EnumType>())
    if (classifyEnum(pp, enumTy->getDecl()) == EnumKind::Options) {
      auto enumName = enumTy->getDecl()->getName();
      for (auto word : reversed(camel_case::getWords(enumName))) {
        if (camel_case::sameWordIgnoreFirstCase(word, "options"))
          return true;
      }
    }

  return false;
}

/// Adjust the result type of a throwing function based on the
/// imported error information.
static Type adjustResultTypeForThrowingFunction(
              const ClangImporter::Implementation::ImportedErrorInfo &errorInfo,
              Type resultTy) {
  switch (errorInfo.Kind) {
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
}
                                     
/// Produce the foreign error convention from the imported error info,
/// error parameter type, and original result type.
static ForeignErrorConvention
getForeignErrorInfo(
    const ClangImporter::Implementation::ImportedErrorInfo &errorInfo,
    CanType errorParamTy, CanType origResultTy) {
  assert(errorParamTy && "not fully initialized!");
  using FEC = ForeignErrorConvention;
  auto ReplaceParamWithVoid = errorInfo.ReplaceParamWithVoid
                                ? FEC::IsReplaced
                                : FEC::IsNotReplaced;
  switch (errorInfo.Kind) {
  case FEC::ZeroResult:
    return FEC::getZeroResult(errorInfo.ParamIndex, errorInfo.IsOwned,
                              ReplaceParamWithVoid, errorParamTy, origResultTy);
  case FEC::NonZeroResult:
    return FEC::getNonZeroResult(errorInfo.ParamIndex, errorInfo.IsOwned,
                                 ReplaceParamWithVoid, errorParamTy,
                                 origResultTy);
  case FEC::ZeroPreservedResult:
    return FEC::getZeroPreservedResult(errorInfo.ParamIndex, errorInfo.IsOwned,
                                       ReplaceParamWithVoid, errorParamTy);
  case FEC::NilResult:
    return FEC::getNilResult(errorInfo.ParamIndex, errorInfo.IsOwned,
                             ReplaceParamWithVoid, errorParamTy);
  case FEC::NonNilError:
    return FEC::getNonNilError(errorInfo.ParamIndex, errorInfo.IsOwned,
                               ReplaceParamWithVoid, errorParamTy);
  }
  llvm_unreachable("bad error convention");
}

Type ClangImporter::Implementation::importMethodType(
       const clang::ObjCMethodDecl *clangDecl,
       clang::QualType resultType,
       ArrayRef<const clang::ParmVarDecl *> params,
       bool isVariadic, bool isNoReturn,
       bool isFromSystemModule,
       ParameterList **bodyParams,
       ImportedName importedName,
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
  CanType origSwiftResultTy;
  Type swiftResultTy;
  auto errorInfo = importedName.ErrorInfo;
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

    bool allowNSUIntegerAsIntInResult = isFromSystemModule;
    if (allowNSUIntegerAsIntInResult) {
      Identifier name = methodName.getBaseName();
      if (!name.empty()) {
        allowNSUIntegerAsIntInResult = !nameContainsUnsigned(name.str());
      }
    }

    swiftResultTy = importType(resultType, resultKind,
                               allowNSUIntegerAsIntInResult,
                               /*isFullyBridgeable*/true,
                               OptionalityOfReturn);
    // Adjust the result type for a throwing function.
    if (swiftResultTy && errorInfo) {
      origSwiftResultTy = swiftResultTy->getCanonicalType();
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

      if (nonOptionalTy->isAnyClassReferenceType()) {
        swiftResultTy = getUnmanagedType(*this, nonOptionalTy);
        if (OptionalityOfReturn != OTK_None)
          swiftResultTy = OptionalType::get(OptionalityOfReturn, swiftResultTy);
      }
    }
  }
  if (!swiftResultTy)
    return Type();

  CanType errorParamType;

  llvm::SmallBitVector nonNullArgs = getNonNullArgs(clangDecl, params);

  // Import the parameters.
  SmallVector<ParamDecl*, 4> swiftParams;

  auto addEmptyTupleParameter = [&](Identifier argName) {
    // It doesn't actually matter which DeclContext we use, so just
    // use the imported header unit.
    auto type = TupleType::getEmpty(SwiftContext);
    auto var = new (SwiftContext) ParamDecl(/*IsLet*/ true,
                                            SourceLoc(), argName,
                                            SourceLoc(), argName, type,
                                            ImportedHeaderUnit);
    swiftParams.push_back(var);
  };

  // Determine the number of parameters.
  unsigned numEffectiveParams = params.size();
  if (errorInfo) --numEffectiveParams;

  auto argNames = methodName.getArgumentNames();
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
    OptionalTypeKind optionalityOfParam
      = getParamOptionality(param,
                            !nonNullArgs.empty() && nonNullArgs[paramIndex],
                            knownMethod
                              ? Optional<clang::NullabilityKind>(
                                  knownMethod->getParamTypeInfo(paramIndex))
                              : None);

    bool allowNSUIntegerAsIntInParam = isFromSystemModule;
    if (allowNSUIntegerAsIntInParam) {
      Identifier name;
      if (nameIndex < argNames.size())
        name = argNames[nameIndex];
      if (name.empty() && nameIndex == 0)
        name = methodName.getBaseName();
      if (!name.empty())
        allowNSUIntegerAsIntInParam = !nameContainsUnsigned(name.str());
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
                                allowNSUIntegerAsIntInParam,
                                /*isFullyBridgeable*/true,
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
    if (errorInfo && paramIndex == errorInfo->ParamIndex) {
      errorParamType = swiftParamTy->getCanonicalType();

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
    Identifier bodyName = importFullName(param).Imported.getBaseName();

    // Figure out the name for this argument, which comes from the method name.
    Identifier name;
    if (nameIndex < argNames.size()) {
      name = argNames[nameIndex];
    }
    ++nameIndex;

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

    // Set up the parameter info.
    auto paramInfo = bodyVar;
    
    // Determine whether we have a default argument.
    if (InferDefaultArguments &&
        (kind == SpecialMethodKind::Regular ||
         kind == SpecialMethodKind::Constructor)) {
      bool isLastParameter = (paramIndex == params.size() - 1) ||
        (paramIndex == params.size() - 2 &&
         errorInfo && errorInfo->ParamIndex == params.size() - 1);
      
      if (canInferDefaultArgument(getClangPreprocessor(),
                                  param->getType(), optionalityOfParam,
                                  methodName.getBaseName(), numEffectiveParams,
                                  isLastParameter))
        paramInfo->setDefaultArgumentKind(DefaultArgumentKind::Normal);
    }
    swiftParams.push_back(paramInfo);
  }

  // If we have a constructor with no parameters and a name with an
  // argument name, synthesize a Void parameter with that name.
  if (kind == SpecialMethodKind::Constructor && params.empty() && 
      argNames.size() == 1) {
    addEmptyTupleParameter(argNames[0]);
  }

  if (importedName.HasCustomName && argNames.size() != swiftParams.size()) {
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
  
  FunctionType::ExtInfo extInfo;
  extInfo = extInfo.withIsNoReturn(isNoReturn);

  if (errorInfo) {
    foreignErrorInfo = getForeignErrorInfo(*errorInfo, errorParamType,
                                           origSwiftResultTy);

    // Mark that the function type throws.
    extInfo = extInfo.withThrows(true);
  }
  
  // Form the function type.
  return FunctionType::get((*bodyParams)->getType(SwiftContext),
                           swiftResultTy, extInfo);
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

static Type getNamedProtocolType(ClangImporter::Implementation &impl,
                                 StringRef name) {
  auto &sema = impl.getClangSema();
  auto clangName = &sema.getASTContext().Idents.get(name);
  assert(clangName);

  // Perform name lookup into the global scope.
  clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                   clang::Sema::LookupObjCProtocolName);
  if (!sema.LookupName(lookupResult, /*Scope=*/0))
    return Type();

  for (auto decl : lookupResult) {
    if (auto swiftDecl = impl.importDecl(decl->getUnderlyingDecl())) {
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

Type ClangImporter::Implementation::getCFStringRefType() {
  if (auto decl = dyn_cast_or_null<TypeDecl>(importDeclByName("CFStringRef")))
    return decl->getDeclaredType();
  return Type();
}

