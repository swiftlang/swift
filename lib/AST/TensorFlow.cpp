//===--- TensorFlow.cpp - AST Level TensorFlow Support Logic --------------===//
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
// This file implements the AST level TensorFlow support logic that is used
// across the Swift compiler.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/TensorFlow.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#ifdef SWIFT_ENABLE_TENSORFLOW
#include "tensorflow/c/c_api.h"
#endif
using namespace swift;
using namespace tf;

/// Return true if the given type represents a TensorFlow dtype.
bool tf::isTensorFlowDType(Type ty) {
  auto nominal = ty->getAnyNominal();
  if (!nominal)
    return false;
  auto &ctx = ty->getASTContext();
  auto tensorProto =
      ctx.getProtocol(KnownProtocolKind::TensorFlowDataTypeCompatible);
  if (!tensorProto)
    return false;
  SmallVector<ProtocolConformance *, 2> conformances;
  nominal->lookupConformance(nullptr, tensorProto, conformances);
  return !conformances.empty();
}

static bool is64(Type ty) {
  return ty->getASTContext().LangOpts.Target.isArch64Bit();
}

/// This function maps a Swift type (either a language type like Float or an
/// LLVM Builtin type like Builtin.f32) into the TensorFlow TF_DataType value.
///
/// This returns 0 (which is an invalid tensorflow type ID) on error.
///
unsigned tf::convertSwiftTypeToTF(Type ty) {
#ifdef SWIFT_ENABLE_TENSORFLOW
  // Handle wrappers like Float, which come up in TensorHandle<Float>
  if (auto *s = ty->getAs<StructType>()) {
    // Make sure the type is defined inside the Swift module.
    auto context = s->getDecl()->getDeclContext()->getParentModule();
    if (!context || context->getName().str() != "Swift")
      return 0;

    return llvm::StringSwitch<unsigned>(s->getDecl()->getNameStr())
        .Case("Bool", TF_BOOL)
        .Case("Int8", TF_INT8)
        .Case("UInt8", TF_UINT8)
        .Case("Int16", TF_INT16)
        .Case("UInt16", TF_UINT16)
        .Case("Int32", TF_INT32)
        .Case("UInt32", TF_UINT32)
        .Case("Int64", TF_INT64)
        .Case("UInt64", TF_UINT64)
        .Case("Int8", TF_INT8)
        .Case("UInt8", TF_UINT8)
        .Case("BFloat16", TF_BFLOAT16)
        .Case("Float", TF_FLOAT)
        .Case("Double", TF_DOUBLE)
        .Case("Int", is64(s) ? TF_INT64 : TF_INT32)
        .Case("UInt", is64(s) ? TF_UINT64 : TF_UINT32)
        .Case("String", TF_STRING)
        .Default(0);
  }

  // BuiltinIntegerType doesn't carry sign information, which TensorFlow needs,
  // so we can't rely on getting type information from the builtin types
  // themselves.  For now we'll just use signed types.
  if (auto *BII = ty->getAs<BuiltinIntegerType>()) {
    if (BII->getWidth().isPointerWidth())
      return is64(ty) ? TF_INT64 : TF_INT32;

    switch (BII->getFixedWidth()) {
    case 1:
      return TF_BOOL;
    case 8:
      return TF_INT8;
    case 16:
      return TF_INT16;
    case 32:
      return TF_INT32;
    case 64:
      return TF_INT64;
    }
  }

  if (auto *BIF = ty->getAs<BuiltinFloatType>()) {
    switch (BIF->getFPKind()) {
    case BuiltinFloatType::IEEE16:
      return TF_HALF;
    case BuiltinFloatType::IEEE32:
      return TF_FLOAT;
    case BuiltinFloatType::IEEE64:
      return TF_DOUBLE;
    case BuiltinFloatType::IEEE80:
    case BuiltinFloatType::IEEE128:
    case BuiltinFloatType::PPC128:
      return 0;
    }
  }

  if (auto *BRPT = ty->getAs<BuiltinRawPointerType>()) {
    return TF_STRING;
  }
#endif
  return 0;
}

/// If the specified type is the well-known TensorHandle<T> type, then return
/// "T".  If not, return a null type.
Type tf::getTensorHandleElementType(Type ty) {
  // TODO: Check that this type is declared in the TensorFlow module.
  if (auto *bgct = ty->getAs<BoundGenericClassType>()) {
    if (bgct->getDecl()->getNameStr() == "TensorHandle") {
      assert(bgct->getGenericArgs().size() == 1 && "Expected one generic arg");
      return bgct->getGenericArgs()[0];
    }
  }
  return Type();
}

/// Determine whether the specified type is one of our well-known types, and
/// if so, which one it is.
TFValueKind tf::classifyTensorFlowValue(Type ty) {
  // TODO: Check that these types are declared in the TensorFlow module.
  if (auto *ct = ty->getAs<ClassType>()) {
    auto name = ct->getDecl()->getNameStr();
    if (name == "ResourceHandle")
      return TFValueKind::ResourceHandle;
    if (name == "VariantHandle")
      return TFValueKind::VariantHandle;
  }

  if (getTensorHandleElementType(ty))
    return TFValueKind::TensorHandle;
  return TFValueKind::Nope;
}

/// Return true if the specified type is a TensorHandle<T>.
bool tf::isTensorHandle(Type ty) {
  return classifyTensorFlowValue(ty) == TFValueKind::TensorHandle;
}

/// Return true if the specified type is an opaque handle, such as
/// VariantHandle and ResourceHandle.
bool tf::isOpaqueHandle(Type ty) {
  auto kind = classifyTensorFlowValue(ty);
  return kind != TFValueKind::Nope && kind != TFValueKind::TensorHandle;
}

/// Return true if the specified type is TensorHandle<T>, ResourceHandle, or
/// VariantHandle.
bool tf::isTensorFlowValue(Type ty) {
  return classifyTensorFlowValue(ty) != TFValueKind::Nope;
}

/// Returns true if the specified type is a TensorFlow value or an tuple or
/// struct of such.
bool tf::isTensorFlowValueOrAggregate(Type ty) {
  if (isTensorFlowValue(ty))
    return true;
  if (auto *tupleTy = ty->getAs<TupleType>())
    return llvm::all_of(tupleTy->getElementTypes(),
      [](Type eltTy) {
        return isTensorFlowValueOrAggregate(eltTy);
      });
  if (auto *structDecl = ty->getStructOrBoundGenericStruct())
    return llvm::all_of(structDecl->getStoredProperties(),
      [](VarDecl *member) {
        return isTensorFlowValueOrAggregate(member->getType());
      });
  return false;
}

bool tf::flattenTensorFlowValueAggregate(Type ty,
                                         SmallVectorImpl<Type> &result) {
  if (isTensorFlowValue(ty)) {
    result.push_back(ty);
    return true;
  }
  if (auto *tupleTy = ty->getAs<TupleType>())
    return llvm::all_of(tupleTy->getElementTypes(),
        [&](Type eltTy) {
          return flattenTensorFlowValueAggregate(eltTy, result);
        });
  if (auto *structDecl = ty->getStructOrBoundGenericStruct()) {
    auto *module = structDecl->getModuleContext();
    return llvm::all_of(structDecl->getStoredProperties(),
        [&](VarDecl *member) {
          auto subMap = ty->getMemberSubstitutionMap(module, member);
          auto eltTy = member->getType().subst(subMap);
          return flattenTensorFlowValueAggregate(eltTy, result);
        });
  }
  // Terminal type is not a TensorFlow value or an aggregate of TensorFlow
  // values, so it fails.
  result.clear();
  return false;
}

/// Return true if the specified type contains a TensorFlow value type that
/// will be exposed after deabstraction.
/// If `checkHigherOrderFunctions`, also check for a function-typed `ty`, if its
/// parameter of result contains any TensorFlow value type.
bool TypeContainsTensorFlowValue::containsTensorFlowValue(
    Type ty, bool checkHigherOrderFunctions) {
  // If this type literally is a value type, then yep, we contain it.  This is
  // the base case.
  if (isTensorFlowValue(ty))
    return true;

  // Deabstraction flattens tuples, so if a tuple contains any tensor values,
  // then the tuple itself does.
  if (auto *tuple = ty->getAs<TupleType>()) {
    for (auto &elt : tuple->getElements())
      if (containsTensorFlowValue(elt.getType(), checkHigherOrderFunctions))
        return true;
    return false;
  }

  // Deabstraction scalarizes structs.
  if (auto *st = ty->getAs<StructType>())
    return structContainsTensorFlowValue(st->getDecl());

  // Deabstractions binds specialized generic structs.  Check if either the
  // struct itself or one of the generic arguments contains a tensor value.
  if (auto *bgst = ty->getAs<BoundGenericStructType>()) {
    // Check the generic arguments.
    for (auto arg : bgst->getGenericArgs())
      if (containsTensorFlowValue(arg, checkHigherOrderFunctions))
        return true;

    return structContainsTensorFlowValue(bgst->getDecl());
  }

  // Handle still-generic types that may contain a tensor value.
  if (auto *ugst = ty->getAs<UnboundGenericType>())
    if (auto *decl = dyn_cast<StructDecl>(ugst->getDecl()))
      return structContainsTensorFlowValue(decl);

  if (checkHigherOrderFunctions) {
    if (auto *fnType = ty->getAs<SILFunctionType>()) {
      for (auto &result : fnType->getResults())
        if (containsTensorFlowValue(result.getType(),
                                    checkHigherOrderFunctions))
          return true;

      for (auto &param : fnType->getParameters())
        if (containsTensorFlowValue(param.getType(), checkHigherOrderFunctions))
          return true;
    }
  }

  // Otherwise we have a class or some other type that is opaque to
  // deabstraction.
  return false;
}

/// Determine whether the given struct contains a TensorFlow value type, caching
/// the result.
bool TypeContainsTensorFlowValue::
structContainsTensorFlowValue(StructDecl *decl) {
  auto it = declContainsTensorFlowValue.find(decl);
  if (it != declContainsTensorFlowValue.end())
    return it->second;

  bool hasTensorFlowValue = false;
  for (auto p : decl->getStoredProperties())
    if (containsTensorFlowValue(p->getType(),
                                /*checkHigherOrderFunctions*/ false)) {
      hasTensorFlowValue = true;
      break;
    }

  return declContainsTensorFlowValue[decl] = hasTensorFlowValue;
}
