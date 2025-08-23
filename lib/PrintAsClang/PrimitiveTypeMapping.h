//===--- PrimitiveTypeMapping.h - Mapping primitive types -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTASCLANG_PRIMITIVETYPEMAPPING_H
#define SWIFT_PRINTASCLANG_PRIMITIVETYPEMAPPING_H

#include "swift/AST/Decl.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {

class ASTContext;
class TypeDecl;

/// Provides a mapping from Swift's primitive types to C / Objective-C / C++
/// primitive types.
///
/// Certain types have mappings that differ in different language modes.
/// For example, Swift's `Int` maps to `NSInteger` for Objective-C declarations,
/// but to something like `intptr_t` or `swift::Int` for C and C++ declarations.
class PrimitiveTypeMapping {
public:
  struct ClangTypeInfo {
    StringRef name;
    bool canBeNullable;
  };

  /// Returns the Objective-C type name and nullability for the given Swift
  /// primitive type declaration, or \c None if no such type name exists.
  std::optional<ClangTypeInfo> getKnownObjCTypeInfo(const TypeDecl *typeDecl);

  /// Returns the C type name and nullability for the given Swift
  /// primitive type declaration, or \c None if no such type name exists.
  std::optional<ClangTypeInfo> getKnownCTypeInfo(const TypeDecl *typeDecl);

  /// Returns the C++ type name and nullability for the given Swift
  /// primitive type declaration, or \c None if no such type name exists.
  std::optional<ClangTypeInfo> getKnownCxxTypeInfo(const TypeDecl *typeDecl);

private:
  void initialize(ASTContext &ctx);

  struct FullClangTypeInfo {
    // The Objective-C name of the Swift type.
    StringRef objcName;
    // The C name of the Swift type.
    std::optional<StringRef> cName;
    // The C++ name of the Swift type.
    std::optional<StringRef> cxxName;
    bool canBeNullable;
  };

  FullClangTypeInfo *getMappedTypeInfoOrNull(const TypeDecl *typeDecl);

  /// Associate Swift types to their {name, nullability} in foreign languages.
  ///
  /// This is populated on first use with a list of known Swift types that are
  /// translated directly by the ObjC printer instead of structurally, allowing
  /// it to do things like map 'Int' to 'NSInteger' and 'Float' to 'float'.
  /// In some sense it's the reverse of the ClangImporter's MappedTypes.def.
  /// Must be kept aligned with BuiltinMappedTypes.def.
  llvm::DenseMap<TypeDecl*, FullClangTypeInfo> mappedTypeNames;
};

} // end namespace swift

#endif
