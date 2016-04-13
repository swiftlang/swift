//===--- SwiftIndexing.h --------------------------------------------------===//
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

#ifndef LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTINDEXING_H
#define LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTINDEXING_H

enum class SymbolKind {
  Unknown,

  Module,
  ClangModule, // FIXME: collapse into Module and use a separate Language field.
  SourceFile,

  Enum,
  Struct,
  Class,
  Protocol,
  Extension,

  TypeAlias,
  AssociatedType,
  GenericTypeParam,

  Function,
  PrefixOperator,
  PostfixOperator,
  InfixOperator,

  LocalVariable,
  GlobalVariable,
  ParamVariable,

  Accessor,
  Subscript,
  EnumElement,

  InstanceMethod,
  ClassMethod,
  StaticMethod,
  InstanceProperty,
  ClassProperty,
  StaticProperty,

  Constructor,
  Destructor,
};

enum class SymbolSubKind {
  None,

  AccessorGetter,
  AccessorSetter,
  AccessorWillSet,
  AccessorDidSet,
  AccessorMaterializeForSet,
  AccessorAddressor,
  AccessorMutableAddressor,

  ExtensionOfStruct,
  ExtensionOfClass,
  ExtensionOfEnum,
  ExtensionOfProtocol,
};

#endif // LLVM_SOURCEKIT_LIB_SWIFTLANG_SWIFTINDEXING_H
