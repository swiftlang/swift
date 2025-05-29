//===--- ReferenceTypeInfo.h - Supplement for reference types ---*- C++ -*-===//
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
// This file defines ReferenceTypeInfo, which supplements the
// FixedTypeInfo interface for types with reference semantics.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_REFERENCETYPEINFO_H
#define SWIFT_IRGEN_REFERENCETYPEINFO_H

#include "LoadableTypeInfo.h"

namespace swift {
namespace irgen {

class TypeConverter;
  
/// An abstract class designed for use when implementing a type
/// that has reference semantics.
class ReferenceTypeInfo : public LoadableTypeInfo {
protected:
  // FIXME: Get spare bits for pointers from a TargetInfo-like structure.
  ReferenceTypeInfo(llvm::Type *type, Size size, SpareBitVector spareBits,
                    Alignment align, IsTriviallyDestroyable_t pod = IsNotTriviallyDestroyable)
    : LoadableTypeInfo(type, size, spareBits, align, pod, IsCopyable,
                       IsFixedSize, IsABIAccessible, SpecialTypeInfoKind::Reference)
  {}

public:
  /// Strongly retains a value.
  virtual void strongRetain(IRGenFunction &IGF, Explosion &in,
                            Atomicity atomicity) const = 0;

  /// Strongly releases a value.
  virtual void strongRelease(IRGenFunction &IGF, Explosion &in,
                             Atomicity atomicity) const = 0;

  virtual ReferenceCounting getReferenceCountingType() const {
    llvm_unreachable("not supported");
  }

#define REF_STORAGE_HELPER(Name) \
  virtual const TypeInfo *create##Name##StorageType(TypeConverter &TC, \
                                                    bool isOptional) const = 0;
#define NEVER_LOADABLE_CHECKED_REF_STORAGE_HELPER(Name, name) \
  virtual void name##TakeStrong(IRGenFunction &IGF, Address addr, \
                                 Explosion &out, bool isOptional) const = 0; \
  virtual void name##LoadStrong(IRGenFunction &IGF, Address addr, \
                                 Explosion &out, bool isOptional) const = 0; \
  virtual void name##Init(IRGenFunction &IGF, Explosion &in, \
                           Address dest, bool isOptional) const = 0; \
  virtual void name##Assign(IRGenFunction &IGF, Explosion &in, \
                             Address dest, bool isOptional) const = 0;
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE_HELPER(Name, name) \
  virtual void strongRetain##Name(IRGenFunction &IGF, Explosion &in, \
                                  Atomicity atomicity) const = 0; \
  virtual void strongRetain##Name##Release(IRGenFunction &IGF, \
                                           Explosion &in, \
                                           Atomicity atomicity) const = 0; \
  virtual void name##Retain(IRGenFunction &IGF, Explosion &in, \
                             Atomicity atomicity) const = 0; \
  virtual void name##Release(IRGenFunction &IGF, Explosion &in, \
                              Atomicity atomicity) const = 0;
#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, name, ...) \
  NEVER_LOADABLE_CHECKED_REF_STORAGE_HELPER(Name, name) \
  REF_STORAGE_HELPER(Name)
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, name, ...) \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE_HELPER(Name, name) \
  REF_STORAGE_HELPER(Name)
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, name, ...) \
  NEVER_LOADABLE_CHECKED_REF_STORAGE_HELPER(Name, name) \
  ALWAYS_LOADABLE_CHECKED_REF_STORAGE_HELPER(Name, name) \
  REF_STORAGE_HELPER(Name)
#define UNCHECKED_REF_STORAGE(Name, name, ...) \
  REF_STORAGE_HELPER(Name)
#include "swift/AST/ReferenceStorage.def"
#undef REF_STORAGE_HELPER
#undef NEVER_LOADABLE_CHECKED_REF_STORAGE_HELPER
#undef ALWAYS_LOADABLE_CHECKED_REF_STORAGE_HELPER

  static bool classof(const ReferenceTypeInfo *type) { return true; }
  static bool classof(const TypeInfo *type) {
    return type->getSpecialTypeInfoKind() == SpecialTypeInfoKind::Reference;
  }
};

}
}

#endif
