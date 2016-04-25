//===--- SwiftRemoteMirror.cpp - C wrapper for Reflection API -------------===//
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

#include "swift/Reflection/ReflectionContext.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Remote/CMemoryReader.h"
#include "swift/SwiftRemoteMirror/SwiftRemoteMirror.h"

using namespace swift;
using namespace swift::reflection;
using namespace swift::remote;

using NativeReflectionContext
  = ReflectionContext<External<RuntimeTarget<sizeof(uintptr_t)>>>;

SwiftReflectionContextRef
swift_reflection_createReflectionContext(void *reader_context,
                                         PointerSizeFunction getPointerSize,
                                         SizeSizeFunction getSizeSize,
                                         ReadBytesFunction readBytes,
                                         GetStringLengthFunction getStringLength,
                                         GetSymbolAddressFunction getSymbolAddress) {
  MemoryReaderImpl ReaderImpl {
    reader_context,
    getPointerSize,
    getSizeSize,
    readBytes,
    getStringLength,
    getSymbolAddress
  };

  auto Reader = std::make_shared<CMemoryReader>(ReaderImpl);
  auto Context
    = new ReflectionContext<External<RuntimeTarget<sizeof(uintptr_t)>>>(Reader);
  return reinterpret_cast<SwiftReflectionContextRef>(Context);
}

void swift_reflection_destroyReflectionContext(SwiftReflectionContextRef ContextRef) {
  auto Context = reinterpret_cast<ReflectionContext<InProcess> *>(ContextRef);
  delete Context;
}

void
swift_reflection_addReflectionInfo(SwiftReflectionContextRef ContextRef,
                                   const char *ImageName,
                                   swift_reflection_section_t fieldmd,
                                   swift_reflection_section_t assocty,
                                   swift_reflection_section_t builtin,
                                   swift_reflection_section_t typeref,
                                   swift_reflection_section_t reflstr) {
  ReflectionInfo Info {
    ImageName,
    FieldSection(fieldmd.Begin, fieldmd.End),
    AssociatedTypeSection(assocty.Begin, assocty.End),
    BuiltinTypeSection(builtin.Begin, builtin.End),
    GenericSection(typeref.Begin, typeref.End),
    GenericSection(reflstr.Begin, reflstr.End)
  };
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  Context->addReflectionInfo(Info);
}

swift_typeref_t
swift_reflection_typeRefForMetadata(SwiftReflectionContextRef ContextRef,
                                    uintptr_t metadata) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  auto TR = Context->readTypeFromMetadata(metadata);
  return reinterpret_cast<swift_typeref_t>(TR);
}

swift_typeref_t
swift_reflection_genericArgumentOfTypeRef(swift_typeref_t OpaqueTypeRef,
                                          unsigned Index) {
  auto TR = reinterpret_cast<const TypeRef *>(OpaqueTypeRef);

  if (auto BG = dyn_cast<BoundGenericTypeRef>(TR)) {
    auto &Params = BG->getGenericParams();
    assert(Index < Params.size());
    return reinterpret_cast<swift_typeref_t>(Params[Index]);
  }
  return 0;
}

unsigned
swift_reflection_genericArgumentCountOfTypeRef(swift_typeref_t OpaqueTypeRef) {
  auto TR = reinterpret_cast<const TypeRef *>(OpaqueTypeRef);

  if (auto BG = dyn_cast<BoundGenericTypeRef>(TR)) {
    auto &Params = BG->getGenericParams();
    return Params.size();
  }
  return 0;
}

swift_typeinfo_t
swift_reflection_infoForTypeRef(SwiftReflectionContextRef ContextRef,
                                swift_typeref_t OpaqueTypeRef) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  auto TR = reinterpret_cast<const TypeRef *>(OpaqueTypeRef);
  auto TI = Context->getTypeInfo(TR);

  if (TI == nullptr) {
    return {
      SWIFT_UNKNOWN,
      0,
      0,
      0,
      0
    };
  }

  swift_layout_kind_t Kind;
  unsigned NumFields = 0;

  switch (TI->getKind()) {
  case TypeInfoKind::Builtin:
    Kind = SWIFT_BUILTIN;
    break;
  case TypeInfoKind::Record: {
    auto *RecordTI = cast<RecordTypeInfo>(TI);
    switch (RecordTI->getRecordKind()) {
    case RecordKind::Tuple:
      Kind = SWIFT_TUPLE;
      break;
    case RecordKind::Struct:
      Kind = SWIFT_STRUCT;
      break;
    case RecordKind::ThickFunction:
      Kind = SWIFT_THICK_FUNCTION;
      break;
    }
    NumFields = RecordTI->getNumFields();
    break;
  }
  case TypeInfoKind::Reference: {
    auto *ReferenceTI = cast<ReferenceTypeInfo>(TI);
    switch (ReferenceTI->getReferenceKind()) {
    case ReferenceKind::Strong:
      Kind = SWIFT_STRONG_REFERENCE;
      break;
    case ReferenceKind::Unowned:
      Kind = SWIFT_UNOWNED_REFERENCE;
      break;
    case ReferenceKind::Weak:
      Kind = SWIFT_WEAK_REFERENCE;
      break;
    case ReferenceKind::Unmanaged:
      Kind = SWIFT_UNMANAGED_REFERENCE;
      break;
    }
  }
  }

  return {
    Kind,
    TI->getSize(),
    TI->getAlignment(),
    TI->getStride(),
    NumFields
  };
}

swift_childinfo_t
swift_reflection_infoForChild(SwiftReflectionContextRef ContextRef,
                              swift_typeref_t OpaqueTypeRef,
                              unsigned Index) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  auto TR = reinterpret_cast<const TypeRef *>(OpaqueTypeRef);

  auto *RecordTI = cast<RecordTypeInfo>(Context->getTypeInfo(TR));
  auto FieldInfo = RecordTI->getFields()[Index];

  return {
    FieldInfo.Name.c_str(),
    FieldInfo.Offset,
    reinterpret_cast<swift_typeref_t>(FieldInfo.TR),
  };
}
