#include "swift/Reflection/ReflectionContext.h"
#include "swift/SwiftRemoteMirror/SwiftRemoteMirror.h"

using namespace swift;
using namespace reflection;

using NativeReflectionContext
  = ReflectionContext<External<RuntimeTarget<sizeof(uintptr_t)>>>;

SwiftReflectionContextRef
swift_reflection_createReflectionContext(PointerSizeFunction getPointerSize,
                                         SizeSizeFunction getSizeSize,
                                         ReadBytesFunction readBytes,
                                         GetStringLengthFunction getStringLength,
                                         GetSymbolAddressFunction getSymbolAddress) {
  MemoryReaderImpl ReaderImpl {
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
                                   swift_reflection_section_t typeref,
                                   swift_reflection_section_t reflstr,
                                   swift_reflection_section_t assocty) {
  ReflectionInfo Info {
    ImageName,
    FieldSection(fieldmd.Begin, fieldmd.End),
    AssociatedTypeSection(assocty.Begin, assocty.End),
    GenericSection(reflstr.Begin, reflstr.End),
    GenericSection(typeref.Begin, typeref.End)
  };
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  Context->addReflectionInfo(Info);
}

void swift_reflection_clearCaches(SwiftReflectionContextRef ContextRef) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  Context->clear();
}

swift_typeref_t
swift_reflection_typeRefForMetadata(SwiftReflectionContextRef ContextRef,
                                    uintptr_t metadata) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  auto TR = Context->getTypeRef(metadata);
  return reinterpret_cast<swift_typeref_t>(TR.get());
}

swift_typeref_t
swift_reflection_genericArgumentOfTypeRef(swift_typeref_t OpaqueTypeRef,
                                          unsigned Index) {
  auto TR = reinterpret_cast<TypeRef *>(OpaqueTypeRef);

  if (auto BG = dyn_cast<BoundGenericTypeRef>(TR)) {
    auto &Params = BG->getGenericParams();
    if (Index < Params.size()) {
      return reinterpret_cast<swift_typeref_t>(Params[Index].get());
    }
  }
  return 0;
}

swift_typeinfo_t
swift_reflection_infoForTypeRef(SwiftReflectionContextRef ContextRef,
                                swift_typeref_t OpaqueTypeRef) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  auto TR = reinterpret_cast<TypeRef *>(OpaqueTypeRef);
  return Context->getInfoForTypeRef(TR);
}

swift_fieldinfo_t
swift_reflection_infoForField(SwiftReflectionContextRef ContextRef,
                              swift_typeref_t OpaqueTypeRef,
                              unsigned Index) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  auto TR = reinterpret_cast<TypeRef *>(OpaqueTypeRef);
  return Context->getInfoForField(TR, Index);
}

