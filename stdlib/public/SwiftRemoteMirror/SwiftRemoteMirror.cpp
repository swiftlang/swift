#include "swift/Reflection/ReflectionContext.h"
#include "swift/Remote/CMemoryReader.h"
#include "swift/SwiftRemoteMirror/SwiftRemoteMirror.h"

using namespace swift;
using namespace swift::reflection;
using namespace swift::remote;

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
    if (Index < Params.size()) {
      return reinterpret_cast<swift_typeref_t>(Params[Index]);
    }
  }
  return 0;
}

swift_typeinfo_t
swift_reflection_infoForTypeRef(SwiftReflectionContextRef ContextRef,
                                swift_typeref_t OpaqueTypeRef) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  auto TR = reinterpret_cast<const TypeRef *>(OpaqueTypeRef);
  return Context->getInfoForTypeRef(TR);
}

swift_childinfo_t
swift_reflection_infoForChild(SwiftReflectionContextRef ContextRef,
                              swift_typeref_t OpaqueTypeRef,
                              unsigned Index) {
  auto Context = reinterpret_cast<NativeReflectionContext *>(ContextRef);
  auto TR = reinterpret_cast<const TypeRef *>(OpaqueTypeRef);
  return Context->getInfoForChild(TR, Index);
}

