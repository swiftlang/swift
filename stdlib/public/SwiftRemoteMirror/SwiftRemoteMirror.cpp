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

