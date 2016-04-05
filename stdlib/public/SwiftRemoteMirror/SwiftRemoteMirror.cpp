#include "swift/Reflection/ReflectionContext.h"
#include "swift/SwiftRemoteMirror/SwiftRemoteMirror.h"

using namespace swift;
using namespace reflection;

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
  auto Context = new ReflectionContext<InProcess>(Reader);
  return reinterpret_cast<SwiftReflectionContextRef>(Context);
}

void swift_reflection_destroyReflectionContext(SwiftReflectionContextRef ContextRef) {
  auto Context = reinterpret_cast<ReflectionContext<InProcess> *>(ContextRef);
  delete Context;
}

void swift_reflection_clearCaches(SwiftReflectionContextRef ContextRef) {
  auto Context = reinterpret_cast<ReflectionContext<InProcess> *>(ContextRef);
  Context->clear();
}
