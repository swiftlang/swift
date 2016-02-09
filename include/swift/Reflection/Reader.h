
#ifndef SWIFT_REFLECTION_READER_H
#define SWIFT_REFLECTION_READER_H

#include <cstdint>
#include <memory>

class BufferImpl;

namespace swift {
namespace reflection {

using CopyFunction = size_t (*)(uintptr_t Source, void *Dest, size_t Size);

/// An abstract impl interface for getting the head of what may be a buffer
/// copied from an external address space.
class BufferImpl {
public:
  virtual const void *getPointer() = 0;
  virtual ~BufferImpl() {}
};

/// A trivial wrapper for pointers in the current address space.
template <typename T>
class InternalBuffer final : public BufferImpl {
  const T *Source;

public:
  InternalBuffer(const T *Source) : Source(Source) {}
  virtual const void *getPointer() override {
    return reinterpret_cast<const void *>(Source);
  }
};

/// A buffer of data copied out of an external address space.
template <typename T>
class ExternalBuffer final : public BufferImpl {
  uint8_t Buf[sizeof(T)] = {0};

public:
  virtual const void *getPointer() override {
    return reinterpret_cast<const void *>(Buf);
  }
};

/// An abstraction over a pointer which may be in another
/// address space.
///
/// These will always be backed by real data, but it is
/// important not to indirect twice out of this type.
/// Ask the MemoryReader to read each pointer. For example:
/// Buffer<MyStruct> MS = Reader.read(APointer);
/// int i = MS->i; // OK.
///
/// If MyStruct has a nested MyStruct *, you read it like so:
/// Buffer<MyStruct> MS = Reader.read(MS->nestedMyStruct);
template <typename T>
class Buffer {
  friend class MemoryReader;

  std::unique_ptr<BufferImpl> Impl;

  Buffer() : Impl(nullptr) {}

  Buffer(std::unique_ptr<BufferImpl> Impl)
    : Impl(std::move(Impl)) {}

public:
  const T& operator*() {
      return *reinterpret_cast<const T*>(Impl->getPointer());
  }

  const T* operator->() {
      return reinterpret_cast<const T*>(Impl->getPointer());
  }

  operator bool() const {
    return bool(Impl);
  }
};

class MemoryReader {
  CopyFunction copy;

public:
  MemoryReader() : copy(nullptr) {}
  MemoryReader(CopyFunction copy) : copy(copy) {}

  template <typename T>
  Buffer<T> read(const T *Source) {
    using External = ExternalBuffer<T>;
    using Internal = InternalBuffer<T>;
    if (copy) {
      auto external = std::unique_ptr<External>(new External());
      auto bytesCopied = copy(reinterpret_cast<uintptr_t>(Source),
                              const_cast<void *>(external->getPointer()),
                              sizeof(T));

      if (bytesCopied != sizeof(T))
        return Buffer<T>();

      return Buffer<T>(std::move(external));
    } else {
      auto internal = new Internal(Source);
      return Buffer<T>(std::unique_ptr<Internal>(internal));
    }
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_READER_H

