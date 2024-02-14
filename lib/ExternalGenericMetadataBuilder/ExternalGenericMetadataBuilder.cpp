//===------------------ ExternalGenericMetadataBuilder.cpp ----------------===//
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

#include "ExternalGenericMetadataBuilder.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/Demangling/Demangle.h"
#include "swift/RemoteInspection/Records.h"
#include "swift/Runtime/GenericMetadataBuilder.h"
#include "swift/Runtime/LibPrespecialized.h"
#include "swift/Runtime/PrebuiltStringMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Object/Binary.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/JSON.h"

#include <cstdint>
#include <initializer_list>
#include <stdarg.h>
#include <string.h>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "swift/Demangling/Demangler.h"

namespace swift {

// Logging macro. Currently we always enable logs, but we'll want to make this
// conditional eventually. LOG_ENABLED can be used to gate work that's needed
// for log statements but not for anything else.
enum class LogLevel {
  None = 0,
  Warning = 1,
  Info = 2,
  Detail = 3,
};

#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"
#define LOG(level, fmt, ...)                                                   \
  do {                                                                         \
    if (level <= getLogLevel())                                                \
      fprintf(stderr, "%s:%d:%s: " fmt "\n", METADATA_BUILDER_LOG_FILE_NAME,   \
              __LINE__, __func__, __VA_ARGS__);                                \
  } while (0)

static const char *nodeKindString(swift::Demangle::Node::Kind k) {
  switch (k) {
#define NODE(ID)                                                               \
  case Node::Kind::ID:                                                         \
    return #ID;
#include "swift/Demangling/DemangleNodes.def"
  }
  return "Demangle::Node::Kind::???";
}

// An external runtime target that preserves the static type of pointees.
template <typename Runtime>
struct TypedExternal {
  //  using StoredPointer = typename Runtime::StoredPointer;
  struct StoredPointer {
    typename Runtime::StoredPointer value;

    StoredPointer(MetadataKind kind)
        : value(static_cast<typename Runtime::StoredPointer>(kind)) {}

    static bool isNull(StoredPointer ptr) { return ptr.value == 0; }
  };
  using StoredSignedPointer = typename Runtime::StoredSignedPointer;
  using StoredSize = typename Runtime::StoredSize;
  using StoredPointerDifference = typename Runtime::StoredPointerDifference;

  static constexpr size_t PointerSize = Runtime::PointerSize;
  static constexpr bool ObjCInterop = Runtime::ObjCInterop;
  const StoredPointer PointerValue;

  template <typename T, typename Integer, bool Nullable = true,
            typename TagType = void>
  struct TypedInteger {
    Integer value;
  };

  template <typename T, typename Integer, bool Nullable = true>
  struct TypedSignedInteger {
    Integer SignedValue;
  };

  template <typename T>
  using Pointer = TypedInteger<T, StoredPointer>;

  template <typename T>
  using SignedPointer = TypedSignedInteger<T, StoredPointer>;

  template <typename T, bool Nullable = false>
  using FarRelativeDirectPointer = TypedInteger<T, StoredPointer, Nullable>;

  struct RelativeIndirectablePointerTag {};

  template <typename T, bool Nullable = false>
  using RelativeIndirectablePointer =
      TypedInteger<T, int32_t, Nullable, RelativeIndirectablePointerTag>;

  template <typename T, bool Nullable = true>
  using RelativeDirectPointer = TypedInteger<T, int32_t, Nullable>;

  template <typename T, bool Nullable = true, typename Offset = int32_t>
  using CompactFunctionPointer = TypedInteger<T, int32_t, Nullable>;

  template <unsigned discriminator>
  struct ValueWitnessFunctionPointer {
    StoredPointer pointer;
  };

  StoredPointer
  getStrippedSignedPointer(const StoredSignedPointer pointer) const {
    return swift_ptrauth_strip(pointer);
  }
};

using ExternalRuntime32 =
    swift::TypedExternal<swift::WithObjCInterop<swift::RuntimeTarget<4>>>;
using ExternalRuntime64 =
    swift::TypedExternal<swift::WithObjCInterop<swift::RuntimeTarget<8>>>;

// Declare a specialized version of the value witness types that use a wrapper
// on the functions that captures the ptrauth discriminator.
template <>
class TargetValueWitnessTypes<ExternalRuntime64> {
public:
  using StoredPointer = typename ExternalRuntime64::StoredPointer;

#define WANT_ALL_VALUE_WITNESSES
#define DATA_VALUE_WITNESS(lowerId, upperId, type)
#define FUNCTION_VALUE_WITNESS(lowerId, upperId, returnType, paramTypes)       \
  typedef ExternalRuntime64::ValueWitnessFunctionPointer<                      \
      SpecialPointerAuthDiscriminators::upperId>                               \
      lowerId;
#define MUTABLE_VALUE_TYPE TargetPointer<ExternalRuntime64, OpaqueValue>
#define IMMUTABLE_VALUE_TYPE ConstTargetPointer<ExternalRuntime64, OpaqueValue>
#define MUTABLE_BUFFER_TYPE TargetPointer<ExternalRuntime64, ValueBuffer>
#define IMMUTABLE_BUFFER_TYPE ConstTargetPointer<ExternalRuntime64, ValueBuffer>
#define TYPE_TYPE ConstTargetPointer<ExternalRuntime64, Metadata>
#define SIZE_TYPE StoredSize
#define INT_TYPE int
#define UINT_TYPE unsigned
#define VOID_TYPE void
#include "swift/ABI/ValueWitness.def"

  // Handle the data witnesses explicitly so we can use more specific
  // types for the flags enums.
  typedef size_t size;
  typedef size_t stride;
  typedef TargetValueWitnessFlags<typename ExternalRuntime64::StoredSize> flags;
  typedef uint32_t extraInhabitantCount;
};

enum class PtrauthKey : int8_t {
  None = -1,
  IA = 0,
  IB = 1,
  DA = 2,
  DB = 3,
};

template <typename T>
inline MetadataKind getEnumeratedMetadataKind(T kind,
                                              decltype(kind.value) dummy = 0) {
  return getEnumeratedMetadataKind(kind.value);
}

template <typename... Args>
NodePointer wrapNode(Demangler &demangler, NodePointer node, Args... kinds) {
  std::initializer_list<Node::Kind> kindsList{kinds...};

  for (auto iter = std::rbegin(kindsList); iter != std::rend(kindsList);
       iter++) {
    auto wrapper = demangler.createNode(*iter);
    wrapper->addChild(node, demangler);
    node = wrapper;
  }
  return node;
}

std::string getErrorString(llvm::Error error) {
  std::string errorString;
  handleAllErrors(std::move(error), [&](const llvm::ErrorInfoBase &E) {
    errorString = E.message();
  });
  return errorString;
}

struct FixupTarget {
  // This value in `library` means the target is the containing library.
  static constexpr int selfLibrary = 0;

  int library;

  // Virtual address or symbol name.
  std::variant<uint64_t, llvm::StringRef> target;
};

struct MachOFile {
  std::optional<std::unique_ptr<llvm::MemoryBuffer>> memoryBuffer;

  std::unique_ptr<llvm::object::MachOObjectFile> objectFile;
  std::string path;

  // With the leading '_'.
  llvm::StringMap<llvm::object::SymbolRef> symbols;

  // Guaranteed that getName returns a value.
  std::vector<llvm::object::SymbolRef> namedSymbolsSortedByAddress;
  std::vector<llvm::object::SymbolRef> exportedSymbolsSortedByAddress;
  std::vector<const char *> libraryNames;

  std::unordered_map<uint64_t, FixupTarget> fixups;

  MachOFile(std::optional<std::unique_ptr<llvm::MemoryBuffer>> &&memoryBuffer,
            std::unique_ptr<llvm::object::MachOObjectFile> &&objectFile,
            const std::string &path)
      : memoryBuffer(std::move(memoryBuffer)),
        objectFile(std::move(objectFile)), path(path) {}
};

template <typename Runtime>
class ReaderWriter;

template <typename Runtime>
class ExternalGenericMetadataBuilderContext {
  // Structs that provide the ptrauth info for pointers to specific tyeps.
  template <typename Target>
  struct PtrauthInfo;

  template <>
  struct PtrauthInfo<const TargetValueTypeDescriptor<Runtime>> {
    static constexpr PtrauthKey key = PtrauthKey::DA;
    static constexpr bool addressDiversified = true;
    static constexpr unsigned discriminator =
        SpecialPointerAuthDiscriminators::TypeDescriptor;
  };

  template <>
  struct PtrauthInfo<const TargetValueWitnessTable<Runtime>> {
    static constexpr PtrauthKey key = PtrauthKey::DA;
    static constexpr bool addressDiversified = true;
    static constexpr unsigned discriminator =
        SpecialPointerAuthDiscriminators::ValueWitnessTable;
  };

  struct Atom;

  struct FileTarget {
    MachOFile *file;
    uint64_t addressInFile;
  };

  struct AtomTarget {
    Atom *atom;
    uint64_t offset;
  };

  struct PointerTarget {
    template <typename T>
    class Buffer;

    unsigned offset;
    unsigned size;

    PtrauthKey ptrauthKey;
    bool addressDiversified;
    unsigned discriminator;

    std::variant<FileTarget, AtomTarget> fileOrAtom;
  };

  struct Atom {
    std::string name;
    std::vector<char> buffer;
    std::vector<PointerTarget> pointerTargetsSorted;
  };

  std::vector<std::unique_ptr<Atom>> atoms;

  Atom *allocateAtom(size_t size) {
    auto atom = atoms.emplace_back(new Atom).get();
    atom->buffer.resize(size);
    return atom;
  }

public:
  using ContextDescriptor = TargetContextDescriptor<Runtime>;
  using ExtensionContextDescriptor = TargetExtensionContextDescriptor<Runtime>;
  using ModuleContextDescriptor = TargetModuleContextDescriptor<Runtime>;
  using ProtocolDescriptor = TargetProtocolDescriptor<Runtime>;
  using TypeContextDescriptor = TargetTypeContextDescriptor<Runtime>;
  using Metadata = TargetMetadata<Runtime>;
  using StoredPointer = typename Runtime::StoredPointer;

  template <typename T>
  using Pointer = typename Runtime::template Pointer<T>;
  template <typename T>
  using SignedPointer = typename Runtime::template SignedPointer<T>;

  template <typename T>
  class Buffer {
    template <typename U>
    friend class Buffer;

    // Copy constructor, but done as a static function to make errors less
    // confusing when outside code accidentally tries to initialize a Buffer<T>
    // with a Buffer<U>.
    template <typename U>
    static Buffer<T> createFrom(const Buffer<U> &other) {
      Buffer<T> result{other.context};
      result.section = other.section;
      result.ptr = (T *)(void *)other.ptr;
      result.file = other.file;
      result.atom = other.atom;
      return result;
    }

  protected:
    template <typename U>
    BuilderErrorOr<Buffer<const U>>
    resolveVirtualAddressInFile(MachOFile *targetFile, uint64_t vmAddr) {
      auto section = context->findSectionInFile(targetFile, vmAddr);
      if (!section)
        return BuilderError("No section contained resolved address %#" PRIx64,
                            vmAddr);

      intptr_t targetOffsetInSection = vmAddr - section->getAddress();
      auto targetSectionContents = section->getContents();
      if (!targetSectionContents) {
        return BuilderError(
            "Failed to get target section contents: %s",
            getErrorString(targetSectionContents.takeError()).c_str());
      }

      intptr_t target =
          (intptr_t)targetSectionContents->data() + targetOffsetInSection;

      return Buffer<const U>{context, targetFile, *section, (U *)target};
    }

    template <typename U>
    BuilderErrorOr<Buffer<const U>> resolveVirtualAddress(uint64_t vmAddr) {
      return resolveVirtualAddressInFile<U>(file, vmAddr);
    }

    template <typename U, typename Integer, bool Nullable>
    BuilderErrorOr<Buffer<const U>> resolveRelativePointer(const void *ptr,
                                                           Integer value) {
      if (Nullable && value == 0)
        return {{Buffer<const U>::null(context)}};

      auto pointerVirtualAddress = getVirtualAddress(ptr);
      if (!pointerVirtualAddress)
        return *pointerVirtualAddress.getError();
      uintptr_t targetVirtualAddress = *pointerVirtualAddress + value;
      return resolveVirtualAddress<U>(targetVirtualAddress);
    }

    BuilderErrorOr<const char *> getLibraryName(int ordinal) {
      if (ordinal > 0 && (size_t)ordinal <= file->libraryNames.size())
        return file->libraryNames[ordinal - 1];
      return BuilderError("ordinal %d is out of bounds", ordinal);
    }

  public:
    ExternalGenericMetadataBuilderContext *context;
    MachOFile *file = nullptr;
    llvm::object::SectionRef section;

    T *ptr;
    Atom *atom = nullptr;

    static Buffer null(ExternalGenericMetadataBuilderContext *context) {
      return Buffer{context};
    }

    Buffer(ExternalGenericMetadataBuilderContext *context)
        : context(context), ptr(nullptr) {}

    explicit Buffer(ExternalGenericMetadataBuilderContext *context,
                    MachOFile *file, llvm::object::SectionRef section, T *ptr)
        : context(context), file(file), section(section), ptr(ptr) {}

    explicit Buffer(ExternalGenericMetadataBuilderContext *context,
                    MachOFile *file, llvm::object::SectionRef section,
                    llvm::StringRef str)
        : context(context), file(file), section(section),
          ptr(reinterpret_cast<T *>(str.data())) {}

    operator bool() { return ptr != nullptr; }

    bool isNull() { return ptr == nullptr; }

    Buffer<const T> toConst() { return Buffer<const T>::createFrom(*this); }

    template <typename U>
    Buffer<U> cast() {
      return Buffer<U>::createFrom(*this);
    }

    Buffer<T> offsetBy(ptrdiff_t offset) const {
      auto newBuffer = *this;
      newBuffer.ptr = (T *)((uintptr_t)ptr + offset);
      return newBuffer;
    }

    BuilderErrorOr<uint64_t> getVirtualAddress(const void *ptr) {
      auto sectionContents = section.getContents();
      if (!sectionContents) {
        return BuilderError(
            "Failed to get section contents: %s",
            getErrorString(sectionContents.takeError()).c_str());
      }
      intptr_t sectionStartInMemory = (intptr_t)sectionContents->data();
      intptr_t pointerAddress = (intptr_t)ptr;
      uint64_t pointerVirtualAddress =
          section.getAddress() + pointerAddress - sectionStartInMemory;
      return pointerVirtualAddress;
    }

    // Get the virtual address of ptr, or ~0 if there's an error. Meant for
    // logging purposes.
    uint64_t getAddress() {
      if (auto *address = getVirtualAddress(ptr).getValue())
        return *address;
      return ~0;
    }

    template <typename U, bool Nullable>
    BuilderErrorOr<Buffer<const U>> resolvePointer(
        const TargetRelativeDirectPointer<Runtime, U, Nullable> *ptr) {
      return resolveRelativePointer<U, decltype(ptr->value), Nullable>(
          ptr, ptr->value);
    }

    template <typename U, bool Nullable>
    BuilderErrorOr<Buffer<const U>> resolvePointer(
        const TargetRelativeIndirectablePointer<Runtime, U, Nullable> *ptr) {
      bool isDirect = !(ptr->value & 1);
      auto pointerValue = ptr->value & ~static_cast<decltype(ptr->value)>(1);

      auto directBuffer =
          resolveRelativePointer<U, decltype(ptr->value), Nullable>(
              ptr, pointerValue);
      if (directBuffer.getError() || isDirect)
        return directBuffer;

      // Indirect case: relative pointer to absolute pointer to value.
      auto indirectAbsolutePointer =
          directBuffer.getValue()->template cast<const StoredPointer>();
      return indirectAbsolutePointer.template resolvePointer<U>(
          indirectAbsolutePointer.ptr);
    }

    template <typename U, bool Nullable, typename IntTy, typename Offset>
    BuilderErrorOr<Buffer<const U>> resolvePointer(
        const RelativeDirectPointerIntPair<U, IntTy, Nullable, Offset> *ptr) {
      return resolveRelativePointer<U, Offset, Nullable>(ptr, ptr->getOffset());
    }

    template <typename U = char>
    BuilderErrorOr<Buffer<const U>> resolvePointer(const StoredPointer *ptr) {
      if (isNull())
        return BuilderError("Tried to resolve pointer %p in null buffer", ptr);

      if (!file) {
        return reinterpret_cast<WritableData<T> *>(this)
            ->template resolveWritableDataPointer<U>(ptr);
      }
      assert(section.getObject());

      auto pointerVirtualAddress = getVirtualAddress(ptr);
      if (!pointerVirtualAddress)
        return *pointerVirtualAddress.getError();

      auto found = file->fixups.find(*pointerVirtualAddress);
      if (found != file->fixups.end()) {
        auto target = std::get<1>(*found);
        if (auto *vmAddr = std::get_if<uint64_t>(&target.target)) {
          if (target.library > 0) {
            const char *libName = "ordinal too large";
            if (auto ptr = getLibraryName(target.library).getValue())
              libName = *ptr;
            BuilderError("Found fixup at %#" PRIx64
                         " with address target %#" PRIx64
                         " and library target %s",
                         *pointerVirtualAddress, *vmAddr, libName);
          }

          return resolveVirtualAddress<U>(*vmAddr);
        }

        auto symbolName = std::get<llvm::StringRef>(target.target);

        MachOFile *targetFile;
        if (target.library > 0) {
          auto targetLibraryName = getLibraryName(target.library);
          if (!targetLibraryName)
            return *targetLibraryName.getError();
          auto found = context->machOFilesByPath.find(*targetLibraryName);
          if (found == context->machOFilesByPath.end())
            return BuilderError("Symbol referenced unknown library '%s'",
                                *targetLibraryName);
          targetFile = found->getValue();
        } else if (target.library == FixupTarget::selfLibrary) {
          targetFile = file;
        } else {
          return BuilderError(
              "Can't resolve pointer with fixup target ordinal %d",
              target.library);
        }

        auto foundSymbol = targetFile->symbols.find(symbolName);
        if (foundSymbol == targetFile->symbols.end()) {
          const char *targetLibraryName;
          if (auto *ptr = getLibraryName(target.library).getValue())
            targetLibraryName = *ptr;
          else
            targetLibraryName = file->path.c_str();
          return BuilderError(
              "Symbol referenced unknown symbol '%s' in library '%s'",
              symbolName.str().c_str(), targetLibraryName);
        }
        return context->readerWriter->template getSymbolPointer<U>(
            symbolName, foundSymbol->getValue(), targetFile);
      }

      // We didn't find any fixups for this spot. It might just be NULL.
      if (ptr->value == 0)
        return {Buffer<const U>::null(context)};

      return BuilderError("Attempted to resolve pointer at %#" PRIx64
                          " with contents %#" PRIx64
                          " but no fixup exists at this location",
                          *pointerVirtualAddress, (uint64_t)ptr->value);
    }

    template <typename U = char>
    BuilderErrorOr<Buffer<const U>>
    resolvePointer(const SignedPointer<U *> *ptr) {
      return resolvePointer<U>(reinterpret_cast<const StoredPointer *>(ptr));
    }

    template <typename U = char>
    BuilderErrorOr<Buffer<const U>> resolvePointer(const Pointer<U> *ptr) {
      return resolvePointer<U>(reinterpret_cast<const StoredPointer *>(ptr));
    }

    template <typename U>
    BuilderErrorOr<Buffer<const char>> resolveFunctionPointer(const U *ptr) {
      static_assert(sizeof(*ptr) == sizeof(StoredPointer));
      return resolvePointer(reinterpret_cast<const StoredPointer *>(ptr));
    }

    template <typename U, bool nullable>
    BuilderErrorOr<Buffer<const char>> resolveFunctionPointer(
        const TargetCompactFunctionPointer<Runtime, U, nullable> *ptr) {
      auto result = resolvePointer(ptr);
      if (auto *error = result.getError())
        return *error;
      return result.getValue()->template cast<const char>();
    }
  };

  template <typename T>
  class WritableData : public Buffer<T> {
    /// Check that the given pointer lies within memory of this data object.
    void checkPtr(void *toCheck) {
      assert((uintptr_t)toCheck - (uintptr_t)this->atom->buffer.data() <
             this->atom->buffer.size());
    }

    template <typename U>
    void writePointerImpl(void *where, Buffer<U> value,
                          PtrauthKey ptrauthKey = PtrauthKey::None,
                          bool addressDiversified = true,
                          unsigned discriminator = 0) {
      if (!value) {
        memset(where, 0, sizeof(StoredPointer));
        return;
      }

      // Write some arbitrary nonzero data so that null checks work with != 0.
      // This value won't make it out into the serialized data.
      memset(where, 0x5a, sizeof(StoredPointer));

      PointerTarget target = {};

      target.offset = (uintptr_t)where - (uintptr_t)this->atom->buffer.data();
      assert(target.offset + sizeof(StoredPointer) <=
             this->atom->buffer.size());

      target.size = sizeof(StoredPointer);

      target.ptrauthKey = ptrauthKey;
      target.addressDiversified = addressDiversified;
      target.discriminator = discriminator;

      if (auto *file = value.file) {
        auto contents = value.section.getContents();
        if (!contents) {
          fprintf(stderr,
                  "section.getContents should never fail, but it did: %s",
                  getErrorString(contents.takeError()).c_str());
          abort();
        }

        auto addressInFile = (uintptr_t)value.ptr -
                             (uintptr_t)contents->data() +
                             value.section.getAddress();
        target.fileOrAtom = FileTarget{value.file, addressInFile};
      } else {
        auto offsetInValue =
            (uintptr_t)value.ptr - (uintptr_t)value.atom->buffer.data();
        target.fileOrAtom = AtomTarget{value.atom, offsetInValue};
      }

      auto &targets = this->atom->pointerTargetsSorted;
      auto compare = [](const PointerTarget &a, const PointerTarget &b) {
        return a.offset < b.offset;
      };
      auto insertionPoint =
          std::upper_bound(targets.begin(), targets.end(), target, compare);

      // Check to see if there's already a target at this offset, and overwrite
      // it if so.
      if (insertionPoint > targets.begin()) {
        auto prevPoint = insertionPoint - 1;
        if (prevPoint->offset == target.offset) {
          *prevPoint = target;
          return;
        }
      }
      targets.insert(insertionPoint, target);

      return;
    }

  public:
    WritableData(ExternalGenericMetadataBuilderContext *context, Atom *atom)
        : Buffer<T>(context, {}, {},
                    reinterpret_cast<T *>(atom->buffer.data())) {
      this->atom = atom;
    }

    void setName(const std::string &str) { this->atom->name = str; }

    template <typename U = char>
    BuilderErrorOr<Buffer<const U>>
    resolveWritableDataPointer(const StoredPointer *ptr) {
      unsigned offset = (uintptr_t)ptr - (uintptr_t)this->atom->buffer.data();
      assert(offset + sizeof(StoredPointer) <= this->atom->buffer.size());

      auto &targets = this->atom->pointerTargetsSorted;
      auto compare = [](const PointerTarget &a, const unsigned &b) {
        return a.offset < b;
      };
      auto found =
          std::lower_bound(targets.begin(), targets.end(), offset, compare);
      if (found == targets.end() || found->offset != offset) {
        if (ptr->value == 0)
          return Buffer<const U>::null(this->context);
        return BuilderError(
            "Asked to resolve non-null pointer %#" PRIx64
            " at offset %u in writable data with no target at that offset",
            (uint64_t)ptr->value, offset);
      }

      if (auto *fileTarget = std::get_if<FileTarget>(&found->fileOrAtom))
        return this->template resolveVirtualAddressInFile<U>(
            fileTarget->file, fileTarget->addressInFile);

      auto atomTarget = std::get<AtomTarget>(found->fileOrAtom);
      WritableData<const U> buffer{this->context, atomTarget.atom};
      return buffer.offsetBy(atomTarget.offset);
    }

    template <typename U>
    void writePointer(Pointer<U> *where, Buffer<U> value) {
      checkPtr(where);
      where->value.value = ~(uintptr_t)value.ptr;

      writePointerImpl(where, value);
    }

    // SignedPointer is templated on the pointer type, not the pointee type like
    // the other pointer templates.
    template <typename U>
    void writePointer(SignedPointer<U *> *where, Buffer<U> value) {
      checkPtr(where);
      where->SignedValue.value = ~(uintptr_t)value.ptr;

      writePointerImpl(where, value, PtrauthInfo<U>::key,
                       PtrauthInfo<U>::addressDiversified,
                       PtrauthInfo<U>::discriminator);
    }

    template <typename U>
    void writePointer(StoredPointer *where, Buffer<U> value) {
      checkPtr(where);
      where->value = ~(uintptr_t)value.ptr;

      writePointerImpl(where, value);
    }

    void writeFunctionPointer(void *where, Buffer<const char> target) {
      writePointer(reinterpret_cast<StoredPointer *>(where), target);
    }

    template <unsigned discriminator>
    void writeFunctionPointer(
        ExternalRuntime64::ValueWitnessFunctionPointer<discriminator> *where,
        Buffer<const char> target) {
      checkPtr(where);

      writePointerImpl(where, target, PtrauthKey::IA, true, discriminator);
    }
  };

  ExternalGenericMetadataBuilderContext() {
    readerWriter.reset(new ReaderWriter<Runtime>{this});
  }

  ExternalGenericMetadataBuilderContext(
      const ExternalGenericMetadataBuilderContext &) = delete;
  ExternalGenericMetadataBuilderContext &
  operator=(const ExternalGenericMetadataBuilderContext &) = delete;

  LogLevel getLogLevel() {
    return logLevel;
  }

  void setLogLevel(int level) {
    logLevel = LogLevel(level);
  }

  template <typename T>
  WritableData<T> allocate(size_t size) {
    auto atom = allocateAtom(size);
    return WritableData<T>{this, atom};
  }

  template <typename T>
  WritableData<T> allocateArray(size_t count) {
    return allocate<T>(count * sizeof(T));
  }

  void setArch(const char *arch) {
    this->arch = arch;
    this->usePtrauth = this->arch == "arm64e";
  }

  void setNamesToBuild(const std::vector<std::string> &names) {
    this->mangledNamesToBuild = names;
  }

  BuilderErrorOr<NodePointer>
  buildDemanglingForContext(Buffer<const ContextDescriptor> descriptorBuffer,
                            Demangler &dem);

  llvm::Error
  addImage(std::unique_ptr<llvm::object::Binary> &&binary,
           std::optional<std::unique_ptr<llvm::MemoryBuffer>> &&memoryBuffer,
           const std::string &path);
  llvm::Error addImageAtPath(const std::string &path);
  llvm::Error addImageInMemory(const void *start, uint64_t length,
                               const std::string &path);
  BuilderErrorOr<std::monostate> addImagesInPath(std::string path);

  BuilderErrorOr<Buffer<const Metadata>>
  metadataForNode(swift::Demangle::NodePointer Node);

  std::optional<std::pair<MachOFile *, llvm::object::SymbolRef>>
  findSymbol(llvm::StringRef name, MachOFile *searchFile);
  std::optional<llvm::object::SectionRef> findSectionInFile(MachOFile *file,
                                                            uint64_t address);
  std::optional<MachOFile *> findPointerInFiles(const void *ptr);

  void build();
  void writeOutput(llvm::json::OStream &J, unsigned platform,
                   const std::string &platformVersion);
  void logDescriptorMap();

private:
  using Builder = GenericMetadataBuilder<ReaderWriter<Runtime>>;
  using ConstructedMetadata = typename Builder::ConstructedMetadata;

  void cacheDescriptor(Buffer<const ContextDescriptor> descriptorBuffer);

  void addImage(MachOFile *file);
  void populateMachOSymbols(MachOFile *file);
  void populateMachOFixups(MachOFile *file);

  void readMachOSections(MachOFile *file);
  BuilderErrorOr<std::string> _mangledNominalTypeNameForBoundGenericNode(Demangle::NodePointer BoundGenericNode);
  BuilderErrorOr<std::optional<typename Builder::ConstructedMetadata>>
  constructMetadataForMangledTypeName(llvm::StringRef typeName);
  BuilderErrorOr<std::optional<typename Builder::ConstructedMetadata>>
  constructMetadataForNode(swift::Demangle::NodePointer Node);

  Buffer<char> serializeMetadataMapTable(void);

  template <typename SymbolCallback>
  void writeAtomContentsJSON(llvm::json::OStream &J, const Atom &atom,
                             const SymbolCallback &symbolCallback);
  void writeDylibsJSON(
      llvm::json::OStream &J,
      std::unordered_map<MachOFile *, std::unordered_set<std::string>>
          &symbolReferences);
  void writeJSONSerialization(llvm::json::OStream &J, unsigned platform,
                              const std::string &platformVersion);

  // The current log level.
  LogLevel logLevel = LogLevel::None;

  // The architecture being targeted.
  std::string arch;

  // Does this target use pointer authentication?
  bool usePtrauth = false;

  // The readerWriter and builder helper objects.
  std::unique_ptr<ReaderWriter<Runtime>> readerWriter;
  std::unique_ptr<Builder> builder;

  // The mangled names we're building metadata for.
  std::vector<std::string> mangledNamesToBuild;

  // Map from standardized mangled names to the corresponding type descriptors.
  std::unordered_map<std::string, Buffer<const TypeContextDescriptor>>
      mangledNominalTypeDescriptorMap;

  // Map from mangled type names to the built metadata.
  std::unordered_map<std::string, const typename Builder::ConstructedMetadata>
      builtMetadataMap;

  // All of the MachOFile objects we're working with.
  std::vector<std::unique_ptr<MachOFile>> machOFiles;

  // A map from paths (install names) to MachOFile objects.
  llvm::StringMap<MachOFile *> machOFilesByPath;

  // A map from symbol names (including _ prefix) to file and SymbolRef.
  llvm::StringMap<std::pair<MachOFile *, llvm::object::SymbolRef>> allSymbols;

  swift::Demangle::Context demangleCtx;
};

template <typename RuntimeT>
class ReaderWriter {
  ExternalGenericMetadataBuilderContext<RuntimeT> *context;

  bool sectionContainsAddress(llvm::object::SectionRef section, uint64_t addr) {
    return section.getAddress() <= addr &&
           addr < section.getAddress() + section.getSize();
  }

  std::optional<llvm::object::SymbolRef>
  getNearestSymbol(MachOFile *file, uint64_t addr,
                   const std::vector<llvm::object::SymbolRef> &in) {
    auto comparator = [&](uint64_t addr, const llvm::object::SymbolRef &sym) {
      auto symAddr = sym.getAddress();
      if (!symAddr)
        return true;
      return addr < *symAddr;
    };
    auto foundSymbol = std::upper_bound(in.begin(), in.end(), addr, comparator);

    // We found the first symbol with address greater than our target. Back
    // up one to find the symbol with address less than or equal to our
    // target.
    if (foundSymbol != in.begin())
      foundSymbol--;

    if (foundSymbol == in.end())
      return {};

    // Make sure the symbol is in the same section as the target.
    auto section = foundSymbol->getSection();
    if (!section)
      return {};

    if (!sectionContainsAddress(*section.get(), addr)) {
      // Try the following symbol and use it if it's in the same section as the
      // target.
      foundSymbol++;
      if (foundSymbol == in.end())
        return {};
      auto section = foundSymbol->getSection();
      if (!section)
        return {};
      if (!sectionContainsAddress(*section.get(), addr))
        return {};
    }

    return *foundSymbol;
  }

public:
  using Runtime = RuntimeT;

  using MetadataContext = ExternalGenericMetadataBuilderContext<Runtime>;

  using Metadata = typename MetadataContext::Metadata;
  using GenericArgument =
      typename MetadataContext::template Buffer<const Metadata>;

  template <typename T>
  using Buffer = typename MetadataContext::template Buffer<T>;

  template <typename T>
  using WritableData = typename MetadataContext::template WritableData<T>;

  struct SymbolInfo {
    std::string symbolName;
    std::string libraryName;
    uint64_t pointerOffset;
  };

  class ResolveToDemangling {
    ReaderWriter &readerWriter;
    Demangle::Demangler &dem;

    LogLevel getLogLevel() {
      return readerWriter.getLogLevel();
    }

  public:
    ResolveToDemangling(ReaderWriter &readerWriter, Demangle::Demangler &dem)
        : readerWriter(readerWriter), dem(dem) {}

    Demangle::NodePointer operator()(Demangle::SymbolicReferenceKind kind,
                                     Demangle::Directness directness,
                                     int32_t offset, const void *base) {
      auto maybeBuffer = readerWriter.getBufferForPointerInFile<
          TargetRelativeDirectPointer<Runtime, char>>(base);
      if (auto *error = maybeBuffer.getError()) {
        LOG(LogLevel::Warning,
            "Failed to find buffer for symbolic reference at %p offset "
            "%" PRId32,
            base, offset);
        return nullptr;
      }

      auto buffer = maybeBuffer.getValue();
      auto target = buffer->resolvePointer(buffer->ptr);
      if (auto *error = target.getError()) {
        LOG(LogLevel::Warning,
            "Failed to resolve symbolic reference at %p offset %" PRId32, base,
            offset);
        return nullptr;
      }

      if (directness == Directness::Indirect) {
        auto castBuffer =
            target.getValue()
                ->template cast<typename MetadataContext::StoredPointer>();
        target = castBuffer.resolvePointer(castBuffer.ptr);
        if (auto *error = target.getError()) {
          LOG(LogLevel::Warning,
              "Failed to resolve indirect symbolic reference at %p offset "
              "%" PRId32,
              base, offset);
          return nullptr;
        }
      }

      switch (kind) {
      case Demangle::SymbolicReferenceKind::Context: {
        auto contextBuffer =
            target.getValue()
                ->template cast<const TargetContextDescriptor<Runtime>>();
        auto demangleNode =
            readerWriter.context->buildDemanglingForContext(contextBuffer, dem);
        if (auto *error = demangleNode.getError()) {
          LOG(LogLevel::Warning,
              "Failed to build demangling for symbolic reference at %p offset "
              "%" PRId32,
              base, offset);
          return nullptr;
        }
        return *demangleNode.getValue();
      }
      default:
        LOG(LogLevel::Warning,
            "Don't know how to handle symbolic reference kind %u",
            (unsigned)kind);
        return nullptr;
      }
    }
  };

  ReaderWriter(ExternalGenericMetadataBuilderContext<Runtime> *context)
      : context(context) {}

  bool isLoggingEnabled() { return getLogLevel() >= LogLevel::Info; }

  LogLevel getLogLevel() {
    return context->getLogLevel();
  }

  SWIFT_FORMAT(5, 6)
  void log(const char *filename, unsigned line, const char *function,
           const char *fmt, ...) {
    if (!isLoggingEnabled())
      return;

    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s:%u:%s: ", filename, line, function);
    vfprintf(stderr, fmt, args);
    fputs("\n", stderr);

    va_end(args);
  }

  template <typename T = char>
  BuilderErrorOr<Buffer<const T>> getSymbolPointer(const char *name) {
    return getSymbolPointer<T>(llvm::StringRef{name});
  }

  template <typename T = char>
  BuilderErrorOr<Buffer<const T>>
  getSymbolPointer(llvm::StringRef name, MachOFile *searchFile = nullptr) {
    auto result = context->findSymbol(name, searchFile);
    if (!result)
      return BuilderError("Could not find symbol '%s'", name.str().c_str());

    auto [file, symbol] = *result;
    return getSymbolPointer<T>(name, symbol, file);
  }

  template <typename T = char>
  BuilderErrorOr<Buffer<const T>>
  getSymbolPointer(llvm::StringRef name, llvm::object::SymbolRef symbol,
                   MachOFile *file) {
    auto expectedAddress = symbol.getAddress();
    if (!expectedAddress) {
      return BuilderError("Could not get address of symbol '%s': %s",
                          name.str().c_str(),
                          getErrorString(expectedAddress.takeError()).c_str());
    }
    auto address = expectedAddress.get();
    auto expectedSection = symbol.getSection();
    if (!expectedSection) {
      return BuilderError("Could not get section of symbol '%s': %s",
                          name.str().c_str(),
                          getErrorString(expectedSection.takeError()).c_str());
    }
    auto section = expectedSection.get();
    if (section == file->objectFile->section_end())
      return BuilderError("Could not get section of symbol '%s' (%#" PRIx64
                          "), it is undefined or absolute",
                          name.str().c_str(), address);
    auto sectionAddress = section->getAddress();

    if (address < sectionAddress)
      return BuilderError("Symbol '%s' has address %#" PRIx64
                          " < section address %#" PRIx64,
                          name.str().c_str(), address, sectionAddress);

    auto expectedSectionContents = section->getContents();
    if (!expectedSectionContents)
      return BuilderError(
          "Could not get section contents for symbol '%s': %s",
          name.str().c_str(),
          getErrorString(expectedSectionContents.takeError()).c_str());
    auto sectionContents = expectedSectionContents.get();

    auto delta = address - sectionAddress;
    if (delta >= sectionContents.size())
      return BuilderError(
          "Symbol '%s' has address %#" PRIx64
          " past the end of its section, start address %#" PRIx64 " length %zu",
          name.str().c_str(), address, sectionAddress, sectionContents.size());

    auto symbolContents = sectionContents.drop_front(delta);

    Buffer<const T> buffer{context, file, *section, symbolContents};
    return {buffer};
  }

  template <typename T>
  BuilderErrorOr<Buffer<const T>> getBufferForPointerInFile(const void *ptr) {
    auto maybeFile = context->findPointerInFiles(ptr);
    if (!maybeFile)
      return BuilderError("Pointer %p is not in any loaded mach-o file", ptr);
    auto file = *maybeFile;

    auto target = (uintptr_t)ptr;
    bool hadErrors = false;
    for (auto &section : file->objectFile->sections()) {
      auto contents = section.getContents();
      if (!contents) {
        consumeError(contents.takeError());
        hadErrors = true;
        continue;
      }

      auto start = (uintptr_t)contents->begin();
      auto end = (uintptr_t)contents->end();
      if (start <= target && target < end)
        return Buffer<const T>{context, file, section, (const T *)ptr};
    }

    return BuilderError(
        "Pointer %p is in file %s but is not in any section%s", ptr,
        file->path.c_str(),
        hadErrors ? " (errors occurred when fetching section contents)" : "");
  }

  BuilderErrorOr<NodePointer> substituteGenericParameters(
      NodePointer node, NodePointer metadataMangleNode,
      WritableData<FullMetadata<Metadata>> containingMetadataBuffer) {
    if (node->getKind() == Demangle::Node::Kind::DependentGenericParamType) {
      auto depth = node->getChild(0)->getIndex();
      auto index = node->getChild(1)->getIndex();

      unsigned foundDepth = 0;
      NodePointer cursor = metadataMangleNode;
      while (cursor) {
        switch (cursor->getKind()) {
        case Node::Kind::BoundGenericClass:
        case Node::Kind::BoundGenericStructure:
        case Node::Kind::BoundGenericEnum:
        case Node::Kind::BoundGenericProtocol:
        case Node::Kind::BoundGenericTypeAlias:
        case Node::Kind::BoundGenericFunction:
        case Node::Kind::BoundGenericOtherNominalType:
          if (foundDepth == depth) {
            auto typeList = cursor->getChild(1);
            if (!typeList)
              return BuilderError(
                  "Requested generic parameter at depth=%" PRIu64
                  " index=%" PRIu64
                  ", BoundGeneric node does not have two children",
                  depth, index);
            if (typeList->getKind() != Node::Kind::TypeList)
              return BuilderError(
                  "Requested generic parameter at depth=%" PRIu64
                  " index=%" PRIu64
                  ", child 1 of BoundGeneric is %s, not TypeList",
                  depth, index, nodeKindString(typeList->getKind()));

            auto child = typeList->getChild(index);
            if (!child)
              return BuilderError(
                  "Requested generic parameter at depth=%" PRIu64
                  " index=%" PRIu64
                  ", but bound generic TypeList only has %zu children",
                  depth, index, typeList->getNumChildren());

            return child;
          }
          foundDepth++;
          cursor = cursor->getFirstChild();
          break;

        default:
          cursor = cursor->getFirstChild();
          break;
        }
      }

      return BuilderError("Requested generic parameter at depth=%" PRIu64
                          " index=%" PRIu64 ", but maximum depth was %u",
                          depth, index, foundDepth);
    }

    for (size_t i = 0; i < node->getNumChildren(); i++) {
      auto child = node->getChild(i);
      auto newChild = substituteGenericParameters(child, metadataMangleNode,
                                                  containingMetadataBuffer);
      if (!newChild)
        return *newChild.getError();
      if (*newChild != child)
        node->replaceChild(i, *newChild);
    }

    return node;
  }

  BuilderErrorOr<Buffer<const Metadata>> getTypeByMangledName(
      WritableData<FullMetadata<Metadata>> containingMetadataBuffer,
      NodePointer metadataMangleNode, llvm::StringRef mangledTypeName) {
    Demangle::Demangler dem;

    auto node =
        dem.demangleType(mangledTypeName, ResolveToDemangling(*this, dem));
    LOG(LogLevel::Detail, "%s", getNodeTreeAsString(node).c_str());

    auto substituted = substituteGenericParameters(node, metadataMangleNode,
                                                   containingMetadataBuffer);
    if (!substituted)
      return *substituted.getError();

    LOG(LogLevel::Detail, "%s", getNodeTreeAsString(*substituted).c_str());

    return context->metadataForNode(*substituted);
  }

  /// Get info about the symbol corresponding to the given buffer. If no
  /// information can be retrieved, the result is filled with "<unknown>"
  /// strings and a 0 offset.
  template <typename T>
  SymbolInfo getSymbolInfo(Buffer<T> buffer) {
    SymbolInfo result = {"<unknown>", "<unknown>", 0};
    if (buffer.file) {
      result.libraryName = buffer.file->path;
      if (auto *address = buffer.getVirtualAddress(buffer.ptr).getValue()) {
        auto symbol = getNearestSymbol(buffer.file, *address);
        if (symbol) {
          if (auto name = symbol->getName())
            result.symbolName = *name;
          result.pointerOffset = *address;
        }
      }
    }
    return result;
  }

  // Find the nearest exported symbol before the target, if possible. Finds the
  // first symbol after the target if there are none before. If the file
  // contains no symbols, returns an empty SymbolRef.
  std::optional<llvm::object::SymbolRef>
  getNearestExportedSymbol(MachOFile *file, uint64_t addr) {
    return getNearestSymbol(file, addr, file->exportedSymbolsSortedByAddress);
  }

  // Find the nearest symbol before the target, if possible. Finds the first
  // symbol after the target if there are none before. If the file contains no
  // symbols, returns an empty SymbolRef.
  std::optional<llvm::object::SymbolRef> getNearestSymbol(MachOFile *file,
                                                          uint64_t addr) {
    return getNearestSymbol(file, addr, file->namedSymbolsSortedByAddress);
  }

  template <typename T>
  WritableData<T> allocate(size_t size) {
    return context->template allocate<T>(size);
  }
};

// Produces the standard mangled name for a prespecialized metadata that we'll
// use as the key to the metadata map table.
static BuilderErrorOr<std::string>
_standardMangledNameForNode(Demangle::NodePointer typeNode,
                            Demangler &demangler) {
  // Wrap the type in a global node to match the mangling we get from the input.
  if (typeNode->getKind() != Node::Kind::Global) {
    typeNode = wrapNode(demangler, typeNode, Node::Kind::Global);
  }

  auto resolver = [](SymbolicReferenceKind kind,
                     const void *ref) -> NodePointer { abort(); };
  auto mangling = Demangle::mangleNode(typeNode, resolver, demangler);

  if (!mangling.isSuccess())
    return BuilderError("Failed to mangle node: %s",
                        getNodeTreeAsString(typeNode).c_str());

  return mangling.result().str();
}

static std::string getInstallName(llvm::object::MachOObjectFile *objectFile) {
  for (auto &command : objectFile->load_commands()) {
    if (command.C.cmd == llvm::MachO::LC_ID_DYLIB) {
      auto dylibCommand = objectFile->getDylibIDLoadCommand(command);
      return command.Ptr + dylibCommand.dylib.name;
    }
  }

  return "";
}

template <typename Runtime>
BuilderErrorOr<NodePointer>
ExternalGenericMetadataBuilderContext<Runtime>::buildDemanglingForContext(
    Buffer<const ContextDescriptor> descriptorBuffer, Demangler &dem) {
  NodePointer node = nullptr;

  // Walk up the context tree.
  llvm::SmallVector<Buffer<const ContextDescriptor>, 8> descriptorPath;
  {
    Buffer<const ContextDescriptor> parent = descriptorBuffer;
    while (parent) {
      descriptorPath.push_back(parent);

      // Temporary shenanigans to resolve the weird pointer type until we figure
      // out the right template whatnot to make it work properly.
      auto parentFieldPointer = &parent.ptr->Parent;
      auto parentFieldPointerCast =
          (const TargetRelativeIndirectablePointer<Runtime, ContextDescriptor>
               *)parentFieldPointer;
      auto newParent = parent.resolvePointer(parentFieldPointerCast);
      if (!newParent)
        return *newParent.getError();
      parent = *newParent;
    }
  }

  for (auto component : llvm::reverse(descriptorPath)) {
    switch (auto kind = component.ptr->getKind()) {
    case ContextDescriptorKind::Module: {
      if (node != nullptr)
        return BuilderError("Building demangling for context, found module not "
                            "at the top level");

      auto moduleDescriptor =
          component.template cast<ModuleContextDescriptor>();
      auto name = moduleDescriptor.resolvePointer(&moduleDescriptor.ptr->Name);
      if (!name)
        return *name.getError();
      node = dem.createNode(Node::Kind::Module, name->ptr);
      break;
    }

    case ContextDescriptorKind::Extension: {
      auto extension = component.template cast<ExtensionContextDescriptor>();
      auto mangledExtendedContextBuffer =
          extension.resolvePointer(&extension.ptr->ExtendedContext);
      if (!mangledExtendedContextBuffer)
        return *mangledExtendedContextBuffer.getError();
      auto mangledExtendedContext = Demangle::makeSymbolicMangledNameStringRef(
          mangledExtendedContextBuffer->ptr);
      typename ReaderWriter<Runtime>::ResolveToDemangling resolver{
          *readerWriter, dem};
      auto selfType = dem.demangleType(mangledExtendedContext, resolver);

      if (selfType->getKind() == Node::Kind::Type)
        selfType = selfType->getFirstChild();

      if (selfType->getKind() == Node::Kind::BoundGenericEnum ||
          selfType->getKind() == Node::Kind::BoundGenericStructure ||
          selfType->getKind() == Node::Kind::BoundGenericClass ||
          selfType->getKind() == Node::Kind::BoundGenericOtherNominalType) {
        // TODO: Use the unsubstituted type if we can't handle the
        // substitutions yet.
        selfType = selfType->getChild(0)->getChild(0);
      }

      auto extNode = dem.createNode(Node::Kind::Extension);
      extNode->addChild(node, dem);
      extNode->addChild(selfType, dem);

      // TODO: Turn the generic signature into a demangling as the third
      // generic argument.

      node = extNode;
      break;
    }

    case ContextDescriptorKind::Protocol: {
      auto protocol = component.template cast<ProtocolDescriptor>();
      auto name = protocol.resolvePointer(&protocol.ptr->Name);
      if (!name)
        return *name.getError();

      auto protocolNode = dem.createNode(Node::Kind::Protocol);
      protocolNode->addChild(node, dem);
      auto nameNode = dem.createNode(Node::Kind::Identifier, name->ptr);
      protocolNode->addChild(nameNode, dem);

      node = protocolNode;
      break;
    }

    default:
      // Form a type context demangling for type contexts.
      if (auto type = llvm::dyn_cast<TypeContextDescriptor>(component.ptr)) {
        if (type->getTypeContextDescriptorFlags().hasImportInfo())
          return BuilderError("Don't know how to build demangling for type "
                              "context descriptor with import info");

        Node::Kind nodeKind;
        switch (kind) {
        case ContextDescriptorKind::Class:
          nodeKind = Node::Kind::Class;
          break;
        case ContextDescriptorKind::Struct:
          nodeKind = Node::Kind::Structure;
          break;
        case ContextDescriptorKind::Enum:
          nodeKind = Node::Kind::Enum;
          break;
        default:
          // We don't know about this kind of type. Use an "other type" mangling
          // for it.
          nodeKind = Node::Kind::OtherNominalType;
          break;
        }

        auto name = component.resolvePointer(&type->Name);
        if (!name)
          return *name.getError();

        auto typeNode = dem.createNode(nodeKind);
        typeNode->addChild(node, dem);
        auto nameNode = dem.createNode(Node::Kind::Identifier, name->ptr);
        typeNode->addChild(nameNode, dem);

        node = typeNode;
        break;
      }

      return BuilderError(
          "Don't know how to build demangling for descriptor kind %hhu",
          static_cast<uint8_t>(kind));
    }
  }

  return wrapNode(dem, node, Node::Kind::Type);
}

template <typename Runtime>
llvm::Error ExternalGenericMetadataBuilderContext<Runtime>::addImage(
    std::unique_ptr<llvm::object::Binary> &&binary,
    std::optional<std::unique_ptr<llvm::MemoryBuffer>> &&memoryBuffer,
    const std::string &path) {
  if (isa<llvm::object::MachOObjectFile>(binary.get())) {
    auto objectFile = cast<llvm::object::MachOObjectFile>(std::move(binary));
    auto filetype = objectFile->getHeader().filetype;
    // Do we want to support bundles?
    if (filetype != llvm::MachO::MH_DYLIB &&
        filetype != llvm::MachO::MH_EXECUTE)
      return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                     "Unsupported mach-o filetype %" PRIu32,
                                     filetype);
    LOG(LogLevel::Info, "Loaded %p from %s", objectFile.get(), path.c_str());
    MachOFile *file =
        new MachOFile{std::move(memoryBuffer), std::move(objectFile), path};
    addImage(file);
    return llvm::Error::success();
  }

  if (auto universal =
          dyn_cast<llvm::object::MachOUniversalBinary>(binary.get())) {
    auto objectForArch = universal->getMachOObjectForArch(arch);
    if (!objectForArch)
      return objectForArch.takeError();
    auto objectFile = objectForArch.get().get();
    LOG(LogLevel::Info, "Loaded %p from %s", objectFile, path.c_str());
    MachOFile *file = new MachOFile{std::move(memoryBuffer),
                                    std::move(objectForArch.get()), path};
    addImage(file);
    return llvm::Error::success();
  }

  return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                 "Unknown binary type.");
}

template <typename Runtime>
llvm::Error ExternalGenericMetadataBuilderContext<Runtime>::addImageAtPath(
    const std::string &path) {
  LOG(LogLevel::Info, "Adding %s", path.c_str());

  auto binaryOwner = llvm::object::createBinary(path);
  if (!binaryOwner) {
    return binaryOwner.takeError();
  }
  auto [binary, buffer] = binaryOwner->takeBinary();
  return addImage(std::move(binary), std::optional(std::move(buffer)), path);
}

template <typename Runtime>
llvm::Error ExternalGenericMetadataBuilderContext<Runtime>::addImageInMemory(
    const void *start, uint64_t length, const std::string &path) {
  LOG(LogLevel::Info, "Adding %s in memory", path.c_str());
  llvm::StringRef stringRef{reinterpret_cast<const char *>(start), length};
  llvm::MemoryBufferRef bufferRef{stringRef, {}};
  auto binary = llvm::object::createBinary(bufferRef);
  if (!binary)
    return binary.takeError();
  return addImage(std::move(binary.get()), {}, path);
}

/// Add images in the given path. Returns an error if there was a filesystem
/// error when iterating over the directory. Errors reading individual images
/// are NOT returned, as it's assumed that there may be non-dylib content in the
/// directory as well.
template <typename Runtime>
BuilderErrorOr<std::monostate>
ExternalGenericMetadataBuilderContext<Runtime>::addImagesInPath(
    std::string path) {
  auto forEachFile = [&](std::string filepath) {
    auto error = addImageAtPath(filepath);
    handleAllErrors(std::move(error), [&](const llvm::ErrorInfoBase &E) {
      fprintf(stderr, "Could not read dylib at %s: %s\n", filepath.c_str(),
              E.message().c_str());
    });
  };
  auto result = enumerateFilesInPath(path, forEachFile);
  if (!result)
    return *result.getError();

  return {{}};
}

template <typename Runtime>
BuilderErrorOr<
    typename ExternalGenericMetadataBuilderContext<Runtime>::template Buffer<
        const typename ExternalGenericMetadataBuilderContext<
            Runtime>::Metadata>>
ExternalGenericMetadataBuilderContext<Runtime>::metadataForNode(
    swift::Demangle::NodePointer node) {
  Demangler demangler;

  auto symbolNode =
      wrapNode(demangler, node, Node::Kind::Global, Node::Kind::TypeMetadata);

  auto resolver = [](SymbolicReferenceKind kind,
                     const void *ref) -> NodePointer { abort(); };
  auto symbolMangling = Demangle::mangleNode(symbolNode, resolver, demangler);
  if (!symbolMangling.isSuccess()) {
    return BuilderError("symbol mangling failed with code %u",
                        symbolMangling.error().code);
  }
  LOG(LogLevel::Detail, "symbol mangling: %s",
      symbolMangling.result().str().c_str());

  auto symbolName = symbolMangling.result();
  auto maybeSymbol =
      readerWriter->template getSymbolPointer<Metadata>(symbolName);
  auto symbolResult = findSymbol(symbolName, nullptr);
  if (symbolResult) {
    auto [file, symbol] = *symbolResult;
    LOG(LogLevel::Detail, "Found symbol for metadata '%s' in %s",
        symbolName.str().c_str(), file->path.c_str());
    return readerWriter->template getSymbolPointer<const Metadata>(
        symbolName, symbol, file);
  }

  auto metadataName = _standardMangledNameForNode(node, demangler);
  if (!metadataName)
    return *metadataName.getError();
  auto metadata = builtMetadataMap.find(*metadataName);
  if (metadata != builtMetadataMap.end()) {
    LOG(LogLevel::Detail, "Found metadata we already built for '%s'",
        symbolName.str().c_str());
    auto constructedMetadata = std::get<1>(*metadata);
    return constructedMetadata.data.offsetBy(constructedMetadata.offset)
        .template cast<const Metadata>();
  }

  // TODO: need a way to find non-exported metadata with no corresponding
  // symbol.

  LOG(LogLevel::Detail, "Did not find metadata '%s', trying to build it",
      symbolName.str().c_str());

  auto constructedMetadata = constructMetadataForNode(node);
  if (!constructedMetadata)
    return *constructedMetadata.getError();
  if (!*constructedMetadata)
    return BuilderError("Failed to find non-generic type '%s'",
                        metadataName->c_str());

  LOG(LogLevel::Info, "Successfully built metadata for '%s'",
      symbolName.str().c_str());
  auto metadataFromMap = builtMetadataMap.find(*metadataName);
  if (metadataFromMap == builtMetadataMap.end())
    return BuilderError("Successfully constructed metadata for "
                        "'%s', but it was not in the map",
                        metadataName->c_str());
  auto constructedMetadataFromMap = std::get<1>(*metadataFromMap);
  assert((*constructedMetadata)->data.ptr ==
         constructedMetadataFromMap.data.ptr);
  assert((*constructedMetadata)->offset == constructedMetadataFromMap.offset);

  return (*constructedMetadata)
      ->data.offsetBy((*constructedMetadata)->offset)
      .template cast<const Metadata>();
}

template <typename Runtime>
std::optional<std::pair<MachOFile *, llvm::object::SymbolRef>>
ExternalGenericMetadataBuilderContext<Runtime>::findSymbol(
    llvm::StringRef toFind, MachOFile *searchFile) {
  llvm::SmallString<128> prefixed;
  prefixed += "_";
  prefixed += toFind;

  if (searchFile == nullptr) {
    auto it = allSymbols.find(prefixed);
    if (it != allSymbols.end())
      return it->getValue();
    return {};
  }

  auto it = searchFile->symbols.find(prefixed);
  if (it != searchFile->symbols.end())
    return {{searchFile, it->getValue()}};

  return {};
}

template <typename Runtime>
std::optional<llvm::object::SectionRef>
ExternalGenericMetadataBuilderContext<Runtime>::findSectionInFile(
    MachOFile *file, uint64_t vmAddr) {
  // TODO: be smarter than scanning all sections?
  for (auto &section : file->objectFile->sections()) {
    if (section.getAddress() <= vmAddr &&
        vmAddr < section.getAddress() + section.getSize()) {
      return section;
    }
  }

  return {};
}

template <typename Runtime>
std::optional<MachOFile *>
ExternalGenericMetadataBuilderContext<Runtime>::findPointerInFiles(
    const void *ptr) {
  auto target = (uintptr_t)ptr;

  // TODO: we should probably do a binary search or anything better than just
  // a linear scan.
  for (auto &file : machOFiles) {
    auto buffer = file->objectFile->getMemoryBufferRef();
    auto start = (uintptr_t)buffer.getBufferStart();
    auto end = (uintptr_t)buffer.getBufferEnd();
    if (start <= target && target < end)
      return file.get();
  }

  return {};
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::build() {
  // Set up the builder.
  builder.reset(
      new GenericMetadataBuilder<ReaderWriter<Runtime>>{*readerWriter.get()});

  // Process all input symbmols.
  for (auto mangledTypeName : mangledNamesToBuild) {
    auto result = constructMetadataForMangledTypeName(mangledTypeName);
    if (auto *error = result.getError())
      fprintf(stderr, "Could not construct metadata for '%s': %s\n",
              mangledTypeName.c_str(), error->cStr());
  }

  auto metadataMap = serializeMetadataMapTable();

  using PrespecializedData = LibPrespecializedData<Runtime>;
  auto topLevelData = allocate<PrespecializedData>(sizeof(PrespecializedData));
  topLevelData.setName(LIB_PRESPECIALIZED_TOP_LEVEL_SYMBOL_NAME);
  topLevelData.ptr->majorVersion = PrespecializedData::currentMajorVersion;
  topLevelData.ptr->minorVersion = PrespecializedData::currentMinorVersion;

  topLevelData.writePointer(&topLevelData.ptr->metadataMap,
                            metadataMap.template cast<const void>());
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::writeOutput(
    llvm::json::OStream &J, unsigned platform,
    const std::string &platformVersion) {
  writeJSONSerialization(J, platform, platformVersion);
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::logDescriptorMap() {
  if (logLevel <= LogLevel::Detail) {
    LOG(LogLevel::Detail, "Descriptor map (%zu entries):",
        mangledNominalTypeDescriptorMap.size());
    for (auto [name, descriptor] : mangledNominalTypeDescriptorMap) {
      fprintf(stderr, "    %s -> %p\n", name.c_str(), descriptor.ptr);
    }
  }
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::cacheDescriptor(
    Buffer<const ContextDescriptor> descriptorBuffer) {
  auto desc = descriptorBuffer.ptr;
  auto typeDesc = dyn_cast<const TypeContextDescriptor>(desc);
  if (!typeDesc) {
    return;
  }

  // This produces a Type -> {Structure|Class|Enum}
  Demangle::Demangler Dem;
  auto demangleNode = buildDemanglingForContext(descriptorBuffer, Dem);
  if (auto *error = demangleNode.getError()) {
    auto symbolInfo = readerWriter->getSymbolInfo(descriptorBuffer);
    LOG(LogLevel::Warning,
        "Could not build demangling for context %s (%s + %" PRIu64 "): %s",
        symbolInfo.symbolName.c_str(), symbolInfo.libraryName.c_str(),
        symbolInfo.pointerOffset, error->cStr());
    return;
  }

  // ... which needs to be wrapped in Global -> NominalTypeDescriptor -> * to
  // match what we cache during dylib reading.
  auto global = wrapNode(Dem, *demangleNode.getValue(), Node::Kind::Global,
                         Node::Kind::NominalTypeDescriptor);

  auto mangling = Demangle::mangleNode(global);
  if (!mangling.isSuccess()) {
    auto symbolInfo = readerWriter->getSymbolInfo(descriptorBuffer);
    LOG(LogLevel::Warning,
        "Could not build demangling for context %s (%s + %" PRIu64 "): %s",
        symbolInfo.symbolName.c_str(), symbolInfo.libraryName.c_str(),
        symbolInfo.pointerOffset, getNodeTreeAsString(global).c_str());
    return;
  }

  LOG(LogLevel::Info, "Found descriptor %s", mangling.result().c_str());

  auto [iterator, didInsert] = mangledNominalTypeDescriptorMap.try_emplace(
      mangling.result(),
      descriptorBuffer.template cast<const TypeContextDescriptor>());

  if (!didInsert) {
    auto [name, oldTypeDesc] = *iterator;
    LOG(LogLevel::Warning,
        "Descriptor for %s already exists - old: %p - new: %p", name.c_str(),
        oldTypeDesc.ptr, typeDesc);
  }
}

// Perform prep tasks on file, add it to machOFiles, and take ownership of the
// object.
template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::addImage(MachOFile *file) {
  // Replace the path with the library's install name if possible.
  auto installName = getInstallName(file->objectFile.get());
  if (installName.length() > 0)
    file->path = installName;

  populateMachOSymbols(file);
  populateMachOFixups(file);
  readMachOSections(file);
  machOFiles.emplace_back(file);
  machOFilesByPath.insert({file->path, file});
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::populateMachOSymbols(
    MachOFile *file) {
  // Fill out library names.
  for (auto &command : file->objectFile->load_commands()) {
    if (command.C.cmd == llvm::MachO::LC_LOAD_DYLIB ||
        command.C.cmd == llvm::MachO::LC_LOAD_WEAK_DYLIB ||
        command.C.cmd == llvm::MachO::LC_REEXPORT_DYLIB ||
        command.C.cmd == llvm::MachO::LC_LOAD_UPWARD_DYLIB) {
      auto *dylibCommand =
          reinterpret_cast<const llvm::MachO::dylib_command *>(command.Ptr);
      auto name = reinterpret_cast<const char *>(dylibCommand) +
                  dylibCommand->dylib.name;
      file->libraryNames.push_back(name);
    }
  }

  // Find all the symbols.
  for (auto &sym : file->objectFile->symbols()) {
    auto type = sym.getType();
    if (!type)
      continue;
    if (type.get() != llvm::object::SymbolRef::ST_Data &&
        type.get() != llvm::object::SymbolRef::ST_Function)
      continue;

    auto name = sym.getName();
    if (!name) {
      consumeError(name.takeError());
      continue;
    }

    auto flags = sym.getFlags();
    if (!flags) {
      consumeError(flags.takeError());
      continue;
    }

    auto [iterator, didInsert] = file->symbols.try_emplace(*name, sym);
    if (!didInsert) {
      LOG(LogLevel::Detail, "Duplicate symbol name %s in %s",
          name->str().c_str(), file->path.c_str());
    }
    file->namedSymbolsSortedByAddress.push_back(sym);

    allSymbols.insert({*name, {file, sym}});

    if (*flags & llvm::object::SymbolRef::Flags::SF_Exported) {
      file->exportedSymbolsSortedByAddress.push_back(sym);
    }
  }

  auto compareAddresses = [&](const llvm::object::SymbolRef &a,
                              const llvm::object::SymbolRef &b) {
    auto aAddr = a.getAddress();
    if (!aAddr)
      return false;
    auto bAddr = b.getAddress();
    if (!bAddr)
      return true;

    return *aAddr < *bAddr;
  };
  std::sort(file->namedSymbolsSortedByAddress.begin(),
            file->namedSymbolsSortedByAddress.end(), compareAddresses);
  std::sort(file->exportedSymbolsSortedByAddress.begin(),
            file->exportedSymbolsSortedByAddress.end(), compareAddresses);

  LOG(LogLevel::Info, "%s: populated %zu linked libraries and %u symbols",
      file->path.c_str(), file->libraryNames.size(), file->symbols.size());
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::populateMachOFixups(
    MachOFile *file) {
  llvm::Error err = llvm::Error::success();

  auto objectFile = file->objectFile.get();

  std::optional<std::variant<llvm::object::MachOBindEntry,
                             llvm::object::MachOChainedFixupEntry>>
      found;

  for (const auto &bind : objectFile->bindTable(err)) {
    LOG(LogLevel::Detail, "bind: %s %s %s %s %#" PRIx64,
        bind.typeName().str().c_str(), bind.symbolName().str().c_str(),
        bind.segmentName().str().c_str(), bind.sectionName().str().c_str(),
        bind.address());

    FixupTarget target{bind.ordinal(), bind.symbolName()};
    file->fixups.insert({bind.address(), target});
  }
  if (err) {
    LOG(LogLevel::Warning, "Failed to get bind table: %s",
        getErrorString(std::move(err)).c_str());
  }

  // Ignoring lazyBindTable and weakBindTable, as the symbols we're interested
  // in shouldn't appear in them.

  for (const auto &fixup : objectFile->fixupTable(err)) {
    const char *libName;
    int ordinal = fixup.ordinal();
    if (ordinal > 0 && (size_t)ordinal <= file->libraryNames.size())
      libName = file->libraryNames[ordinal - 1];
    else if (ordinal == FixupTarget::selfLibrary)
      libName = "<self>";
    else
      libName = "<unknown ordinal>";

    LOG(LogLevel::Detail,
        "fixup: ordinal=%d type=%s symbol=%s segment=%s section=%s "
        "address=%#" PRIx64 " lib=%s",
        ordinal, fixup.typeName().str().c_str(),
        fixup.symbolName().str().c_str(), fixup.segmentName().str().c_str(),
        fixup.sectionName().str().c_str(), fixup.address(), libName);

    FixupTarget target;
    target.library = fixup.ordinal();
    if (auto ptr = fixup.pointerValue())
      target.target = ptr;
    else
      target.target = fixup.symbolName();
    file->fixups.insert({fixup.address(), target});
  }
  if (err) {
    LOG(LogLevel::Warning, "Failed to get bind table: %s",
        getErrorString(std::move(err)).c_str());
  }
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::readMachOSections(
    MachOFile *file) {
  using namespace llvm;
  // Search for the section for types
  for (auto &Section : file->objectFile->sections()) {
    llvm::Expected<StringRef> SectionNameOrErr = Section.getName();
    if (!SectionNameOrErr) {
      llvm::consumeError(SectionNameOrErr.takeError());
      continue;
    }

    StringRef SectionName = *SectionNameOrErr;
    if (SectionName == "__swift5_types") {
      llvm::Expected<llvm::StringRef> SectionData = Section.getContents();
      if (!SectionData) {
        LOG(LogLevel::Warning,
            "Failed to get __swift5_types section data from %s: %s",
            file->path.c_str(),
            getErrorString(SectionData.takeError()).c_str());
        return;
      }
      auto Contents = SectionData.get();

      using SectionContentType =
          const RelativeDirectPointerIntPair<ContextDescriptor,
                                             TypeReferenceKind>;

      Buffer<SectionContentType> sectionBuffer{this, file, Section, Contents};
      size_t count = Contents.size() / sizeof(SectionContentType);
      for (size_t i = 0; i < count; i++) {
        auto &record = sectionBuffer.ptr[i];
        if (record.getInt() == TypeReferenceKind::DirectTypeDescriptor) {
          auto resolved = sectionBuffer.resolvePointer(&record);
          if (auto *error = resolved.getError()) {
            LOG(LogLevel::Warning,
                "Descriptor record %p (__swift5_types + %zu), failed to "
                "resolve pointer: %s",
                &record, (size_t)&record - (size_t)sectionBuffer.ptr,
                error->cStr());
            break;
          }

          auto descBuffer = *resolved.getValue();
          LOG(LogLevel::Info,
              "Caching descriptor record %p (__swift5_types + %zu) -> %p",
              &record, (size_t)&record - (size_t)sectionBuffer.ptr,
              descBuffer.ptr);
          cacheDescriptor(descBuffer);
        } else {
          LOG(LogLevel::Warning,
              "Descriptor record %p (__swift5_types + %zu) is indirect, "
              "not yet implemented",
              &record, (size_t)&record - (size_t)sectionBuffer.ptr);
        }
      }
      break;
    }
  }
}

template <typename Runtime>
BuilderErrorOr<std::string> ExternalGenericMetadataBuilderContext<Runtime>::_mangledNominalTypeNameForBoundGenericNode(
    Demangle::NodePointer BoundGenericNode) {
  LOG(LogLevel::Detail, "BoundGenericNode:\n%s",
      getNodeTreeAsString(BoundGenericNode).c_str());

  Demangle::Demangler Dem;
  auto unspecializedOrError = getUnspecialized(BoundGenericNode, Dem);
  if (!unspecializedOrError.isSuccess()) {
    return BuilderError("getUnspecialized failed with code %u",
                        unspecializedOrError.error().code);
  }
  auto unspecialized = unspecializedOrError.result();
  LOG(LogLevel::Detail, "unspecialized:\n%s",
      getNodeTreeAsString(unspecialized).c_str());

  auto globalNode =
      wrapNode(Dem, unspecialized, Node::Kind::Global,
               Node::Kind::NominalTypeDescriptor, Node::Kind::Type);

  LOG(LogLevel::Detail, "globalTypeDescriptor:\n%s",
      getNodeTreeAsString(globalNode).c_str());

  auto mangling = Demangle::mangleNode(globalNode);
  if (!mangling.isSuccess()) {
    return BuilderError("Failed to mangle unspecialized node.");
  }

  return mangling.result();
}

template <typename Runtime>
BuilderErrorOr<std::optional<typename ExternalGenericMetadataBuilderContext<
    Runtime>::Builder::ConstructedMetadata>>
ExternalGenericMetadataBuilderContext<
    Runtime>::constructMetadataForMangledTypeName(llvm::StringRef typeName) {
  auto node = demangleCtx.demangleTypeAsNode(typeName);
  if (!node) {
    return BuilderError("Failed to demangle '%s'.", typeName.str().c_str());
  }
  LOG(LogLevel::Detail, "Result: %s", nodeToString(node).c_str());
  return constructMetadataForNode(node);
}

// Returns the constructed metadata, or no value if the node doesn't contain a
// bound generic.
template <typename Runtime>
BuilderErrorOr<std::optional<typename ExternalGenericMetadataBuilderContext<
    Runtime>::Builder::ConstructedMetadata>>
ExternalGenericMetadataBuilderContext<Runtime>::constructMetadataForNode(
    swift::Demangle::NodePointer node) {
  LOG(LogLevel::Detail, "\n%s", getNodeTreeAsString(node).c_str());

  // Pre-specialize BoundGenericClass|Structure|Enum
  switch (node->getKind()) {
  case Node::Kind::Global:
  case Node::Kind::Type:
  case Node::Kind::TypeMangling:
  case Node::Kind::TypeMetadata:
    if (!node->hasChildren())
      return BuilderError("%s node has no children",
                          nodeKindString(node->getKind()));
    return constructMetadataForNode(node->getFirstChild());

  case Node::Kind::BoundGenericClass:
  case Node::Kind::BoundGenericStructure:
  case Node::Kind::BoundGenericEnum:
    break;
  case Node::Kind::BoundGenericProtocol:
  case Node::Kind::BoundGenericTypeAlias:
  case Node::Kind::BoundGenericFunction:
  case Node::Kind::BoundGenericOtherNominalType:
    return BuilderError("Can't construct metadata for kind %#hx (%s)",
                        static_cast<uint16_t>(node->getKind()),
                        nodeKindString(node->getKind()));
  default:
    LOG(LogLevel::Info, "Default success with kind %s",
        nodeKindString(node->getKind()));
    return {{}};
  }

  // Get the type descriptor.
  // Type descriptors were cached from __swift5_types sections during
  // readMachOSections. If one is missing here, then there was a mismatch
  // between what bound generic type names we were given, and what dylibs we
  // have parsed.
  auto mangledNominalTypeName =
      _mangledNominalTypeNameForBoundGenericNode(node);
  if (!mangledNominalTypeName)
    return *mangledNominalTypeName.getError();
  auto foundTypeDescriptor =
      mangledNominalTypeDescriptorMap.find(*mangledNominalTypeName);
  if (foundTypeDescriptor == mangledNominalTypeDescriptorMap.end()) {
    // Either the mangling failed, or we didn't find this type during dylib
    // ingestion.
    LOG(LogLevel::Detail, "%s", getNodeTreeAsString(node).c_str());
    return BuilderError(
        "Failed to look up type descriptor for '%s' from string '%s'.",
        nodeToString(node).c_str(), mangledNominalTypeName->c_str());
  }
  auto typeDescriptor = std::get<1>(*foundTypeDescriptor);

  LOG(LogLevel::Detail, "Found type descriptor %p\n", typeDescriptor.ptr);

  // Get the generic arguments.
  auto genericTypeListNode = node->getChild(1);
  assert(genericTypeListNode->getKind() == Node::Kind::TypeList);
  assert(genericTypeListNode->hasChildren());

  std::vector<Buffer<const Metadata>> genericTypeMetadatas;
  for (auto genericTypeNode : *genericTypeListNode) {
    LOG(LogLevel::Detail, "genericTypeNode:\n%s",
        getNodeTreeAsString(genericTypeNode).c_str());
    auto maybeMetadata = metadataForNode(genericTypeNode);
    if (auto *error = maybeMetadata.getError()) {
      return BuilderError("Failed to get generic type reference '%s': %s",
                          nodeToString(genericTypeNode).c_str(), error->cStr());
    }
    genericTypeMetadatas.push_back(*maybeMetadata.getValue());
  }

  // Get the generic metadata pattern.
  auto &generics = typeDescriptor.ptr->getFullGenericContextHeader();
  auto maybePattern =
      typeDescriptor.resolvePointer(&generics.DefaultInstantiationPattern);
  if (auto *error = maybePattern.getError()) {
    return BuilderError("Failed to resolve instantiation pattern pointer: %s",
                        error->cStr());
  }
  auto pattern = *maybePattern.getValue();

  LOG(LogLevel::Detail, "pattern: %p", pattern.ptr);

  auto maybeExtraDataSize =
      builder->extraDataSize(typeDescriptor, pattern.toConst());
  if (auto *error = maybeExtraDataSize.getError()) {
    return BuilderError("Failed to compute extra data size: %s", error->cStr());
  }
  auto extraDataSize = *maybeExtraDataSize.getValue();
  LOG(LogLevel::Detail, "extraDataSize: %zu", extraDataSize);

  auto maybeMetadata = builder->buildGenericMetadata(
      typeDescriptor, genericTypeMetadatas, pattern.toConst(), extraDataSize);
  if (auto *error = maybeMetadata.getError()) {
    return BuilderError("Failed to build metadata '%s': %s",
                        getNodeTreeAsString(node).c_str(), error->cStr());
  }
  auto metadata = *maybeMetadata.getValue();

  LOG(LogLevel::Detail, "metadata: %p", metadata.data.ptr);

  Demangle::Demangler Dem;
  auto mangledName = _standardMangledNameForNode(node, Dem);
  if (!mangledName)
    return *mangledName.getError();

  metadata.data.setName(*mangledName);
  builtMetadataMap.emplace(*mangledName, metadata);

  auto initializeResult =
      builder->initializeGenericMetadata(metadata.data, node);
  if (auto *error = initializeResult.getError()) {
    builtMetadataMap.erase(*mangledName);
    return BuilderError("Failed to build metadata '%s': %s",
                        getNodeTreeAsString(node).c_str(), error->cStr());
  }

  auto optionalMetadata = std::optional{metadata};
  return {optionalMetadata};
}

/// Make the metadata map table, and return a buffer pointing to it.
template <typename Runtime>
typename ExternalGenericMetadataBuilderContext<Runtime>::template Buffer<char>
ExternalGenericMetadataBuilderContext<Runtime>::serializeMetadataMapTable(
    void) {
  auto numEntries = builtMetadataMap.size();

  // Array size must be at least numEntries+1 per the requirements of
  // PrebuiltStringMap. Aim for a 75% load factor.
  auto arraySize = std::max(numEntries * 4 / 3, numEntries + 1);

  using Map =
      PrebuiltStringMap<StoredPointer, StoredPointer, StoredPointer::isNull>;
  auto byteSize = Map::byteSize(arraySize);

  auto mapData = allocate<char>(byteSize);
  auto serializedMap = new (mapData.ptr) Map(arraySize);

  std::vector<const std::pair<const std::string, const ConstructedMetadata> *>
      sortedMapElements;
  sortedMapElements.reserve(numEntries);
  for (auto &entry : builtMetadataMap)
    sortedMapElements.push_back(&entry);

  std::sort(
      sortedMapElements.begin(), sortedMapElements.end(),
      [](const std::pair<const std::string, const ConstructedMetadata> *a,
         const std::pair<const std::string, const ConstructedMetadata> *b) {
        return std::get<0>(*a) < std::get<0>(*b);
      });

  for (auto *entry : sortedMapElements) {
    auto [key, value] = *entry;
    auto *elementPtr = serializedMap->insert(key.c_str());
    if (!elementPtr) {
      fprintf(stderr, "PrebuiltStringMap insert failed!\n");
      abort();
    }

    auto stringData = allocate<char>(key.size() + 1);
    memcpy(stringData.ptr, key.c_str(), key.size());
    stringData.setName("_cstring_" + key);
    mapData.writePointer(&elementPtr->key, stringData);

    auto metadataPointer = value.data.offsetBy(value.offset);
    mapData.writePointer(&elementPtr->value, metadataPointer);
  }

  mapData.setName("_swift_prespecializedMetadataMap");

  return mapData;
}

/// Write the atom's contents as JSON. For each symbol reference emitted, the
/// callback is passed the target MachOFile * and StringRef of the symbol's
/// name.
template <typename Runtime>
template <typename SymbolCallback>
void ExternalGenericMetadataBuilderContext<Runtime>::writeAtomContentsJSON(
    llvm::json::OStream &J,
    const typename ExternalGenericMetadataBuilderContext<Runtime>::Atom &atom,
    const SymbolCallback &symbolCallback) {
  const char *ptrTargetKind = sizeof(StoredPointer) == 8 ? "ptr64" : "ptr32";

  // Maintain cursors in the atom's buffer and targets. In a loop, we write out
  // any buffer contents between the current point and the next target, then we
  // write out the next target.
  size_t bufferCursor = 0;
  auto targetsCursor = atom.pointerTargetsSorted.begin();
  auto targetsEnd = atom.pointerTargetsSorted.end();

  auto writeSlice = [&](size_t from, size_t to) {
    if (from < to) {
      llvm::StringRef slice{&atom.buffer[from], to - from};
      J.value(llvm::toHex(slice));
    }
  };

  while (targetsCursor != targetsEnd) {
    assert(bufferCursor <= targetsCursor->offset);
    writeSlice(bufferCursor, targetsCursor->offset);

    J.object([&] {
      if (auto *fileTarget =
              std::get_if<FileTarget>(&targetsCursor->fileOrAtom)) {
        std::string foundSymbolName = "";
        uint64_t foundSymbolAddress = 0;

        auto foundSymbol = readerWriter->getNearestExportedSymbol(
            fileTarget->file, fileTarget->addressInFile);

        if (foundSymbol) {
          if (auto name = foundSymbol->getName()) {
            if (auto address = foundSymbol->getAddress()) {
              foundSymbolName = *name;
              foundSymbolAddress = *address;
            }
          }
        }

        if (foundSymbolName == "") {
          auto section =
              findSectionInFile(fileTarget->file, fileTarget->addressInFile);
          if (section) {
            auto name = section->getName();
            if (name) {
              const char *segment = section->isText() ? "__TEXT" : "__DATA";
              foundSymbolName = "$dylib_segment$" + fileTarget->file->path +
                                "$start$" + segment + "$" + name->str();
            }
          }
        }
        if (foundSymbolName == "") {
          // Couldn't find a symbol or a section, this really shouldn't happen.
          foundSymbolName =
              "$dylib_segment$" + fileTarget->file->path + "$start$__TEXT";
          foundSymbolAddress = 0;
        }

        symbolCallback(fileTarget->file, foundSymbolName);

        J.attribute("target", foundSymbolName);
        J.attribute("addend", (int64_t)fileTarget->addressInFile -
                                  (int64_t)foundSymbolAddress);
        J.attribute("kind", ptrTargetKind);
      } else {
        auto atomTarget = std::get<AtomTarget>(targetsCursor->fileOrAtom);
        J.attribute("self", true);
        J.attribute("target", "_" + atomTarget.atom->name);
        J.attribute("addend", atomTarget.offset);
        J.attribute("kind", ptrTargetKind);
      }

      if (usePtrauth && targetsCursor->ptrauthKey != PtrauthKey::None) {
        J.attributeObject("authPtr", [&] {
          J.attribute("key", static_cast<uint8_t>(targetsCursor->ptrauthKey));
          J.attribute("addr", targetsCursor->addressDiversified);
          J.attribute("diversity", targetsCursor->discriminator);
        });
      }
    });

    bufferCursor = targetsCursor->offset + targetsCursor->size;

    targetsCursor++;
  }

  // Write any remaining data after the last target.
  writeSlice(bufferCursor, atom.buffer.size());
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::writeDylibsJSON(
    llvm::json::OStream &J,
    std::unordered_map<MachOFile *, std::unordered_set<std::string>>
        &symbolReferences) {
  for (auto &elt : symbolReferences) {
    auto file = std::get<0>(elt);
    auto &names = std::get<1>(elt);
    J.object([&] {
      J.attribute("installName", file->path);
      J.attributeArray("exports", [&] {
        for (auto name : names) {
          J.object([&] { J.attribute("name", name); });
        }
      });
    });
  }
}

template <typename Runtime>
void ExternalGenericMetadataBuilderContext<Runtime>::writeJSONSerialization(
    llvm::json::OStream &J, unsigned platform,
    const std::string &platformVersion) {
  // Name any unnamed atoms.
  unsigned i = 0;
  for (auto &atom : atoms) {
    if (atom->name.length() == 0) {
      atom->name = "__unnamed_atom_" + std::to_string(i++);
    }
  }

  J.object([&] {
    J.attribute("version", 1);

    J.attribute("platform", platform);
    J.attribute("platformVersion", platformVersion);

    J.attribute("arch", arch);
    J.attribute("installName", "/usr/lib/libswiftPrespecialized.dylib");

    // Track the referenced libraries and symbol names from all of the atoms.
    std::unordered_map<MachOFile *, std::unordered_set<std::string>>
        symbolReferences;
    J.attributeArray("atoms", [&] {
      for (auto &atom : atoms) {
        J.object([&] {
          J.attribute("name", "_" + atom->name);
          J.attribute("contentType", "constData");
          J.attributeArray("contents", [&] {
            writeAtomContentsJSON(
                J, *atom, [&](MachOFile *file, llvm::StringRef symbolName) {
                  symbolReferences[file].insert(symbolName.str());
                });
          });
        });
      }
    });

    J.attributeArray("dylibs", [&] { writeDylibsJSON(J, symbolReferences); });
  });
}

BuilderErrorOr<std::vector<std::string>> readNames(llvm::StringRef contents) {
  auto json = llvm::json::parse(contents);
  if (!json)
    return BuilderError("Failed to parse names JSON: %s",
                        getErrorString(json.takeError()).c_str());

  auto topLevel = json->getAsObject();
  if (!topLevel)
    return BuilderError(
        "Failed to parse names JSON: top level value is not an object");

  auto metadataNamesValue = topLevel->get("metadataNames");
  if (!metadataNamesValue)
    return BuilderError(
        "Failed to parse names JSON: no 'metadataNames' key in top level");

  auto metadataNames = metadataNamesValue->getAsArray();
  if (!metadataNames)
    return BuilderError(
        "Failed to parse names JSON: 'metadataNames' is not an array");

  std::vector<std::string> names;

  for (auto &entryValue : *metadataNames) {
    auto entry = entryValue.getAsObject();
    if (!entry)
      return BuilderError("Failed to parse names JSON: 'entries' contains "
                          "value that is not an object");

    auto nameValue = entry->get("name");
    if (!nameValue)
      return BuilderError(
          "Failed to parse names JSON: entry does not contain 'name' key");

    auto name = nameValue->getAsString();
    if (!name)
      return BuilderError(
          "Failed to parse names JSON: 'name' value is not a string");

    names.push_back(std::string(*name));
  }

  return {names};
}

BuilderErrorOr<std::vector<std::string>> readNamesFromFile(const char *path) {
  auto contents = llvm::MemoryBuffer::getFile(path);
  if (!contents)
    return BuilderError("Could not read names file at '%s': %s", path,
                        contents.getError().message().c_str());
  return readNames(contents.get()->getBuffer());
}

template <typename Fn>
static BuilderErrorOr<std::monostate> enumerateFilesInPath(std::string path,
                                                           const Fn &callback) {
  std::error_code err;
  llvm::sys::fs::recursive_directory_iterator iterator(path, err);
  llvm::sys::fs::recursive_directory_iterator end;

  while (iterator != end && !err) {
    if (iterator->type() != llvm::sys::fs::file_type::directory_file)
      callback(iterator->path());
    iterator.increment(err);
  }

  if (err)
    return BuilderError("Iteration of directory %s failed: %s", path.c_str(),
                        err.message().c_str());

  return {{}};
}

BuilderErrorOr<unsigned> getPointerWidth(std::string arch) {
  if (arch == "arm64" || arch == "arm64e")
    return 8;
  if (arch == "arm64_32")
    return 4;
  if (arch == "x86_64")
    return 8;

  return BuilderError("Unknown arch '%s'", arch.c_str());
}

} // namespace swift

struct SwiftExternalMetadataBuilder {
  using Builder32 =
      swift::ExternalGenericMetadataBuilderContext<swift::ExternalRuntime32>;
  using Builder64 =
      swift::ExternalGenericMetadataBuilderContext<swift::ExternalRuntime64>;

  std::variant<Builder32, Builder64> context;

  int platform;

  // Storage for an error string returned to the caller.
  std::string lastErrorString;

  SwiftExternalMetadataBuilder(int pointerSize) {
    if (pointerSize == 4) {
      context.emplace<Builder32>();
    } else if (pointerSize == 8) {
      context.emplace<Builder64>();
    } else {
      fprintf(stderr, "Unsupported pointer size %d\n", pointerSize);
      abort();
    }
  }

  template <typename Fn>
  void withContext(const Fn &fn) {
    if (auto *context32 = std::get_if<Builder32>(&context))
      fn(context32);
    if (auto *context64 = std::get_if<Builder64>(&context))
      fn(context64);
  }
};

struct SwiftExternalMetadataBuilder *
swift_externalMetadataBuilder_create(int platform, const char *arch) {
  auto pointerWidth = swift::getPointerWidth(arch);
  if (auto *error = pointerWidth.getError()) {
    fprintf(stderr, "%s\n", error->getErrorString().c_str());
    return nullptr;
  }

  auto *builder = new SwiftExternalMetadataBuilder(*pointerWidth.getValue());
  builder->platform = platform;
  builder->withContext([&](auto *context) { context->setArch(arch); });
  return builder;
}

void swift_externalMetadataBuilder_destroy(
    struct SwiftExternalMetadataBuilder *builder) {
  delete builder;
}

void swift_externalMetadataBuilder_setLogLevel(
    struct SwiftExternalMetadataBuilder *builder, int level) {
  builder->withContext([&](auto *context) { context->setLogLevel(level); });
}

const char *swift_externalMetadataBuilder_readNamesJSON(
    struct SwiftExternalMetadataBuilder *builder, const char *names_json) {
  auto names = swift::readNames(names_json);
  if (auto *error = names.getError()) {
    builder->lastErrorString = error->cStr();
    return builder->lastErrorString.c_str();
  }

  builder->withContext(
      [&](auto *context) { context->setNamesToBuild(*names.getValue()); });
  return nullptr;
}

const char *swift_externalMetadataBuilder_addDylib(
    struct SwiftExternalMetadataBuilder *builder, const char *installName,
    const struct mach_header *mh, uint64_t size) {
  builder->lastErrorString = "";
  builder->withContext([&](auto *context) {
    auto error = context->addImageInMemory(mh, size, installName);
    if (error)
      builder->lastErrorString = swift::getErrorString(std::move(error));
  });

  if (builder->lastErrorString != "")
    return builder->lastErrorString.c_str();
  return nullptr;
}

const char *swift_externalMetadataBuilder_buildMetadata(
    struct SwiftExternalMetadataBuilder *builder) {
  builder->withContext([&](auto *context) { context->build(); });
  return nullptr;
}

const char *swift_externalMetadataBuilder_getMetadataJSON(
    struct SwiftExternalMetadataBuilder *builder) {
  bool prettyPrint = true;
  unsigned indent = prettyPrint ? 4 : 0;

  std::string output;
  llvm::raw_string_ostream ostream(output);
  llvm::json::OStream J(ostream, indent);

  builder->withContext([&](auto *context) {
    context->writeOutput(J, builder->platform, "1.0");
  });

  return strdup(output.c_str());
}

int swift_type_metadata_extract(const char *inputPath,       // mangled names
                                const char *dylibSearchPath, // images to add
                                const char *arch,
                                const char *outputPath // json output
) {
  auto names = swift::readNamesFromFile(inputPath);
  if (auto *error = names.getError()) {
    fprintf(stderr, "%s\n", error->getErrorString().c_str());
    exit(1);
  }

  auto pointerWidth = swift::getPointerWidth(arch);
  if (auto *error = pointerWidth.getError()) {
    fprintf(stderr, "%s\n", error->getErrorString().c_str());
    exit(1);
  }

  SwiftExternalMetadataBuilder builder(*pointerWidth.getValue());
  builder.withContext([&](auto *context) {
    context->setNamesToBuild(*names.getValue());
    context->setArch(arch);
    auto imageAddResult = context->addImagesInPath(dylibSearchPath);
    if (auto *error = imageAddResult.getError()) {
      fprintf(stderr, "Error iterating over dylib search path '%s': %s",
              dylibSearchPath, error->cStr());
      exit(1);
    }
    context->logDescriptorMap();

    bool prettyPrint = true;

    std::error_code errorCode;
    llvm::raw_fd_ostream fileStream(outputPath, errorCode);
    if (errorCode) {
      fprintf(stderr, "ERROR: Could not open %s for writing.\n", outputPath);
      exit(1);
    }

    context->build();

    unsigned indent = prettyPrint ? 4 : 0;
    llvm::json::OStream J(fileStream, indent);

    unsigned platform = 1; // PLATFORM_MACOS
    const char *platformVersion = "14.0";
    context->writeOutput(J, platform, platformVersion);
  });

  fprintf(stderr, "Completed!\n");

  return 0;
}
