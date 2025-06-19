//===--- BasicBridging.h - header for the swift BasicBridging module ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_BASICBRIDGING_H
#define SWIFT_BASIC_BASICBRIDGING_H

#if !defined(COMPILED_WITH_SWIFT) || !defined(PURE_BRIDGING_MODE)
#define USED_IN_CPP_SOURCE
#endif

// Do not add other C++/llvm/swift header files here!
// Function implementations should be placed into BasicBridging.cpp and required header files should be added there.
//
// Pure bridging mode does not permit including any C++/llvm/swift headers.
// See also the comments for `BRIDGING_MODE` in the top-level CMakeLists.txt file.
//
//
// Note: On Windows ARM64, how a C++ struct/class value type is
// returned is sensitive to conditions including whether a
// user-defined constructor exists, etc. See
// https://learn.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-170#return-values
// So, if a C++ struct/class type is returned as a value between Swift
// and C++, we need to be careful to match the return convention
// matches between the non-USED_IN_CPP_SOURCE (Swift) side and the
// USE_IN_CPP_SOURCE (C++) side.
//
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !! Do not put any constructors inside an `#ifdef USED_IN_CPP_SOURCE` block !!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include "swift/Basic/BridgedSwiftObject.h"
#include "swift/Basic/SwiftBridging.h"

#include <stddef.h>
#include <stdint.h>
#ifdef USED_IN_CPP_SOURCE
// Workaround to avoid a compiler error because `cas::ObjectRef` is not defined
// when including VirtualFileSystem.h
#include <cassert>
#include "llvm/CAS/CASReference.h"

#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/APInt.h"
#include <string>
#include <vector>
#endif

#if __has_attribute(availability)
#define SWIFT_UNAVAILABLE(msg) \
  __attribute__((availability(swift, unavailable, message=msg)))
#else
#define SWIFT_UNAVAILABLE(msg)
#endif

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, briding functions are not inlined
#define BRIDGED_INLINE
#else
#define BRIDGED_INLINE inline
#endif

namespace llvm {
class raw_ostream;
class StringRef;
class VersionTuple;
} // end namespace llvm

namespace swift {
class SourceLoc;
class SourceRange;
class CharSourceRange;
}

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

typedef intptr_t SwiftInt;
typedef uintptr_t SwiftUInt;

// Define a bridging wrapper that wraps an underlying C++ pointer type. When
// importing into Swift, we expose an initializer and accessor that works with
// `void *`, which is imported as UnsafeMutableRawPointer. Note we can't rely on
// Swift importing the underlying C++ pointer as an OpaquePointer since that is
// liable to change with PURE_BRIDGING_MODE, since that changes what we include,
// and Swift could import the underlying pointee type instead. We need to be
// careful that the interface we expose remains consistent regardless of
// PURE_BRIDGING_MODE.
#define BRIDGING_WRAPPER_IMPL(Node, Name, Qualifier, Nullability)              \
  class Bridged##Name {                                                        \
    Qualifier Node *Nullability Ptr;                                           \
                                                                               \
  public:                                                                      \
    SWIFT_UNAVAILABLE("Use init(raw:) instead")                                \
    Bridged##Name(Qualifier Node *Nullability ptr) : Ptr(ptr) {}               \
                                                                               \
    SWIFT_UNAVAILABLE("Use '.raw' instead")                                    \
    Qualifier Node *Nullability unbridged() const { return Ptr; }              \
                                                                               \
    SWIFT_COMPUTED_PROPERTY                                                    \
    Qualifier void *Nullability getRaw() const { return unbridged(); };        \
  };                                                                           \
                                                                               \
  SWIFT_NAME("Bridged" #Name ".init(raw:)")                                    \
  inline Bridged##Name Bridged##Name##_fromRaw(                                \
      Qualifier void *Nullability ptr) {                                       \
    return static_cast<Qualifier Node *>(ptr);                                 \
  }

// Bridging wrapper macros for convenience.
#define BRIDGING_WRAPPER_NONNULL(Node, Name)                                   \
  BRIDGING_WRAPPER_IMPL(Node, Name, /*unqualified*/, _Nonnull)

#define BRIDGING_WRAPPER_NULLABLE(Node, Name)                                  \
  BRIDGING_WRAPPER_IMPL(Node, Nullable##Name, /*unqualified*/, _Nullable)

#define BRIDGING_WRAPPER_CONST_NONNULL(Node, Name)                             \
  BRIDGING_WRAPPER_IMPL(Node, Name, const, _Nonnull)

#define BRIDGING_WRAPPER_CONST_NULLABLE(Node, Name)                            \
  BRIDGING_WRAPPER_IMPL(Node, Nullable##Name, const, _Nullable)

void assertFail(const char * _Nonnull msg, const char * _Nonnull file,
                SwiftUInt line, const char * _Nonnull function);

//===----------------------------------------------------------------------===//
// MARK: ArrayRef
//===----------------------------------------------------------------------===//

class BridgedArrayRef {
public:
  SWIFT_UNAVAILABLE("Use '.data' instead")
  const void *_Nullable Data;

  SWIFT_UNAVAILABLE("Use '.count' instead")
  size_t Length;

  BridgedArrayRef() : Data(nullptr), Length(0) {}

  SWIFT_NAME("init(data:count:)")
  BridgedArrayRef(const void *_Nullable data, size_t length)
      : Data(data), Length(length) {}

#ifdef USED_IN_CPP_SOURCE
  template <typename T>
  BridgedArrayRef(llvm::ArrayRef<T> arr)
      : Data(arr.data()), Length(arr.size()) {}

  template <typename T>
  llvm::ArrayRef<T> unbridged() const {
    return {static_cast<const T *>(Data), Length};
  }
#endif

  SWIFT_COMPUTED_PROPERTY
  const void *_Nullable getData() const { return Data; }

  SWIFT_COMPUTED_PROPERTY
  SwiftInt getCount() const { return static_cast<SwiftInt>(Length); }

  SWIFT_COMPUTED_PROPERTY
  bool getIsEmpty() const { return Length == 0; }
};

//===----------------------------------------------------------------------===//
// MARK: BridgedData
//===----------------------------------------------------------------------===//

class BridgedData {
public:
  SWIFT_UNAVAILABLE("Use '.baseAddress' instead")
  const char *_Nullable BaseAddress;

  SWIFT_UNAVAILABLE("Use '.count' instead")
  size_t Length;

  BridgedData() : BaseAddress(nullptr), Length(0) {}

  SWIFT_NAME("init(baseAddress:count:)")
  BridgedData(const char *_Nullable baseAddress, size_t length)
      : BaseAddress(baseAddress), Length(length) {}

  SWIFT_COMPUTED_PROPERTY
  const char *_Nullable getBaseAddress() const { return BaseAddress; }

  SWIFT_COMPUTED_PROPERTY
  SwiftInt getCount() const { return static_cast<SwiftInt>(Length); }

  void free() const;
};

//===----------------------------------------------------------------------===//
// MARK: Feature
//===----------------------------------------------------------------------===//

enum ENUM_EXTENSIBILITY_ATTR(open) BridgedFeature {
#define LANGUAGE_FEATURE(FeatureName, SENumber, Description) FeatureName,
#include "swift/Basic/Features.def"
};

//===----------------------------------------------------------------------===//
// MARK: StringRef
//===----------------------------------------------------------------------===//

class BridgedOStream;

class BridgedStringRef {
  const char *_Nullable Data;
  size_t Length;

public:
  BRIDGED_INLINE BridgedStringRef(llvm::StringRef sref);
  BRIDGED_INLINE llvm::StringRef unbridged() const;

  BridgedStringRef() : Data(nullptr), Length(0) {}

  SWIFT_NAME("init(data:count:)")
  BridgedStringRef(const char *_Nullable data, size_t length)
      : Data(data), Length(length) {}

  SWIFT_COMPUTED_PROPERTY
  const uint8_t *_Nullable getData() const { return (const uint8_t *)Data; }

  SWIFT_COMPUTED_PROPERTY
  SwiftInt getCount() const { return Length; }

  SWIFT_COMPUTED_PROPERTY
  bool getIsEmpty() const { return Length == 0; }

  void write(BridgedOStream os) const;
};

class BridgedOwnedString {
  char *_Nonnull Data;
  size_t Length;

public:
  BridgedOwnedString(llvm::StringRef stringToCopy);
  BRIDGED_INLINE llvm::StringRef unbridgedRef() const;

  SWIFT_COMPUTED_PROPERTY
  const uint8_t *_Nonnull getData() const {
    return (const uint8_t *)(Data ? Data : "");
  }

  SWIFT_COMPUTED_PROPERTY
  SwiftInt getCount() const { return Length; }

  SWIFT_COMPUTED_PROPERTY
  bool getIsEmpty() const { return Length == 0; }

  void destroy() const;
} SWIFT_SELF_CONTAINED;

//===----------------------------------------------------------------------===//
// MARK: BridgedOptionalInt
//===----------------------------------------------------------------------===//

struct BridgedOptionalInt {
  SwiftInt value;
  bool hasValue;

#ifdef USED_IN_CPP_SOURCE
  static BridgedOptionalInt getFromAPInt(llvm::APInt i) {
    if (i.getSignificantBits() <= std::min(std::numeric_limits<SwiftInt>::digits, 64)) {
      return {(SwiftInt)i.getSExtValue(), true};
    }
    return {0, false};
  }
#endif
};

//===----------------------------------------------------------------------===//
// MARK: OStream
//===----------------------------------------------------------------------===//

class BridgedOStream {
  llvm::raw_ostream * _Nonnull os;

public:
  SWIFT_UNAVAILABLE("Use init(raw:) instead")
  BridgedOStream(llvm::raw_ostream * _Nonnull os) : os(os) {}

  SWIFT_NAME("init(raw:)")
  BridgedOStream(void *_Nonnull os)
      : os(static_cast<llvm::raw_ostream *>(os)) {}

  SWIFT_UNAVAILABLE("Use '.raw' instead")
  llvm::raw_ostream * _Nonnull unbridged() const { return os; }

  SWIFT_COMPUTED_PROPERTY
  void *_Nonnull getRaw(BridgedOStream bridged) const { return unbridged(); }

  void write(BridgedStringRef string) const;

  void newLine() const;

  void flush() const;
};

BridgedOStream Bridged_dbgs();

//===----------------------------------------------------------------------===//
// MARK: SourceLoc
//===----------------------------------------------------------------------===//

class BridgedSourceLoc {
  const void *_Nullable Raw;

public:
  BridgedSourceLoc() : Raw(nullptr) {}

  SWIFT_NAME("init(raw:)")
  BridgedSourceLoc(const void *_Nullable raw) : Raw(raw) {}

  BRIDGED_INLINE BridgedSourceLoc(swift::SourceLoc loc);

  BRIDGED_INLINE swift::SourceLoc unbridged() const;

  SWIFT_IMPORT_UNSAFE
  const void *_Nullable getOpaquePointerValue() const { return Raw; }

  SWIFT_COMPUTED_PROPERTY
  bool getIsValid() const { return Raw != nullptr; }

  SWIFT_NAME("advanced(by:)")
  BRIDGED_INLINE
  BridgedSourceLoc advancedBy(size_t n) const;
};

//===----------------------------------------------------------------------===//
// MARK: SourceRange
//===----------------------------------------------------------------------===//

class BridgedSourceRange {
public:
  SWIFT_NAME("start")
  BridgedSourceLoc Start;

  SWIFT_NAME("end")
  BridgedSourceLoc End;

  BridgedSourceRange() : Start(), End() {}

  SWIFT_NAME("init(start:end:)")
  BridgedSourceRange(BridgedSourceLoc start, BridgedSourceLoc end)
      : Start(start), End(end) {}

  BRIDGED_INLINE BridgedSourceRange(swift::SourceRange range);

  BRIDGED_INLINE swift::SourceRange unbridged() const;
};

//===----------------------------------------------------------------------===//
// MARK: BridgedCharSourceRange
//===----------------------------------------------------------------------===//

class BridgedCharSourceRange {
public:
  SWIFT_UNAVAILABLE("Use '.start' instead")
  BridgedSourceLoc Start;

  SWIFT_UNAVAILABLE("Use '.byteLength' instead")
  unsigned ByteLength;

  SWIFT_NAME("init(start:byteLength:)")
  BridgedCharSourceRange(BridgedSourceLoc start, unsigned byteLength)
      : Start(start), ByteLength(byteLength) {}

  BRIDGED_INLINE BridgedCharSourceRange(swift::CharSourceRange range);

  BRIDGED_INLINE swift::CharSourceRange unbridged() const;

  SWIFT_COMPUTED_PROPERTY
  BridgedSourceLoc getStart() const { return Start; }

  SWIFT_COMPUTED_PROPERTY
  SwiftInt getByteLength() const { return static_cast<SwiftInt>(ByteLength); }
};

//===----------------------------------------------------------------------===//
// MARK: std::vector<BridgedCharSourceRange>
//===----------------------------------------------------------------------===//

/// An opaque, heap-allocated `std::vector<CharSourceRange>`.
///
/// This type is manually memory managed. The creator of the object needs to
/// ensure that `takeUnbridged` is called to free the memory.
class BridgedCharSourceRangeVector {
  /// Opaque pointer to `std::vector<CharSourceRange>`.
  void *_Nonnull vector;

public:
  BridgedCharSourceRangeVector();

  SWIFT_NAME("append(_:)")
  void push_back(BridgedCharSourceRange range);

#ifdef USED_IN_CPP_SOURCE
  /// Returns the `std::vector<swift::CharSourceRange>` that this
  /// `BridgedCharSourceRangeVector` represents and frees the memory owned by
  /// this `BridgedCharSourceRangeVector`.
  ///
  /// No operations should be called on `BridgedCharSourceRangeVector` after
  /// `takeUnbridged` is called.
  std::vector<swift::CharSourceRange> takeUnbridged() {
    auto *vectorPtr =
        static_cast<std::vector<swift::CharSourceRange> *>(vector);
    std::vector<swift::CharSourceRange> unbridged = *vectorPtr;
    delete vectorPtr;
    return unbridged;
  }
#endif
};

//===----------------------------------------------------------------------===//
// MARK: BridgedSwiftVersion
//===----------------------------------------------------------------------===//

class BridgedSwiftVersion {
  unsigned Major;
  unsigned Minor;

public:
  BridgedSwiftVersion() : Major(0), Minor(0) {}

  BRIDGED_INLINE
  SWIFT_NAME("init(major:minor:)")
  BridgedSwiftVersion(SwiftInt major, SwiftInt minor);

  unsigned getMajor() const { return Major; }
  unsigned getMinor() const { return Minor; }
};

//===----------------------------------------------------------------------===//
// MARK: GeneratedSourceInfo
//===----------------------------------------------------------------------===//

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedGeneratedSourceFileKind {
#define MACRO_ROLE(Name, Description)                                          \
  BridgedGeneratedSourceFileKind##Name##MacroExpansion,
#include "swift/Basic/MacroRoles.def"
#undef MACRO_ROLE

  BridgedGeneratedSourceFileKindReplacedFunctionBody,
  BridgedGeneratedSourceFileKindPrettyPrinted,
  BridgedGeneratedSourceFileKindDefaultArgument,
  BridgedGeneratedSourceFileKindAttributeFromClang,

  BridgedGeneratedSourceFileKindNone,
};

//===----------------------------------------------------------------------===//
// MARK: VirtualFile
//===----------------------------------------------------------------------===//

struct BridgedVirtualFile {
  size_t StartPosition;
  size_t EndPosition;
  BridgedStringRef Name;
  ptrdiff_t LineOffset;
  size_t NamePosition;
};

//===----------------------------------------------------------------------===//
// MARK: VersionTuple
//===----------------------------------------------------------------------===//

struct BridgedVersionTuple {
  unsigned Major : 32;

  unsigned Minor : 31;
  unsigned HasMinor : 1;

  unsigned Subminor : 31;
  unsigned HasSubminor : 1;

  unsigned Build : 31;
  unsigned HasBuild : 1;

  BridgedVersionTuple()
      : Major(0), Minor(0), HasMinor(false), Subminor(0), HasSubminor(false),
        Build(0), HasBuild(false) {}
  BridgedVersionTuple(unsigned Major)
      : Major(Major), Minor(0), HasMinor(false), Subminor(0),
        HasSubminor(false), Build(0), HasBuild(false) {}
  BridgedVersionTuple(unsigned Major, unsigned Minor)
      : Major(Major), Minor(Minor), HasMinor(true), Subminor(0),
        HasSubminor(false), Build(0), HasBuild(false) {}
  BridgedVersionTuple(unsigned Major, unsigned Minor, unsigned Subminor)
      : Major(Major), Minor(Minor), HasMinor(true), Subminor(Subminor),
        HasSubminor(true), Build(0), HasBuild(false) {}
  BridgedVersionTuple(unsigned Major, unsigned Minor, unsigned Subminor,
                      unsigned Build)
      : Major(Major), Minor(Minor), HasMinor(true), Subminor(Subminor),
        HasSubminor(true), Build(Build), HasBuild(true) {}

  BridgedVersionTuple(llvm::VersionTuple);

  llvm::VersionTuple unbridged() const;
};

//===----------------------------------------------------------------------===//
// MARK: BridgedSwiftClosure
//===----------------------------------------------------------------------===//

/// Wrapping a pointer to a Swift closure `(UnsafeRawPointer?) -> Void`
/// See 'withBridgedSwiftClosure(closure:call:)' in ASTGen.
struct BridgedSwiftClosure {
  const void *_Nonnull closure;

  BRIDGED_INLINE void operator()(const void *_Nullable);
};

SWIFT_END_NULLABILITY_ANNOTATIONS

#ifndef PURE_BRIDGING_MODE
// In _not_ PURE_BRIDGING_MODE, bridging functions are inlined and therefore
// included in the header file. This is because they rely on C++ headers that
// we don't want to pull in when using "pure bridging mode".
#include "BasicBridgingImpl.h"
#endif

#endif
