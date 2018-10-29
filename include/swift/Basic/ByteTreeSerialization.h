//===--- ByteTreeSerialization.h - ByteTree serialization -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
/// \file
/// \brief Provides an interface for serializing an object tree to a custom
/// binary format called ByteTree.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_BYTETREESERIALIZATION_H
#define SWIFT_BASIC_BYTETREESERIALIZATION_H

#include "llvm/Support/BinaryStreamError.h"
#include "llvm/Support/BinaryStreamWriter.h"
#include "swift/Basic/ExponentialGrowthAppendingBinaryByteStream.h"
#include <map>

namespace {
// Only used by compiler if both template types are the same
template <typename T, T>
struct SameType;
} // anonymous namespace

namespace swift {
namespace byteTree {
class ByteTreeWriter;

using UserInfoMap = std::map<void *, void *>;

/// Add a template specialization of \c ObjectTraits for any type that
/// serializes as an object consisting of multiple fields.
template <class T>
struct ObjectTraits {
  // Must provide:

  /// Return the number of fields that will be written in \c write when
  /// \p Object gets serialized. \p UserInfo can contain arbitrary values that
  /// can modify the serialization behaviour and gets passed down from the
  /// serialization invocation.
  // static unsigned numFields(const T &Object, UserInfoMap &UserInfo);

  /// Serialize \p Object by calling \c Writer.write for all the fields of
  /// \p Object. \p UserInfo can contain arbitrary values that can modify the
  /// serialization behaviour and gets passed down from the serialization
  /// invocation.
  // static void write(ByteTreeWriter &Writer, const T &Object,
  //                   UserInfoMap &UserInfo);
};

/// Add a template specialization of \c ScalarTraits for any type that
/// serializes into a raw set of bytes.
template <class T>
struct ScalarTraits {
  // Must provide:

  /// Return the number of bytes the serialized format of \p Value will take up.
  // static unsigned size(const T &Value);

  /// Serialize \p Value by writing its binary format into \p Writer. Any errors
  /// that may be returned by \p Writer can be returned by this function and
  /// will be handled on the call-side.
  // static llvm::Error write(llvm::BinaryStreamWriter &Writer, const T &Value);
};

/// Add a template specialization of \c DirectlyEncodable for any type whose
/// serialized form is equal to its binary representation on the serializing
/// machine.
template <class T>
struct DirectlyEncodable {
  // Must provide:

  // static bool const value = true;
};

/// Add a template specialization of \c WrapperTypeTraits for any type that
/// serializes as a type that already has a specialization of \c ScalarTypes.
/// This will typically be useful for types like enums that have a 1-to-1
/// mapping to e.g. an integer.
template <class T>
struct WrapperTypeTraits {
  // Must provide:

  /// Write the serializable representation of \p Value to \p Writer. This will
  /// typically take the form \c Writer.write(convertedValue(Value), Index)
  /// where \c convertedValue has to be defined.
  // static void write(ByteTreeWriter &Writer, const T &Value, unsigned Index);
};

// Test if ObjectTraits<T> is defined on type T.
template <class T>
struct has_ObjectTraits {
  using Signature_numFields = unsigned (*)(const T &, UserInfoMap &UserInfo);
  using Signature_write = void (*)(ByteTreeWriter &Writer, const T &Object,
                                   UserInfoMap &UserInfo);

  template <typename U>
  static char test(SameType<Signature_numFields, &U::numFields> *,
                   SameType<Signature_write, &U::write> *);

  template <typename U>
  static double test(...);

public:
  static bool const value =
      (sizeof(test<ObjectTraits<T>>(nullptr, nullptr)) == 1);
};

// Test if ScalarTraits<T> is defined on type T.
template <class T>
struct has_ScalarTraits {
  using Signature_size = unsigned (*)(const T &Object);
  using Signature_write = llvm::Error (*)(llvm::BinaryStreamWriter &Writer,
                                          const T &Object);

  template <typename U>
  static char test(SameType<Signature_size, &U::size> *,
                   SameType<Signature_write, &U::write> *);

  template <typename U>
  static double test(...);

public:
  static bool const value =
      (sizeof(test<ScalarTraits<T>>(nullptr, nullptr)) == 1);
};

// Test if WrapperTypeTraits<T> is defined on type T.
template <class T>
struct has_WrapperTypeTraits {
  using Signature_write = void (*)(ByteTreeWriter &Writer, const T &Object,
                                   unsigned Index);

  template <typename U>
  static char test(SameType<Signature_write, &U::write> *);

  template <typename U>
  static double test(...);

public:
  static bool const value = (sizeof(test<WrapperTypeTraits<T>>(nullptr)) == 1);
};

class ByteTreeWriter {
private:
  /// The writer to which the binary data is written.
  llvm::BinaryStreamWriter &StreamWriter;

  /// The underlying stream of the StreamWriter. We need this reference so that
  /// we can call \c ExponentialGrowthAppendingBinaryByteStream.writeRaw
  /// which is more efficient than the generic \c writeBytes of
  /// \c llvm::BinaryStreamWriter since it avoids the arbitrary size memcopy.
  ExponentialGrowthAppendingBinaryByteStream &Stream;

  /// The number of fields this object contains. \c UINT_MAX if it has not been
  /// set yet. No member may be written to the object if expected number of
  /// fields has not been set yet.
  unsigned NumFields = UINT_MAX;

  /// The index of the next field to write. Used in assertion builds to keep
  /// track that no indicies are jumped and that the object contains the
  /// expected number of fields.
  unsigned CurrentFieldIndex = 0;

  UserInfoMap &UserInfo;

  /// The \c ByteTreeWriter can only be constructed internally. Use
  /// \c ByteTreeWriter.write to serialize a new object.
  /// \p Stream must be the underlying stream of \p SteamWriter.
  ByteTreeWriter(ExponentialGrowthAppendingBinaryByteStream &Stream,
                 llvm::BinaryStreamWriter &StreamWriter, UserInfoMap &UserInfo)
      : StreamWriter(StreamWriter), Stream(Stream), UserInfo(UserInfo) {}

  /// Write the given value to the ByteTree in the same form in which it is
  /// represented on the serializing machine.
  template <typename T>
  llvm::Error writeRaw(T Value) {
    // FIXME: We implicitly inherit the endianess of the serializing machine.
    // Since we're currently only supporting macOS that's not a problem for now.
    auto Error = Stream.writeRaw(StreamWriter.getOffset(), Value);
    StreamWriter.setOffset(StreamWriter.getOffset() + sizeof(T));
    return Error;
  }

  /// Set the expected number of fields the object written by this writer is
  /// expected to have.
  void setNumFields(uint32_t NumFields) {
    assert(NumFields != UINT_MAX &&
           "NumFields may not be reset since it has already been written to "
           "the byte stream");
    assert((this->NumFields == UINT_MAX) && "NumFields has already been set");
    // Num fields cannot exceed (1 << 31) since it would otherwise interfere
    // with the bitflag that indicates if the next construct in the tree is an
    // object or a scalar.
    assert((NumFields & ((uint32_t)1 << 31)) == 0 && "Field size too large");

    // Set the most significant bit to indicate that the next construct is an
    // object and not a scalar.
    uint32_t ToWrite = NumFields | (1 << 31);
    auto Error = writeRaw(ToWrite);
    (void)Error;
    assert(!Error);

    this->NumFields = NumFields;
  }

  /// Validate that \p Index is the next field that is expected to be written,
  /// does not exceed the number of fields in this object and that
  /// \c setNumFields has already been called.
  void validateAndIncreaseFieldIndex(unsigned Index) {
    assert((NumFields != UINT_MAX) &&
           "setNumFields must be called before writing any value");
    assert(Index == CurrentFieldIndex && "Writing index out of order");
    assert(Index < NumFields &&
           "Writing more fields than object is expected to have");

    CurrentFieldIndex++;
  }

  ~ByteTreeWriter() {
    assert(CurrentFieldIndex == NumFields &&
           "Object had more or less elements than specified");
  }

public:
  /// Write a binary serialization of \p Object to \p StreamWriter, prefixing
  /// the stream by the specified ProtocolVersion.
  template <typename T>
  typename std::enable_if<has_ObjectTraits<T>::value, void>::type
  static write(ExponentialGrowthAppendingBinaryByteStream &Stream,
               uint32_t ProtocolVersion, const T &Object,
               UserInfoMap &UserInfo) {
    llvm::BinaryStreamWriter StreamWriter(Stream);
    ByteTreeWriter Writer(Stream, StreamWriter, UserInfo);

    auto Error = Writer.writeRaw(ProtocolVersion);
    (void)Error;
    assert(!Error);

    // There always is one root. We need to set NumFields so that index
    // validation succeeds, but we don't want to serialize this.
    Writer.NumFields = 1;
    Writer.write(Object, /*Index=*/0);
  }

  template <typename T>
  typename std::enable_if<has_ObjectTraits<T>::value, void>::type
  write(const T &Object, unsigned Index) {
    validateAndIncreaseFieldIndex(Index);

    auto ObjectWriter = ByteTreeWriter(Stream, StreamWriter, UserInfo);
    ObjectWriter.setNumFields(ObjectTraits<T>::numFields(Object, UserInfo));

    ObjectTraits<T>::write(ObjectWriter, Object, UserInfo);
  }

  template <typename T>
  typename std::enable_if<has_ScalarTraits<T>::value, void>::type
  write(const T &Value, unsigned Index) {
    validateAndIncreaseFieldIndex(Index);

    uint32_t ValueSize = ScalarTraits<T>::size(Value);
    // Size cannot exceed (1 << 31) since it would otherwise interfere with the
    // bitflag that indicates if the next construct in the tree is an object
    // or a scalar.
    assert((ValueSize & ((uint32_t)1 << 31)) == 0 && "Value size too large");
    auto SizeError = writeRaw(ValueSize);
    (void)SizeError;
    assert(!SizeError);

    auto StartOffset = StreamWriter.getOffset();
    auto ContentError = ScalarTraits<T>::write(StreamWriter, Value);
    (void)ContentError;
    assert(!ContentError);
    (void)StartOffset;
    assert((StreamWriter.getOffset() - StartOffset == ValueSize) &&
           "Number of written bytes does not match size returned by "
           "ScalarTraits<T>::size");
  }

  template <typename T>
  typename std::enable_if<DirectlyEncodable<T>::value, void>::type
  write(const T &Value, unsigned Index) {
    validateAndIncreaseFieldIndex(Index);

    uint32_t ValueSize = sizeof(T);
    auto SizeError = writeRaw(ValueSize);
    (void)SizeError;
    assert(!SizeError);

    auto ContentError = writeRaw(Value);
    (void)ContentError;
    assert(!ContentError);
  }

  template <typename T>
  typename std::enable_if<has_WrapperTypeTraits<T>::value, void>::type
  write(const T &Value, unsigned Index) {
    auto LengthBeforeWrite = CurrentFieldIndex;
    WrapperTypeTraits<T>::write(*this, Value, Index);
    (void)LengthBeforeWrite;
    assert(CurrentFieldIndex == LengthBeforeWrite + 1 &&
           "WrapperTypeTraits did not call BinaryWriter.write");
  }
};

// Define serialization schemes for common types

template <>
struct DirectlyEncodable<uint8_t> {
  static bool const value = true;
};

template <>
struct DirectlyEncodable<uint16_t> {
  static bool const value = true;
};

template <>
struct DirectlyEncodable<uint32_t> {
  static bool const value = true;
};

template <>
struct WrapperTypeTraits<bool> {
  static void write(ByteTreeWriter &Writer, const bool &Value,
                    unsigned Index) {
    Writer.write(static_cast<uint8_t>(Value), Index);
  }
};

template <>
struct ScalarTraits<llvm::StringRef> {
  static unsigned size(const llvm::StringRef &Str) { return Str.size(); }
  static llvm::Error write(llvm::BinaryStreamWriter &Writer,
                           const llvm::StringRef &Str) {
    return Writer.writeFixedString(Str);
  }
};

template <>
struct ObjectTraits<llvm::NoneType> {
  // Serialize llvm::None as an object without any elements
  static unsigned numFields(const llvm::NoneType &Object,
                            UserInfoMap &UserInfo) {
    return 0;
  }

  static void write(ByteTreeWriter &Writer, const llvm::NoneType &Object,
                    UserInfoMap &UserInfo) {
    // Nothing to write
  }
};

} // end namespace byteTree
} // end namespace swift

#endif
