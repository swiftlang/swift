//===--- BCRecordLayout.h - Convenience wrappers for bitcode ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file Convenience wrappers for the LLVM bitcode format and bitstream APIs.
///
/// This allows you to use a sort of DSL to declare and use bitcode abbrevs
/// and records. Example:
///
/// \code
///     using Metadata = BCRecordLayout<
///       METADATA_ID, // ID
///       BCFixed<16>, // Module format major version
///       BCFixed<16>, // Module format minor version
///       BCBlob // misc. version information
///     >;
///     unsigned MetadataAbbrevCode = Metadata::emitAbbrev(Out);
///     Metadata::emitRecord(Out, ScratchRecord, MetadataAbbrevCode,
///                          VERSION_MAJOR, VERSION_MINOR, extraData);
/// \endcode
///
/// For details on the bitcode format, see
///   http://llvm.org/docs/BitCodeFormat.html
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_BCRECORDLAYOUT_H
#define SWIFT_SERIALIZATION_BCRECORDLAYOUT_H

#include "swift/Basic/LLVM.h"
#include "llvm/Bitcode/BitCodes.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/MathExtras.h"

namespace swift {
namespace serialization {

namespace impl {
  /// Convenience base for all kinds of bitcode abbreviation fields.
  ///
  /// This just defines common properties queried by the metaprogramming.
  template<bool LAST = false>
  class BCField {
  public:
    static const bool MUST_BE_LAST = LAST;

    template<typename T>
    static bool assertValid(const T &data) {}
  };
} // end namespace impl


/// Represents a literal operand in a bitcode record.
///
/// The value of a literal operand is the same for all instances of the record,
/// so it is only emitted in the abbreviation definition.
///
/// Note that because this uses a compile-time template, you cannot have a
/// literal operand that is fixed at run-time without dropping down to the
/// raw LLVM APIs.
template<uint64_t Value>
class BCLiteral : public impl::BCField<> {
public:
  static void emitOp(llvm::BitCodeAbbrev &abbrev) {
    abbrev.Add(llvm::BitCodeAbbrevOp(Value));
  }

  template<typename T>
  static void assertValid(const T &data) {
    assert(data == Value && "data value does not match declared literal value");
  }
};

/// Represents a fixed-width value in a bitcode record.
///
/// Note that the LLVM bitcode format only supports unsigned values.
template<uint64_t Width>
class BCFixed : public impl::BCField<> {
public:
  static void emitOp(llvm::BitCodeAbbrev &abbrev) {
    abbrev.Add(llvm::BitCodeAbbrevOp(llvm::BitCodeAbbrevOp::Fixed, Width));
  }

  template<typename T>
  static void assertValid(const T &data) {
    assert(data >= 0 && "cannot encode signed integers");
    assert(llvm::isUInt<Width>(data) &&
           "data value does not fit in the given bit width");
  }
};

/// Represents a variable-width value in a bitcode record.
///
/// The \p Width parameter should include the continuation bit.
///
/// Note that the LLVM bitcode format only supports unsigned values.
template<uint64_t Width>
class BCVBR : public impl::BCField<> {
  static_assert(Width >= 2, "width does not have room for continuation bit");

public:
  static void emitOp(llvm::BitCodeAbbrev &abbrev) {
    abbrev.Add(llvm::BitCodeAbbrevOp(llvm::BitCodeAbbrevOp::VBR, Width));
  }

  template<typename T>
  static void assertValid(const T &data) {
    assert(data >= 0 && "cannot encode signed integers");
  }
};

/// Represents a character encoded in LLVM's Char6 encoding.
///
/// This format is suitable for encoding decimal numbers (without signs or
/// exponents) and C identifiers (without dollar signs), but not much else.
///
/// \sa http://llvm.org/docs/BitCodeFormat.html#char6-encoded-value
class BCChar6 : public impl::BCField<> {
public:
  static void emitOp(llvm::BitCodeAbbrev &abbrev) {
    abbrev.Add(llvm::BitCodeAbbrevOp(llvm::BitCodeAbbrevOp::Char6));
  }

  template<typename T>
  static void assertValid(const T &data) {
    assert(llvm::BitCodeAbbrevOp::isChar6(data) && "invalid Char6 data");
  }
};

/// Represents an untyped blob of bytes.
///
/// If present, this must be the last field in a record.
class BCBlob : public impl::BCField<true> {
public:
  static void emitOp(llvm::BitCodeAbbrev &abbrev) {
    abbrev.Add(llvm::BitCodeAbbrevOp(llvm::BitCodeAbbrevOp::Blob));
  }
};

/// Represents an array of some other type.
///
/// If present, this must be the last field in a record.
template<typename Element>
class BCArray : public impl::BCField<true> {
  static_assert(!std::is_same<Element, BCBlob>::value,
                "arrays of blobs are not permitted");
public:
  static void emitOp(llvm::BitCodeAbbrev &abbrev) {
    abbrev.Add(llvm::BitCodeAbbrevOp(llvm::BitCodeAbbrevOp::Array));
    Element::emitOp(abbrev);
  }
};


namespace impl {
  /// Attaches the last field to an abbreviation.
  ///
  /// This is the base case for \c emitOps.
  ///
  /// \sa BCRecordLayout::emitAbbrev
  template<typename Last>
  static void emitOps(llvm::BitCodeAbbrev &abbrev) {
    Last::emitOp(abbrev);
  }

  /// Attaches fields to an abbreviation.
  ///
  /// This is the recursive case for \c emitOps.
  ///
  /// \sa BCRecordLayout::emitAbbrev
  template<typename First, typename... Rest>
  static typename std::enable_if<sizeof...(Rest) != 0, void>::type
  emitOps(llvm::BitCodeAbbrev &abbrev) {
    static_assert(!First::MUST_BE_LAST,
                  "arrays and blobs may not appear in the middle of a record");
    First::emitOp(abbrev);
    emitOps<Rest...>(abbrev);
  }


  /// Helper class for emitting a scalar element in the middle of a record.
  ///
  /// \sa BCRecordLayout::emitRecord
  template<typename First, typename... Fields>
  class BCRecordWriter {
  public:
    template <typename BufferTy, typename FirstData, typename... Data>
    static typename std::enable_if<sizeof...(Fields) != 0, void>::type
    emit(llvm::BitstreamWriter &out, BufferTy &buffer, unsigned abbrCode,
         FirstData data, Data... rest) {
      static_assert(!First::MUST_BE_LAST,
                    "arrays and blobs may not appear in the middle of a record");
      First::assertValid(data);
      buffer.push_back(data);
      BCRecordWriter<Fields...>::emit(out, buffer, abbrCode, rest...);
    }
  };

  /// Helper class for emitting a scalar element at the end of a record.
  ///
  /// This has a separate implementation because up until now we've only been
  /// \em building the record (into a data buffer), and now we need to hand it
  /// off to the BitstreamWriter to be emitted.
  ///
  /// \sa BCRecordLayout::emitRecord
  template<typename Last>
  class BCRecordWriter<Last> {
    template <typename BufferTy, typename LastData>
    static void emit(llvm::BitstreamWriter &out, BufferTy &buffer,
                     unsigned abbrCode, LastData data) {
      static_assert(!Last::MUST_BE_LAST,
                    "arrays and blobs need special handling");
      Last::assertValid(data);
      buffer.push_back(data);
      out.EmitRecordWithAbbrev(abbrCode, buffer);
    }
  };

  /// Helper class for emitting an array element at the end of a record.
  ///
  /// \sa BCRecordLayout::emitRecord
  template<typename EleTy>
  class BCRecordWriter<BCArray<EleTy>> {
  public:
    template <typename BufferTy>
    static void emit(llvm::BitstreamWriter &out, BufferTy &buffer,
                     unsigned abbrCode, StringRef arrayData) {
      // FIXME: validate array data.
      out.EmitRecordWithArray(abbrCode, buffer, arrayData);
    }

    template <typename BufferTy, typename ArrayTy>
    static void emit(llvm::BitstreamWriter &out, BufferTy &buffer,
                     unsigned abbrCode, const ArrayTy &arrayData) {
#ifndef NDEBUG
      for (auto &item : arrayData)
        EleTy::assertValid(item);
#endif
      buffer.reserve(buffer.size() + arrayData.size());
      std::copy(arrayData.begin(), arrayData.end(),
                std::back_inserter(buffer));
      out.EmitRecordWithAbbrev(abbrCode, buffer);
    }
  };

  /// Helper class for emitting a blob element at the end of a record.
  ///
  /// \sa BCRecordLayout::emitRecord
  template<>
  class BCRecordWriter<BCBlob> {
  public:
    template <typename BufferTy>
    static void emit(llvm::BitstreamWriter &out, BufferTy &buffer,
                     unsigned abbrCode, StringRef blobData) {
      out.EmitRecordWithBlob(abbrCode, buffer, blobData);
    }
  };
} // end namespace impl

/// Represents a single bitcode record type.
///
/// This class template is meant to be instantiated and then given a name,
/// so that from then on that name can be used 
template<unsigned RecordCode, typename... Fields>
class BCRecordLayout {
  llvm::BitstreamWriter &Out;
  unsigned AbbrevCode;

public:
  enum : unsigned {
    /// The record code associated with this layout.
    Code = RecordCode
  };

  /// Create a layout and register it with the given bitstream writer.
  explicit BCRecordLayout(llvm::BitstreamWriter &out)
    : Out(out), AbbrevCode(emitAbbrev(out)) {}

  /// Emit a record to the bitstream writer, using the given buffer for scratch
  /// space.
  ///
  /// Note that even fixed arguments must be specified here. Currently, arrays
  /// and blobs can only be passed as StringRefs.
  template <typename BufferTy, typename... Data>
  void emit(BufferTy &buffer, Data... data) {
    emitRecord(Out, buffer, AbbrevCode, data...);
  }

  /// Registers this record's layout with the bitstream reader.
  ///
  /// \returns The abbreviation code for the newly-registered record type.
  static unsigned emitAbbrev(llvm::BitstreamWriter &out) {
    auto *abbrev = new llvm::BitCodeAbbrev();
    impl::emitOps<BCLiteral<RecordCode>, Fields...>(*abbrev);
    return out.EmitAbbrev(abbrev);
  }

  /// Emit a record identified by \p abbrCode to bitstream reader \p out, using
  /// \p buffer for scratch space.
  ///
  /// Note that even fixed arguments must be specified here. Currently, arrays
  /// and blobs can only be passed as StringRefs.
  template <typename BufferTy, typename... Data>
  static void emitRecord(llvm::BitstreamWriter &out, BufferTy &buffer,
                         unsigned abbrCode, Data... data) {
    static_assert(sizeof...(data) <= sizeof...(Fields),
                  "Too many record elements");
    static_assert(sizeof...(data) >= sizeof...(Fields),
                  "Too few record elements");
    buffer.clear();
    buffer.push_back(RecordCode);
    impl::BCRecordWriter<Fields...>::emit(out, buffer, abbrCode, data...);
  }
};

/// RAII object to pair entering and exiting a sub-block.
class BCBlockRAII {
  llvm::BitstreamWriter &Writer;
public:
  BCBlockRAII(llvm::BitstreamWriter &writer, unsigned blockID,
              unsigned abbrevLen)
    : Writer(writer) {
    writer.EnterSubblock(blockID, abbrevLen);
  }

  ~BCBlockRAII() {
    Writer.ExitBlock();
  }
};

} // end namespace serialization
} // end namespace swift

#endif
