//===-- tapi/PackedVersion32.h - TAPI Packed Version 32 ---------*- C++ -*-===*\
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the packed version number.
/// \since 1.0
///
//===----------------------------------------------------------------------===//
#ifndef TAPI_PACKED_VERSION_32_H
#define TAPI_PACKED_VERSION_32_H

#include "Defines.h"

///
/// \defgroup TAPI_PACKED_VERSION_32 Packed Version handling
/// \ingroup TAPI_CPP_API
///
/// @{
///

TAPI_NAMESPACE_V1_BEGIN

///
/// Packed Version Number Encoding.
///
/// The Mach-O version numbers are commonly encoded as a 32bit value, where the
/// upper 16 bit quantity is used for the major version number and the lower two
/// 8 bit quantities as minor version number and patch version number.
///
/// \since 1.0
///
class TAPI_PUBLIC PackedVersion32 {
private:
  uint32_t _version;

public:
  ///
  /// Default construct a PackedVersion32.
  /// \since 1.0
  ///
  PackedVersion32() = default;

  ///
  /// Construct a PackedVersion32 with a raw value.
  /// \since 1.0
  ///
  PackedVersion32(uint32_t rawVersion) : _version(rawVersion) {}

  ///
  /// Construct a PackedVersion32 with the provided major, minor, and
  /// patch version number.
  /// \since 1.0
  ///
  PackedVersion32(unsigned major, unsigned minor, unsigned patch)
      : _version((major << 16) | ((minor & 0xff) << 8) | (patch & 0xff)) {}

  ///
  /// Get the major version number.
  /// \return The major version number as unsigned integer.
  /// \since 1.0
  ///
  unsigned getMajor() const { return _version >> 16; }

  ///
  /// Get the minor version number.
  /// \return The minor version number as unsigned integer.
  /// \since 1.0
  ///
  unsigned getMinor() const { return (_version >> 8) & 0xff; }

  ///
  /// Get the patch version number.
  /// \return The patch version number as unsigned integer.
  /// \since 1.0
  ///
  unsigned getPatch() const { return _version & 0xff; }

  bool operator<(const PackedVersion32 &rhs) const {
    return _version < rhs._version;
  }

  bool operator==(const PackedVersion32 &rhs) const {
    return _version == rhs._version;
  }

  bool operator!=(const PackedVersion32 &rhs) const {
    return _version != rhs._version;
  }

  operator unsigned() const { return _version; }
};

TAPI_NAMESPACE_V1_END

///
/// @}
///

#endif // TAPI_PACKED_VERSION_32_H
