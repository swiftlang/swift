//===-- tapi/Version.h - TAPI Version Interface -----------------*- C++ -*-===*\
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Access the TAPI version information.
/// \since 1.0
///
//===----------------------------------------------------------------------===//
#ifndef TAPI_VERSION_H
#define TAPI_VERSION_H

#include <string>
#include "Defines.h"
#define TAPI_VERSION 2.0.0
#define TAPI_VERSION_MAJOR 2
#define TAPI_VERSION_MINOR 0
#define TAPI_VERSION_PATCH 0

///
/// \defgroup TAPI_VERSION Version methods
/// \ingroup TAPI_CPP_API
///
/// @{
///

namespace tapi {

///
/// \brief Access to version related information about the TAPI dynamic library.
/// \since 1.0
///
class TAPI_PUBLIC Version {
public:
  ///
  /// \name Version Number Methods
  /// @{
  ///

  ///
  /// \brief Get the major version number.
  /// \return The major version number as unsigned integer.
  /// \since 1.0
  ///
  static unsigned getMajor() noexcept;

  ///
  /// \brief Get the minor version number.
  /// \return The minor version number as unsigned integer.
  /// \since 1.0
  ///
  static unsigned getMinor() noexcept;

  ///
  /// \brief Get the patch version number.
  /// \return The patch version as unsigned integer.
  /// \since 1.0
  ///
  static unsigned getPatch() noexcept;

  ///
  /// \brief Get the library version as string.
  /// \return A string with the version number.
  /// \since 1.0
  ///
  static std::string getAsString() noexcept;

  ///
  /// \brief Get the full library name and version as string.
  /// \return A string with the program name and version number.
  /// \since 1.0
  ///
  static std::string getFullVersionAsString() noexcept;

  ///
  /// \brief Check if the current version is at least the specified version or
  ///        greater.
  /// \param[in] major The major version number to compare against.
  /// \param[in] minor The minor version number to compare against.
  /// \param[in] patch The patch version number to compare against.
  /// \return True if the current version number is at least the specified
  ///         version or greater.
  /// \since 1.0
  ///
  static bool isAtLeast(unsigned major, unsigned minor = 0,
                        unsigned patch = 0) noexcept;

  ///
  /// @}
  ///
};

} // end tapi namespace.

///
/// @}
///

#endif // TAPI_VERSION_H
