//===-- tapi/APIVersion.h - TAPI API Version Interface ----------*- C++ -*-===*\
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Access the TAPI API version information and feature availability.
/// \since 1.0
///
//===----------------------------------------------------------------------===//
#ifndef TAPI_API_VERSION_H
#define TAPI_API_VERSION_H

#include "Defines.h"

///
/// \defgroup TAPI_API_VERSION API Version and Feature methods
/// \ingroup TAPI_CPP_API
///
/// @{
///

#define TAPI_API_VERSION_MAJOR 1U
#define TAPI_API_VERSION_MINOR 4U
#define TAPI_API_VERSION_PATCH 0U

namespace tapi {

///
/// \brief Defines a list of TAPI library features.
/// \since 1.0
///
enum class Feature : unsigned {};

///
/// \brief Access to API version, feature and ABI related information about the
///        TAPI dynamic library.
/// \since 1.0
///
class TAPI_PUBLIC APIVersion {
public:
  ///
  /// \name API Version Number Methods
  /// @{
  ///

  ///
  /// \brief Get the major API version number.
  /// \return The major API version number as unsigned integer.
  /// \since 1.0
  ///
  static unsigned getMajor() noexcept;

  ///
  /// \brief Get the minor API version number.
  /// \return The minor API version number as unsigned integer.
  /// \since 1.0
  ///
  static unsigned getMinor() noexcept;

  ///
  /// \brief Get the patch API version number.
  /// \return The patch API version as unsigned integer.
  /// \since 1.0
  ///
  static unsigned getPatch() noexcept;

  ///
  /// \brief Check if the current API version is at least the specified API
  ///        version or greater.
  /// \param[in] major The major API version number to compare against.
  /// \param[in] minor The minor API version number to compare against.
  /// \param[in] patch The patch API version number to compare against.
  /// \return True if the current API version number is at least the specified
  ///         version or greater.
  /// \since 1.0
  ///
  static bool isAtLeast(unsigned major, unsigned minor = 0,
                        unsigned patch = 0) noexcept;

  ///
  /// @}
  ///

  ///
  /// \name Feature Methods
  /// @{
  ///

  ///
  /// \brief Check if the library supports a particular #Feature.
  /// \param[in] feature The #Feature to be queried for.
  /// \return True if \a feature is supported.
  /// \since 1.0
  ///
  static bool hasFeature(Feature feature) noexcept;

  ///
  /// @}
  ///

  ///
  /// \name ABI Methods
  /// @{
  ///

  ///
  /// \brief Check if the library supports the specified ABI version.
  /// \param[in] abiVersion The ABI version to query for.
  /// \return True if the library supports the ABI version \a abiVersion.
  /// \since 1.0
  ///
  static bool hasABI(unsigned abiVersion) noexcept;

  ///
  /// @}
  ///
};

} // end tapi namespace.

///
/// @}
///

#endif // TAPI_API_VERSION_H
