//===-- tapi/Defines.h - TAPI C++ Library Defines ---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief TAPI C++ library defines.
/// \since 1.0
///
//===----------------------------------------------------------------------===//
#ifndef TAPI_DEFINES_H
#define TAPI_DEFINES_H

#define TAPI_INTERNAL tapi::internal
#define TAPI_NAMESPACE_INTERNAL_BEGIN namespace tapi { namespace internal {
#define TAPI_NAMESPACE_INTERNAL_END } }

#define TAPI_NAMESPACE_V1_BEGIN namespace tapi { inline namespace v1 {
#define TAPI_NAMESPACE_V1_END } }

#if defined(_WIN32)
#define TAPI_PUBLIC
#else
#define TAPI_PUBLIC __attribute__((visibility ("default")))
#endif

#endif // TAPI_DEFINES_H

