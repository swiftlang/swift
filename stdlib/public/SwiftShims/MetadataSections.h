//===--- MetadataSections.h -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of the MetadataSectionsRange and 
/// MetadataSections struct, which represent, respectively,  information about  
/// an image's section, and an image's metadata information (which is composed
/// of multiple section information).
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_METADATASECTIONS_H
#define SWIFT_STDLIB_SHIMS_METADATASECTIONS_H

#include "SwiftStddef.h"
#include "SwiftStdint.h"

#ifdef __cplusplus
namespace swift { 
extern "C" {
#endif

typedef struct MetadataSectionRange {
  __swift_uintptr_t start;
  __swift_size_t length;
} MetadataSectionRange;

struct MetadataSections {
  __swift_uintptr_t version;
  __swift_uintptr_t reserved;

  struct MetadataSections *next;
  struct MetadataSections *prev;


  MetadataSectionRange swift5_protocols;
  MetadataSectionRange swift5_protocol_conformances;
  MetadataSectionRange swift5_type_metadata;
  MetadataSectionRange swift5_typeref;
  MetadataSectionRange swift5_reflstr;
  MetadataSectionRange swift5_fieldmd;
  MetadataSectionRange swift5_assocty;
  MetadataSectionRange swift5_replace;
  MetadataSectionRange swift5_replac2;
  MetadataSectionRange swift5_builtin;
  MetadataSectionRange swift5_capture;
};

#ifdef __cplusplus
} //extern "C"
} // namespace swift
#endif

#endif // SWIFT_STDLIB_SHIMS_METADATASECTIONS_H
