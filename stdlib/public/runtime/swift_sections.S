//===--- swift_sections.S -------------------------------------------------===//
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
///
/// \swift_sections.S
/// This file contains section markers for the computation of the location and
/// size of the conformances and metadata information for non-Darwin targets.
///
//===----------------------------------------------------------------------===//

#if !defined(SWIFT_BEGIN) && !defined(SWIFT_END)
#error "Define SWIFT_BEGIN or SWIFT_END to compile this file."
#endif

.macro define_sized_section name=1

#if defined(__arm__)
    .section .\()\name, "aw", %progbits
#else
    .section .\()\name, "aw", @progbits
#endif

    .p2align 3

#if defined(SWIFT_BEGIN)
    .globl .\()\name\()_start
    .protected .\()\name\()_start
.\()\name\()_start:
#if defined(__BIG_ENDIAN__)
    .long 0
    .long .\()\name\()_end - .\()\name\()_start - 8
#else
    .long .\()\name\()_end - .\()\name\()_start - 8
    .long 0
#endif
#endif

#if defined(SWIFT_END)
    .globl .\()\name\()_end
    .protected .\()\name\()_end
.\()\name\()_end:
#endif

.endm

.macro define_simple_section name=1

#if defined(SWIFT_BEGIN)
#if defined(__arm__)
    .section .\()\name, "aw", %progbits
#else
    .section .\()\name, "aw", @progbits
#endif

    // TODO .p2align 2 ?

    .globl .\()\name\()_section
    .protected .\()\name\()_section
.\()\name\()_section:
#endif

.endm

define_simple_section swift3_typeref
define_simple_section swift3_reflstr
define_simple_section swift3_fieldmd
define_simple_section swift3_assocty

define_sized_section swift2_protocol_conformances
define_sized_section swift2_type_metadata
#if defined(__arm__)
    .section .note.GNU-stack,"",%progbits
#else
    .section .note.GNU-stack,"",@progbits
#endif
