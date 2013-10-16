//===--- Dwarf.h - DWARF constants ------------------------------*- C++ -*-===//
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
//
// This file defines several temporary Swift-specific DWARF constants.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DWARF_H
#define SWIFT_BASIC_DWARF_H

#include "llvm/Support/Dwarf.h"

namespace swift {

//===--- BEGIN "TO BE FIXED IN A SWIFT BRANCH OF LLVM" ---===//
//
// DWARF constants.
//
// The first unused language value in DWARF v5 is DW_LANG_Haskell+1 =
// 0x19.  We can't use it, because LLVM asserts that there are no
// languages >DW_LANG_Python=0x14.  Wouldn't it would be much more
// appropriate to use a constant in DW_LANG_lo_user..DW_LANG_hi_user
// anyway, you may ask? Well, CompileUnit::constructTypeDIE() will
// always use a DW_FORM_data1, which is too small for that range!  And
// by fixing that in LLVM we would hint at developing a new language.
// So instead, let's hijack a language with a very low potential for
// accidental conflicts for now.
//
// NOTE: Ocaml might beat us: http://www.dwarfstd.org/ShowIssue.php?issue=131009.1
//
typedef enum {
  DW_LANG_Swift = 0xf,
  DW_LANG_ObjC = llvm::dwarf::DW_LANG_ObjC, // For symmetry.
  // Reuse some existing tag so the verifier doesn't complain.
  DW_TAG_meta_type = llvm::dwarf::DW_TAG_restrict_type
} dwarf;
//
//===--------- END "TO BE FIXED IN A SWIFT BRANCH OF LLVM" --------===//

}

#endif
