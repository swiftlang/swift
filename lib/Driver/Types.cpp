//===--- Types.cpp - Driver input & temporary type information ------------===//
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

#include "swift/Driver/Types.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;
using namespace swift::driver;
using namespace swift::driver::types;

struct TypeInfo {
  const char *Name;
  const char *Flags;
  const char *TempSuffix;
};

static const TypeInfo TypeInfos[] = {
#define TYPE(NAME, ID, TEMP_SUFFIX, FLAGS) \
  { NAME, FLAGS, TEMP_SUFFIX },
#include "swift/Driver/Types.def"
#undef TYPE
};
static const unsigned numTypes = llvm::array_lengthof(TypeInfos);

static const TypeInfo &getInfo(unsigned Id) {
  assert(Id > 0 && Id - 1 < numTypes && "Invalid Type ID.");
  return TypeInfos[Id - 1];
}

StringRef types::getTypeName(ID Id) {
  return getInfo(Id).Name;
}

StringRef types::getTypeTempSuffix(ID Id) {
  return getInfo(Id).TempSuffix;
}

ID types::lookupTypeForExtension(StringRef Ext) {
  return llvm::StringSwitch<types::ID>(Ext)
           .Case("swift", TY_Swift)
           .Case("swiftmodule", TY_SwiftModuleFile)
           .Case("pcm", TY_ClangModuleFile)
           .Case("o", TY_Object)
           .Default(TY_INVALID);
}
