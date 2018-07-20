//===-- Windows/SwiftWinDbg.cpp - Extension for WinDbg ----------*- C++ -*-===//
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
///
/// \file
/// This is an extension used for reading Swift strings in WinDbg.
///
//===----------------------------------------------------------------------===//
#define WIN32_LEAN_AND_MEAN // Exclude rarely-used stuff from Windows headers
#include <windows.h>
#define KDEXT_64BIT
#include <dbgeng.h>
#include <wdbgexts.h>
#include <stdint.h>

WINDBG_EXTENSION_APIS ExtensionApis = {0};

extern "C" _declspec(dllexport)
LPEXT_API_VERSION WDBGAPI ExtensionApiVersion(void) {
  static EXT_API_VERSION g_ExtApiVersion = {1, 0, EXT_API_VERSION_NUMBER64, 0};
  return &g_ExtApiVersion;
}

extern "C" _declspec(dllexport)
void WDBGAPI WinDbgExtensionDllInit(PWINDBG_EXTENSION_APIS64 lpExtensionApis,
                                    unsigned short MajorVersion,
                                    unsigned short MinorVersion) {
  ExtensionApis = *lpExtensionApis;
}

/// Returns a pointer to the string extracted from a Swift String.
/// If nullptr is returned, `PayloadAddress` holds the debugee address of the
/// string if nonzero. If nullptr is returned and `PayloadAddress` is zero
/// then the string was not successfully extracted. `Count` holds the number
/// of characters in the string.
char *extractString(uint64_t StringObject, uint64_t StringOtherBits,
                    uint64_t &Count, uint64_t &PayloadAddress) {
  // See StringGuts_SummaryProvider() at
  // swift-lldb/source/Plugins/Language/Swift/SwiftFormatters.cpp
  PayloadAddress = 0;
  bool isValue = StringObject & (1ULL << 63);
  bool isCocoaOrSmall = StringObject & (1ULL << 62);
  bool isOpaque = StringObject & (1ULL << 61);
  bool isUTF16 = StringObject & (1ULL << 60);

  if (!isOpaque && !isCocoaOrSmall) {
    // Handle native Swift string
    PayloadAddress = StringObject & ((1ULL << 56) - 1);
    Count = StringOtherBits & ((1ULL << 48) - 1);
    if (!isValue)
      PayloadAddress += 32;
    return nullptr;
  } else if (isCocoaOrSmall && !isValue) {
    dprintf("Strings which point to NSStrings are not implemented\n");
    return nullptr;
  } else if (isCocoaOrSmall && isValue && !isOpaque) {
    dprintf("Strings which contain an NSString inline are not implemented\n");
    return nullptr;
  } // else if (isCocoaOrSmall && isValue && isOpaque)
  // Handle UTF-8 string stored in registers
  // _object = isSmallUTF8Nibble << 60 | count << 56 | c_14 << 48 | ... | c_8
  // _otherBits = c_7 << 56 | c_6 << 48 | ... | c_0
  Count = (StringObject & 0x0F00000000000000) >> 56;
  char *StringBuffer = (char *)malloc(Count+1);
  for (uint64_t idx = 0; idx < Count; idx++) {
    if (idx < 8)
      StringBuffer[idx] = (StringOtherBits >> (idx * 8)) & 0xFF;
    else
      StringBuffer[idx] = (StringObject >> ((idx - 8) * 8)) & 0xFF;
  }
  StringBuffer[Count] = '\0';
  return StringBuffer;
}

extern "C" _declspec(dllexport)
void help(void *hCurrentProcess, void *hCurrentThread, uint64_t dwCurrentPc,
          unsigned long dwProcessor, const char *args) {
  dprintf("Swift WinDbg Extension\n");
  dprintf("Author: Ellis Hoag\n\n");
  dprintf("\treadstring <variable> - Read the contents of a Swift String\n");
}

extern "C" _declspec(dllexport)
void readstring(void *hCurrentProcess, void *hCurrentThread,
                uint64_t dwCurrentPc, unsigned long dwProcessor,
                const char *args) {
  uint64_t StringGuts = GetExpression(args);
  if (StringGuts == 0) {
    dprintf("Could not find address of %s\n", args);
    return;
  }
  const char *Name = args;

  uint64_t StringObject;
  if (GetFieldValue(StringGuts, "Swift::_StringGuts", "_object",
                    StringObject)) {
    dprintf("Unable to get _object field from Swift::_StringGuts at %p\n",
            StringGuts);
    return;
  }

  uint64_t StringOtherBits;
  if (GetFieldValue(StringGuts, "Swift::_StringGuts", "_otherBits",
                    StringOtherBits)) {
    dprintf("Unable to get _otherBits field from Swift::_StringGuts at %p\n",
            StringGuts);
    return;
  }

  uint64_t Count, PayloadAddress;
  char *StringContents = extractString(StringObject, StringOtherBits, Count,
                                       PayloadAddress);
  if (StringContents || PayloadAddress) {
    if (PayloadAddress) {
      StringContents = (char *)malloc(Count + 1);
      if (!ReadMemory(PayloadAddress, StringContents, Count, NULL)) {
        dprintf("Could not read string at %p\n", PayloadAddress);
        free(StringContents);
        return;
      }
      StringContents[Count] = '\0';
    }
    dprintf("%p %s = \"%s\"\n", StringGuts, Name, StringContents);
    free(StringContents);
  }
}
