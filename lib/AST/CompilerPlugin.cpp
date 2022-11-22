//===--- CompilerPlugin.cpp - Compile Plugin Support ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Compiler plugin support
//
//===----------------------------------------------------------------------===//

#include "swift/AST/CompilerPlugin.h"

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTContext.h"
#include "swift/Basic/Defer.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "llvm/Config/config.h"
#include <cstdlib>

#if !defined(_WIN32)
#include <dlfcn.h>
#endif

using namespace swift;

#define COMPILER_PLUGIN_PROTOCOL_DESCRIPTOR "$s22_CompilerPluginSupport01_aB0Mp"

#if __clang__
#define SWIFT_CC __attribute__((swiftcall))
#define SWIFT_CONTEXT __attribute__((swift_context))
#endif

namespace {
struct MetadataArrayRef {
  const void *const *metadataArray;
  ptrdiff_t size;

  ArrayRef<const void *> array() const {
    return ArrayRef<const void *>(metadataArray, (size_t)size);
  }
};
} // end anonymous namespace

using WitnessTableLookupFn = const void *(const void *type,
                                          const void *protocol);

#if SWIFT_SWIFT_PARSER
extern "C" WitnessTableLookupFn swift_conformsToProtocol;
#endif

CompilerPlugin::CompilerPlugin(const void *metadata, void *parentLibrary,
                               ASTContext &ctx)
   : metadata(metadata), parentLibrary(parentLibrary)
{
#if !SWIFT_SWIFT_PARSER
  auto *swift_conformsToProtocol = reinterpret_cast<WitnessTableLookupFn *>(
      ctx.getAddressOfSymbol("swift_conformsToProtocol"));
#endif
  void *protocolDescriptor =
      ctx.getAddressOfSymbol(COMPILER_PLUGIN_PROTOCOL_DESCRIPTOR);
  assert(swift_conformsToProtocol);
  assert(protocolDescriptor);
  witnessTable = swift_conformsToProtocol(metadata, protocolDescriptor);
  assert(witnessTable && "Type does not conform to _CompilerPlugin");
  kind = invokeKind();
}

CompilerPlugin::~CompilerPlugin() { }

namespace {
// Corresponds to Swift type `(UnsafePointer<UInt8>, Int)`.
struct CharBuffer {
  const char *data;
  ptrdiff_t length;

  StringRef str() const {
    return StringRef(data, (size_t)length);
  }

  NullTerminatedStringRef cstr() const {
    return NullTerminatedStringRef(data, (size_t)length);
  }
};

// Corresponds to Swift type `(UnsafePointer<UInt8>, Int, Int, UInt8)`.
struct DiagnosticBuffer {
  const char *message;
  ptrdiff_t length;
  ptrdiff_t position;
  uint8_t severity;

  StringRef str() const {
    return StringRef(message, (size_t)length);
  }
};

// Corresponds to Swift type
// `(UnsafePointer<(UnsafePointer<UInt8>, Int, Int)>, Int)`.
struct DiagnosticArrayBuffer {
  const DiagnosticBuffer *data;
  ptrdiff_t count;
};
}

CompilerPlugin::Kind CompilerPlugin::invokeKind() const {
#if __clang__
  using Method = SWIFT_CC Kind(
      SWIFT_CONTEXT const void *, const void *, const void *);
  auto method = getWitnessMethodUnsafe<Method>(WitnessTableEntry::Kind);
  return method(metadata, metadata, witnessTable);
#else
  llvm_unreachable("Incompatible host compiler");
#endif
}

Optional<NullTerminatedStringRef>
CompilerPlugin::invokeRewrite(
    StringRef targetModuleName, StringRef filePath, StringRef sourceFileText,
    CharSourceRange range, ASTContext &ctx,
    SmallVectorImpl<Diagnostic> &diagnostics) const {
  struct RewriteResult {
    CharBuffer code;
    DiagnosticArrayBuffer diagnostics;
  };
#if __clang__
  using Method = SWIFT_CC RewriteResult(
      const char *, ptrdiff_t,
      const char *, ptrdiff_t,
      const char *, ptrdiff_t,
      const char *, ptrdiff_t,
      SWIFT_CONTEXT const void *, const void *, const void *);
  auto method = getWitnessMethodUnsafe<Method>(WitnessTableEntry::Rewrite);
  auto result = method(
      targetModuleName.data(), (ptrdiff_t)targetModuleName.size(),
      filePath.data(), (ptrdiff_t)filePath.size(),
      sourceFileText.data(), (ptrdiff_t)sourceFileText.size(),
      range.str().data(), (ptrdiff_t)range.getByteLength(),
      metadata, metadata, witnessTable);
  SWIFT_DEFER {
    free((void*)result.diagnostics.data);
  };
  if (!result.code.data)
    return None;
  // Collect diagnostics.
  for (unsigned i = 0, n = result.diagnostics.count; i < n; ++i) {
    auto diag = result.diagnostics.data[i];
    StringRef message(diag.message, diag.length);
    diagnostics.push_back(
        {message, (unsigned)diag.position,
         (CompilerPlugin::DiagnosticSeverity)diag.severity});
  }
  return result.code.cstr();
#else
  llvm_unreachable("Incompatible host compiler");
#endif
}
