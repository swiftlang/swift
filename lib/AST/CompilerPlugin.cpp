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
#include "swift/AST/DiagnosticsFrontend.h"
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

#define ALLMACROS_PROPERTY_NAME "allMacros"
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

#if __clang__ && SWIFT_SWIFT_PARSER
extern "C"
void swift_ASTGen_getMacroTypes(const void *getter,
                                const void *const **resultMetatypes,
                                ptrdiff_t *resultCount);
#endif

static const void *
getMacroRegistrationPropertyGetter(void *library, StringRef moduleName,
                                   ASTContext &ctx) {
  assert(!moduleName.empty());
  // TODO: Consider using runtime lookup to get all types that conform to the
  // macro protocol instead of finding a global `allMacros` property.
  std::string name;
  // Build a mangling tree for
  // "<module name>.allMacros.getter : [Any.Type]".
  // Example:
  //   kind=Global
  //   kind=Getter
  //     kind=Variable
  //       kind=Module, text="StringifyMacro"
  //       kind=Identifier, text="allMacros"
  //       kind=Type
  //         kind=BoundGenericStructure
  //           kind=Type
  //             kind=Structure
  //               kind=Module, text="Swift"
  //               kind=Identifier, text="Array"
  //           kind=TypeList
  //             kind=Type
  //               kind=ExistentialMetatype
  //                 kind=Type
  //                   kind=ProtocolList
  //                     kind=TypeList
  {
    Demangle::Demangler D;
    auto *global = D.createNode(Node::Kind::Global);
    {
      auto *getter = D.createNode(Node::Kind::Getter);
      {
        auto *variable = D.createNode(Node::Kind::Variable);
        {
          auto *module = D.createNode(Node::Kind::Module, moduleName);
          auto *identifier = D.createNode(Node::Kind::Identifier,
                                          ALLMACROS_PROPERTY_NAME);
          auto *type = D.createNode(Node::Kind::Type);
          {
            auto *boundGenric = D.createNode(Node::Kind::BoundGenericStructure);
            {
              auto *type = D.createNode(Node::Kind::Type);
              {
                auto *structure = D.createNode(Node::Kind::Structure);
                {
                  auto *module = D.createNode(Node::Kind::Module, "Swift");
                  auto *identifier = D.createNode(Node::Kind::Identifier, "Array");
                  structure->addChild(module, D);
                  structure->addChild(identifier, D);
                }
                type->addChild(structure, D);
              }
              auto *typeList = D.createNode(Node::Kind::TypeList);
              {
                auto *type = D.createNode(Node::Kind::Type);
                {
                  auto *exMetatype = D.createNode(Node::Kind::ExistentialMetatype);
                  {
                    auto *type = D.createNode(Node::Kind::Type);
                    {
                      auto *protocolList = D.createNode(Node::Kind::ProtocolList);
                      {
                        auto *typeList = D.createNode(Node::Kind::TypeList);
                        protocolList->addChild(typeList, D);
                      }
                      type->addChild(protocolList, D);
                    }
                    exMetatype->addChild(type, D);
                  }
                  type->addChild(exMetatype, D);
                }
                typeList->addChild(type, D);
              }
              boundGenric->addChild(type, D);
              boundGenric->addChild(typeList, D);
            }
            type->addChild(boundGenric, D);
          }
          variable->addChild(module, D);
          variable->addChild(identifier, D);
          variable->addChild(type, D);
        }
        getter->addChild(variable, D);
      }
      global->addChild(getter, D);
    }
    auto mangleResult = mangleNode(global);
    assert(mangleResult.isSuccess());
    name = mangleResult.result();
  }
  return ctx.getAddressOfSymbol(name.c_str(), library);
}

void ASTContext::loadCompilerPlugins() {
  for (auto &path : SearchPathOpts.getCompilerPluginLibraryPaths()) {
    void *lib = nullptr;
#if !defined(_WIN32)
    lib = dlopen(path.c_str(), RTLD_LAZY|RTLD_LOCAL);
#endif
    if (!lib) {
      const char *errorMsg = "Unsupported platform";
#if !defined(_WIN32)
      errorMsg = dlerror();
#endif
      Diags.diagnose(SourceLoc(), diag::compiler_plugin_not_loaded, path,
                     errorMsg);
      continue;
    }
    auto moduleName = llvm::sys::path::filename(path);
#if !defined(_WIN32)
    moduleName.consume_front("lib");
#endif
    moduleName.consume_back(LTDL_SHLIB_EXT);
    auto *getter = getMacroRegistrationPropertyGetter(lib, moduleName, *this);
    if (!getter) {
      Diags.diagnose(SourceLoc(),
                     diag::compiler_plugin_missing_macro_declaration,
                     moduleName, path, ALLMACROS_PROPERTY_NAME);
      continue;
    }
    // Note: We don't currently have a way to poke at the contents of a Swift
    // array `[Any.Type]` from C++. But this should not be an issue for release
    // toolchains where user-defined macros will be used.
#if SWIFT_SWIFT_PARSER
    const void *const *metatypesAddress;
    ptrdiff_t metatypeCount;
    swift_ASTGen_getMacroTypes(getter, &metatypesAddress, &metatypeCount);
    ArrayRef<const void *> metatypes(metatypesAddress, metatypeCount);
    for (const void *metatype : metatypes) {
      auto plugin = new CompilerPlugin(metatype, lib, *this);
      auto name = plugin->getName();
      addLoadedPlugin(name, plugin);
    }
    free(const_cast<void *>((const void *)metatypes.data()));
#endif // SWIFT_SWIFT_PARSER
  }
}

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
  auto returnedName = invokeName();
  name = ctx.getIdentifier(returnedName).str();
  free(const_cast<void *>((const void *)returnedName.data()));
  kind = invokeKind();
}

CompilerPlugin::~CompilerPlugin() {
#if !defined(_WIN32)
  dlclose(parentLibrary);
#endif
}

namespace {
struct CharBuffer {
  const char *data;
  ptrdiff_t size;

  StringRef str() const {
    return StringRef(data, (size_t)size);
  }

  NullTerminatedStringRef cstr() const {
    return NullTerminatedStringRef(data, (size_t)size);
  }
};
}

StringRef CompilerPlugin::invokeName() const {
#if __clang__
  using Method = SWIFT_CC CharBuffer(
      SWIFT_CONTEXT const void *, const void *, const void *);
  auto method = getWitnessMethodUnsafe<Method>(WitnessTableEntry::Name);
  return method(metadata, metadata, witnessTable).str();
#else
  llvm_unreachable("Incompatible host compiler");
#endif
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
CompilerPlugin::invokeRewrite(StringRef targetModuleName,
                              StringRef filePath,
                              StringRef sourceFileText,
                              CharSourceRange range,
                              ASTContext &ctx) const {
#if __clang__
  using Method = SWIFT_CC CharBuffer(
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
  if (!result.data)
    return None;
  return result.cstr();
#else
  llvm_unreachable("Incompatible host compiler");
#endif
}

Optional<StringRef>
CompilerPlugin::invokeGenericSignature() const {
#if __clang__
  using Method = SWIFT_CC CharBuffer(
      SWIFT_CONTEXT const void *, const void *, const void *);
  auto method = getWitnessMethodUnsafe<Method>(
      WitnessTableEntry::GenericSignature);
  return method(metadata, metadata, witnessTable).str();
#else
  llvm_unreachable("Incompatible host compiler");
#endif
}

StringRef CompilerPlugin::invokeTypeSignature() const {
#if __clang__
  using Method = SWIFT_CC CharBuffer(
      SWIFT_CONTEXT const void *, const void *, const void *);
  auto method = getWitnessMethodUnsafe<Method>(
      WitnessTableEntry::TypeSignature);
  return method(metadata, metadata, witnessTable).str();
#else
  llvm_unreachable("Incompatible host compiler");
#endif
}

StringRef CompilerPlugin::invokeOwningModule() const {
#if __clang__
  using Method = SWIFT_CC CharBuffer(
      SWIFT_CONTEXT const void *, const void *, const void *);
  auto method = getWitnessMethodUnsafe<Method>(
      WitnessTableEntry::OwningModule);
  return method(metadata, metadata, witnessTable).str();
#else
  llvm_unreachable("Incompatible host compiler");
#endif
}

StringRef CompilerPlugin::invokeSupplementalSignatureModules() const {
#if __clang__
  using Method = SWIFT_CC CharBuffer(
      SWIFT_CONTEXT const void *, const void *, const void *);
  auto method = getWitnessMethodUnsafe<Method>(
      WitnessTableEntry::SupplementalSignatureModules);
  return method(metadata, metadata, witnessTable).str();
#else
  llvm_unreachable("Incompatible host compiler");
#endif
}
