#include "swift/ABI/Metadata.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Reflection/TypeRefBuilder.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Remote/InProcessMemoryReader.h"

#include "Private.h"

#include <cstdio>

using namespace swift;

static std::string nameForMetadata(const Metadata *md)
{
  Demangle::__runtime::StackAllocatedDemangler<1024> dem;
  auto nodeTree = _swift_buildDemanglingForMetadata(md, dem);
  if (!nodeTree)
    return "<unknown>";

  std::string result = Demangle::__runtime::nodeToString(nodeTree);
  return result;
}

extern "C" SWIFT_CC(swift) void roundTripType(const Metadata *md) {
  // Get a name for it
  const std::string mdName = ::nameForMetadata(md);

  // Convert it to a Node tree
  Demangle::__runtime::StackAllocatedDemangler<1024> dem;
  auto nodeTree = _swift_buildDemanglingForMetadata(md, dem);

  // Mangle that
  std::string mangledName = Demangle::__runtime::mangleNode(nodeTree);

  // Look up the result
  auto result = swift_getTypeByMangledName(MetadataState::Abstract,
                              mangledName,
                              nullptr,
                              [](unsigned, unsigned){ return nullptr; },
                              [](const Metadata *, unsigned) { return nullptr; });
  if (result.isError()) {
    auto err = result.getError();
    char *errStr = err->copyErrorString();
    printf("FAIL: %s (%p) -> %s -> ERROR %s\n",
           mdName.c_str(), md, mangledName.c_str(), errStr);
    err->freeErrorString(errStr);
    nodeTree->dump();

    result = swift_getTypeByMangledNode(MetadataState::Abstract,
                                        dem,
                                        nodeTree,
                                        nullptr,
                                        [](unsigned, unsigned){ return nullptr; },
                                        [](const Metadata *, unsigned) { return nullptr; });
    if (result.isError()) {
      err = result.getError();
      char *errStr = err->copyErrorString();
      printf("=> Also failed on node: %s\n", errStr);
      err->freeErrorString(errStr);
    }
    return;
  }

  const Metadata *md2 = result.getType().getMetadata();

  std::string md2Name = "<FAIL>";

  if (md2)
    md2Name = ::nameForMetadata(md2);

  printf("%s: %s (%p) -> %s -> %s (%p)\n",
         md == md2 ? "PASS" : "FAIL",
         mdName.c_str(), md, mangledName.c_str(), md2Name.c_str(), md2);
}
