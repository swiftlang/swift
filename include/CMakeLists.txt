add_subdirectory(swift)


# Create a library that depends on all headers defined in include/swift/module.modulemap
#
# TODO: generate this dynamically through the modulemap; this cannot use `sed`
# as that is not available on all platforms (e.g. Windows).
file(GENERATE
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/HeaderDependencies.cpp"
  CONTENT "
#define COMPILED_WITH_SWIFT
#define SWIFT_TARGET

#include \"swift/Basic/BasicBridging.h\"
#include \"swift/AST/ASTBridging.h\"
#include \"swift/IDE/IDEBridging.h\"
#include \"swift/Parse/ParseBridging.h\"
#include \"swift/SIL/SILBridging.h\"
#include \"swift/SILOptimizer/OptimizerBridging.h\"
")
add_library(importedHeaderDependencies "${CMAKE_CURRENT_BINARY_DIR}/HeaderDependencies.cpp")

# When building unified, we need to make sure all the Clang headers are
# generated before we try to use them.
# When building Swift against an already compiled LLVM/Clang, like
# build-script does, this target does not exist, but the headers
# are already completely generated at that point.
if(TARGET clang-tablegen-targets)
  add_dependencies(importedHeaderDependencies clang-tablegen-targets)
endif()
