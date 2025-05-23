add_swift_host_library(swiftSIL STATIC
  SIL.cpp)
target_link_libraries(swiftSIL PUBLIC
  swiftDemangling)
target_link_libraries(swiftSIL PRIVATE
  swiftAST
  swiftClangImporter
  swiftSema
  swiftSerialization
  swiftSILOptimizer)

add_subdirectory(IR)
add_subdirectory(Utils)
add_subdirectory(Verifier)
add_subdirectory(Parser)

# intrinsics_gen is the LLVM tablegen target that generates the include files
# where intrinsics and attributes are declared. swiftSIL depends on these
# headers.
# For more information see the comment at the top of lib/CMakeLists.txt.
add_dependencies(swiftSIL intrinsics_gen clang-tablegen-targets)

set_swift_llvm_is_available(swiftSIL)
