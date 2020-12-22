add_swift_host_tool(sil-opt
  SILOpt.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-opt
                      PRIVATE
                        swiftFrontend
                        swiftIRGen
                        swiftSIL
                        swiftSILGen
                        swiftSILOptimizer
                        # Clang libraries included to appease the linker on linux.
                        clangBasic
                        clangCodeGen
)

if (NOT LLVM_ENABLE_ASSERTIONS)
  target_link_options(
    sil-opt
    PRIVATE
    "LINKER:-weak_library"
    "LINKER:$<TARGET_FILE:swiftExternalSILVerifier>")
endif()
