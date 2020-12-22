add_swift_host_tool(sil-func-extractor
  SILFunctionExtractor.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-func-extractor
                      PRIVATE
                        swiftClangImporter
                        swiftDemangling
                        swiftFrontend
                        swiftSerialization
                        swiftSILGen
                        swiftSILOptimizer)
if (NOT LLVM_ENABLE_ASSERTIONS)
  target_link_options(
    sil-func-extractor
    PRIVATE
    "LINKER:-weak_library"
    "LINKER:$<TARGET_FILE:swiftExternalSILVerifier>")
endif()
