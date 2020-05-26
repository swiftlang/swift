add_swift_host_tool(sil-passpipeline-dumper
  SILPassPipelineDumper.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(sil-passpipeline-dumper PRIVATE
  swiftFrontend
  swiftIRGen
  swiftSILGen
  swiftSILOptimizer
  # Clang libraries included to appease the linker on linux.
  clangBasic
  clangCodeGen)
