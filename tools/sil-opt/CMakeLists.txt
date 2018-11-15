add_swift_host_tool(sil-opt
  SILOpt.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftIRGen
    swiftSILGen
    swiftSILOptimizer
    # Clang libraries included to appease the linker on linux.
    clangBasic
    clangCodeGen
  SWIFT_COMPONENT tools
)
