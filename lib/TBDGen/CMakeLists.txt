add_swift_host_library(swiftTBDGen STATIC
  TBDGen.cpp
  LLVM_LINK_COMPONENTS
    demangle
    TextAPI 
)
target_link_libraries(swiftTBDGen PRIVATE
  swiftAST
  swiftSIL)
