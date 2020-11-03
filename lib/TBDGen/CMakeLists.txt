set_swift_llvm_is_available()
add_swift_host_library(swiftTBDGen STATIC
  TBDGen.cpp
  TBDGenRequests.cpp
  LLVM_LINK_COMPONENTS
    demangle
    TextAPI 
)
target_link_libraries(swiftTBDGen PRIVATE
  swiftAST
  swiftIRGen
  swiftSIL)
