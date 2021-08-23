
add_swift_host_library(swiftTBDGen STATIC
  APIGen.cpp
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

set_swift_llvm_is_available(swiftTBDGen)
