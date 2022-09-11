add_swift_host_library(swiftConstExtract STATIC
  ConstExtract.cpp
  )
target_link_libraries(swiftConstExtract PRIVATE
  swiftClangImporter
  swiftAST)

set_swift_llvm_is_available(swiftConstExtract)
