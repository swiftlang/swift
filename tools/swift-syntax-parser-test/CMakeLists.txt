include(SwiftWindowsSupport)
swift_swap_compiler_if_needed("SwiftSyntaxParserTest")

add_swift_host_tool(swift-syntax-parser-test
  swift-syntax-parser-test.cpp
  LLVM_LINK_COMPONENTS
    Support
  SWIFT_COMPONENT tools
)
if(CMAKE_SYSTEM_NAME STREQUAL Darwin)
  set_target_properties(swift-syntax-parser-test PROPERTIES
    BUILD_WITH_INSTALL_RPATH YES
    INSTALL_RPATH @executable_path/../lib)
elseif(NOT CMAKE_SYSTEM_NAME STREQUAL Windows)
  set_target_properties(swift-syntax-parser-test PROPERTIES
    INSTALL_RPATH ${SWIFT_LIBRARY_OUTPUT_INTDIR})
endif()
target_compile_options(swift-syntax-parser-test PRIVATE
  -fblocks)
target_link_libraries(swift-syntax-parser-test PRIVATE
  $<$<NOT:$<PLATFORM_ID:Darwin>>:BlocksRuntime>
  libSwiftSyntaxParser)

if(NOT SWIFT_BUILT_STANDALONE AND NOT CMAKE_C_COMPILER_ID MATCHES Clang)
  add_dependencies(swift-syntax-parser-test clang)
endif()
