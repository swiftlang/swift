# Use an 'internal' name, this is primarily intended for SwiftSyntax to import.
set(SYNTAX_PARSER_LIB_NAME "_InternalSwiftSyntaxParser")

set(LLVM_EXPORTED_SYMBOL_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/libSwiftSyntaxParser.exports)

add_swift_host_library(libSwiftSyntaxParser SHARED
	c-include-check.c
  libSwiftSyntaxParser.cpp

  LINK_LIBRARIES
    swiftParse
)
set_target_properties(libSwiftSyntaxParser
    PROPERTIES
    OUTPUT_NAME ${SYNTAX_PARSER_LIB_NAME})

add_llvm_symbol_exports(libSwiftSyntaxParser ${LLVM_EXPORTED_SYMBOL_FILE})

# Adds -dead_strip option
add_link_opts(libSwiftSyntaxParser)

set_property(TARGET libSwiftSyntaxParser APPEND_STRING PROPERTY
  COMPILE_FLAGS " -fblocks")
if(SWIFT_NEED_EXPLICIT_LIBDISPATCH)
  target_link_libraries(libSwiftSyntaxParser PRIVATE BlocksRuntime)
endif()

swift_install_in_component(parser-lib
  FILES "${SWIFT_LIBRARY_OUTPUT_INTDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${SYNTAX_PARSER_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
  DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}")
swift_install_in_component(parser-lib
  DIRECTORY "${SWIFT_MAIN_INCLUDE_DIR}/swift-c/SyntaxParser/"
  DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SYNTAX_PARSER_LIB_NAME}")
