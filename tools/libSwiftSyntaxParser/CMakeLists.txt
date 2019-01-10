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
    OUTPUT_NAME _InternalSwiftSyntaxParser)

add_llvm_symbol_exports(libSwiftSyntaxParser ${LLVM_EXPORTED_SYMBOL_FILE})

# Adds -dead_strip option
add_link_opts(libSwiftSyntaxParser)

set_property(TARGET libSwiftSyntaxParser APPEND_STRING PROPERTY
  COMPILE_FLAGS " -fblocks")
if(SWIFT_NEED_EXPLICIT_LIBDISPATCH)
  target_link_libraries(libSwiftSyntaxParser PRIVATE BlocksRuntime)
endif()
