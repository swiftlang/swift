add_subdirectory(Sources)

if(NOT SWIFT_BUILD_REGEX_PARSER_IN_COMPILER)
  message(FATAL_ERROR
    "Unable to build ASTGen without Regex Parser support. "
    "Please enable SWIFT_BUILD_REGEX_PARSER_IN_COMPILER.")
endif()

# Build the regular expression parser into the compiler itself.
file(GLOB_RECURSE _COMPILER_REGEX_PARSER_SOURCES
  "${SWIFT_PATH_TO_STRING_PROCESSING_SOURCE}/Sources/_RegexParser/*.swift")
set(COMPILER_REGEX_PARSER_SOURCES)
foreach(source ${_COMPILER_REGEX_PARSER_SOURCES})
  file(TO_CMAKE_PATH "${source}" source)
  list(APPEND COMPILER_REGEX_PARSER_SOURCES ${source})
endforeach()
message(STATUS "Using Experimental String Processing library for _CompilerRegexParser (${SWIFT_PATH_TO_STRING_PROCESSING_SOURCE}).")

add_pure_swift_host_library(_CompilerRegexParser STATIC
  "${COMPILER_REGEX_PARSER_SOURCES}"
)
