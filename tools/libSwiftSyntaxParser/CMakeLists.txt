include(SwiftWindowsSupport)
swift_swap_compiler_if_needed("SwiftSyntaxParser")

function(create_parser_library target_name lib_name type)
  set(SYNTAX_PARSER_LIB_NAME "${lib_name}")

  set(LLVM_EXPORTED_SYMBOL_FILE
      ${CMAKE_CURRENT_SOURCE_DIR}/libSwiftSyntaxParser.exports)

  set(HAS_SWIFT_MODULES "HAS_SWIFT_MODULES")
  if ("${type}" STREQUAL relocatable_object_file)
    # The -r linker option to produce a relocatable object file is not compatible with dead-stripping, so disable it.
    set(LLVM_NO_DEAD_STRIP TRUE)
    set(SWIFT_DISABLE_DEAD_STRIPPING TRUE)
    # Because the resulting static library will be linked by another process, we don't need to add the rpath flags for the host stdlib.
    set(HAS_SWIFT_MODULES "")
  endif()
  add_swift_host_library("${target_name}" SHARED
    c-include-check.c
    libSwiftSyntaxParser.cpp
    "${HAS_SWIFT_MODULES}"
    LLVM_LINK_COMPONENTS support)
  if(NOT SWIFT_BUILT_STANDALONE AND NOT CMAKE_C_COMPILER_ID MATCHES Clang)
    add_dependencies("${target_name}" clang)
  endif()
  target_link_libraries("${target_name}" PRIVATE
    swiftParse
    swiftCompilerModules_SwiftSyntax)
  set_target_properties("${target_name}"
      PROPERTIES
      OUTPUT_NAME ${SYNTAX_PARSER_LIB_NAME})

  add_llvm_symbol_exports("${target_name}" ${LLVM_EXPORTED_SYMBOL_FILE})

  # Adds -dead_strip option
  add_link_opts("${target_name}")

  if ("${type}" STREQUAL relocatable_object_file)
    # CMake does not support building static libraries that contain transitively referenced symbols by default.
    # Perform the following workaround:
    #  * Request a build of a shared library (aka. dylib) from CMake. This makes sure that all transitive dependencies are included in the linker invocation.
    #  * Pass -r to the linker to produce a relocatable object file instead of a dylib
    #  * Run libtool (below) to generate a static library from the relocatable object file, which we install into the toolchain.
    
    #  Add autolinking directives for C(++) libraries to avoid consumers having to add them manually
    target_compile_options("${target_name}" PRIVATE
      "SHELL:-Xclang --dependent-lib=c++"
      "SHELL:-Xclang --dependent-lib=ncurses"
      "SHELL:-Xclang --dependent-lib=z"
    )

    # -Wl,-r passes -r to the linker, which causes a relocatable object file to be created instead of a dylib
    # -Wl,-x passes -x to the linker, which causes non-global symbols to not occur in the relocatable object file.
    target_link_options("${target_name}" PRIVATE -Wl,-r -Wl,-x)
    # Since we aren't producing a dylib anymore because of -Wl,-r, we should change the file extension
    set_target_properties("${target_name}" PROPERTIES SUFFIX ".o")
  endif()

  if (SWIFT_LIBPARSER_VER)
    set(SWIFTSYNTAX_PARSER_VERSION_STRING "${SWIFT_LIBPARSER_VER}")
  else()
    set(SWIFTSYNTAX_PARSER_VERSION_STRING "${SWIFT_COMPILER_VERSION}")
  endif()

  if(SWIFTSYNTAX_PARSER_VERSION_STRING)
    if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
      set(LIBPARSER_LINK_FLAGS " -Wl,-compatibility_version -Wl,1")
      set(LIBPARSER_LINK_FLAGS "${LIBPARSER_LINK_FLAGS} -Wl,-current_version -Wl,${SWIFTSYNTAX_PARSER_VERSION_STRING}")
      set_property(TARGET "${target_name}" APPEND_STRING PROPERTY
                   LINK_FLAGS ${LIBPARSER_LINK_FLAGS})
    else()
      set_target_properties("${target_name}"
        PROPERTIES
        VERSION ${SWIFTSYNTAX_PARSER_VERSION_STRING})
    endif()
  endif()

  set_property(TARGET "${target_name}" APPEND_STRING PROPERTY
    COMPILE_FLAGS " -fblocks")
  if(NOT CMAKE_SYSTEM_NAME STREQUAL Darwin)
    target_link_libraries("${target_name}" PRIVATE
      BlocksRuntime)
  endif()
endfunction()

# Use an 'internal' name, this is primarily intended for SwiftSyntax to import.
create_parser_library(libSwiftSyntaxParser _InternalSwiftSyntaxParser dylib)

add_dependencies(parser-lib libSwiftSyntaxParser)
swift_install_in_component(TARGETS libSwiftSyntaxParser
  ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}" COMPONENT parser-lib
  LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}" COMPONENT parser-lib
  RUNTIME DESTINATION "bin" COMPONENT parser-lib)
swift_install_in_component(DIRECTORY "${SWIFT_MAIN_INCLUDE_DIR}/swift-c/SyntaxParser/"
                           DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/_InternalSwiftSyntaxParser"
                           COMPONENT parser-lib)

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  # TODO: Building the parser library as a static library has only been tested on Darwin so far because we only need the static library 
  # to distribute it as an xcframework for SwiftSyntax and xcframework dependencies in SwiftPM are only supported on Darwin at the moment.
  # Building and static linking the parser library on Linux might be possible, trivial.

  # Build the parser library as a relocatable object file
  create_parser_library(libSwiftSyntaxParserRelocatableObject _InternalSwiftSyntaxParserRelocatableObject relocatable_object_file)

  # Create a static library from the relocatable object file.
  add_custom_command_target(target_name 
    CUSTOM_TARGET_NAME libSwiftSyntaxParserStatic
    COMMAND "libtool" "-static" "-o" "lib_InternalSwiftSyntaxParser.a" "$<TARGET_FILE:libSwiftSyntaxParserRelocatableObject>"
    OUTPUT lib_InternalSwiftSyntaxParser.a
    DEPENDS libSwiftSyntaxParserRelocatableObject
  )

  # Install the static library
  add_dependencies(parser-lib "${target_name}")
  swift_install_in_component(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/lib_InternalSwiftSyntaxParser.a
    DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}" COMPONENT parser-lib)
endif()
