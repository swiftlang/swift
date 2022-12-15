if (SWIFT_SWIFT_PARSER)
  # Ensure that we do not link the _StringProcessing module. But we can
  # only pass this flag for new-enough compilers that support it.
  file(WRITE "${CMAKE_BINARY_DIR}/tmp/empty-check-string-processing.swift" "")
  execute_process(
    COMMAND
      "${CMAKE_Swift_COMPILER}"
      -Xfrontend -disable-implicit-string-processing-module-import
      -c - -o /dev/null
    INPUT_FILE
      "${CMAKE_BINARY_DIR}/tmp/empty-check-string-processing.swift"
    OUTPUT_QUIET ERROR_QUIET
    RESULT_VARIABLE
      SWIFT_SUPPORTS_DISABLE_IMPLICIT_STRING_PROCESSING_MODULE_IMPORT)
  if (NOT SWIFT_SUPPORTS_DISABLE_IMPLICIT_STRING_PROCESSING_MODULE_IMPORT)
    add_compile_options(
      $<$<COMPILE_LANGUAGE:Swift>:-Xfrontend>
      $<$<COMPILE_LANGUAGE:Swift>:-disable-implicit-string-processing-module-import>)
  endif()

  add_library(swiftASTGen STATIC
    Sources/ASTGen/ASTGen.swift
    Sources/ASTGen/Decls.swift
    Sources/ASTGen/Diagnostics.swift
    Sources/ASTGen/Exprs.swift
    Sources/ASTGen/Generics.swift
    Sources/ASTGen/Literals.swift
    Sources/ASTGen/Macros.swift
    Sources/ASTGen/Misc.swift
    Sources/ASTGen/SourceFile.swift
    Sources/ASTGen/Stmts.swift
    Sources/ASTGen/Types.swift
  )

  # Set the appropriate target triple.
  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set(DEPLOYMENT_VERSION "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")
  endif()

  if(SWIFT_HOST_VARIANT_SDK STREQUAL ANDROID)
    set(DEPLOYMENT_VERSION ${SWIFT_ANDROID_API_LEVEL})
  endif()

  get_target_triple(target target_variant "${SWIFT_HOST_VARIANT_SDK}" "${SWIFT_HOST_VARIANT_ARCH}"
    MACCATALYST_BUILD_FLAVOR ""
    DEPLOYMENT_VERSION "${DEPLOYMENT_VERSION}")

    target_compile_options(swiftASTGen PRIVATE $<$<COMPILE_LANGUAGE:Swift>:-target;${target}>)

  # Workaround a cmake bug, see the corresponding function in swift-syntax
  function(force_target_link_libraries TARGET)
    cmake_parse_arguments(ARGS "" "" "PUBLIC" ${ARGN})

    foreach(DEPENDENCY ${ARGS_PUBLIC})
      target_link_libraries(${TARGET} PRIVATE
        ${DEPENDENCY}
      )
      add_dependencies(${TARGET} ${DEPENDENCY})

      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
        COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
        DEPENDS ${DEPENDENCY}
        )
      target_sources(${TARGET} PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
      )
    endforeach()
  endfunction()

  set(SWIFT_SYNTAX_MODULES
    SwiftBasicFormat
    SwiftParser
    SwiftParserDiagnostics
    SwiftDiagnostics
    SwiftSyntax
    SwiftOperators
    SwiftSyntaxBuilder
    _SwiftSyntaxMacros
  )

  # Compute the list of SwiftSyntax targets
  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND "SwiftSyntax::"
       OUTPUT_VARIABLE SWIFT_SYNTAX_TARGETS)

  # TODO: Change to target_link_libraries when cmake is fixed
  force_target_link_libraries(swiftASTGen PUBLIC
    ${SWIFT_SYNTAX_TARGETS}
  )
  target_link_libraries(swiftASTGen PUBLIC
    swiftAST
  )

  set(SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR
      "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/lib/swift/host")
  set(SWIFT_SYNTAX_LIBRARIES_DEST_DIR
      "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/swift/host")

  # Determine the SwiftSyntax shared library files that were built as
  # part of earlyswiftsyntax.
  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND ${CMAKE_SHARED_LIBRARY_PREFIX}
       OUTPUT_VARIABLE SWIFT_SYNTAX_SHARED_LIBRARIES)
  list(TRANSFORM SWIFT_SYNTAX_SHARED_LIBRARIES APPEND
       ${CMAKE_SHARED_LIBRARY_SUFFIX}
       OUTPUT_VARIABLE SWIFT_SYNTAX_SHARED_LIBRARIES)

  # Copy over all of the shared libraries from earlyswiftsyntax so they can
  # be found via RPATH.
  foreach (sharedlib ${SWIFT_SYNTAX_SHARED_LIBRARIES})
    add_custom_command(
      TARGET swiftASTGen PRE_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR}/${sharedlib} ${SWIFT_SYNTAX_LIBRARIES_DEST_DIR}/${sharedlib}
      COMMENT "Copying ${sharedlib}"
    )
  endforeach()

  # Copy all of the Swift modules from earlyswiftsyntax so they can be found
  # in the same relative place within the build directory as in the final
  # toolchain.
  list(TRANSFORM SWIFT_SYNTAX_MODULES APPEND ".swiftmodule"
       OUTPUT_VARIABLE SWIFT_SYNTAX_MODULE_DIRS)
  foreach(module_dir ${SWIFT_SYNTAX_MODULE_DIRS})
    file(GLOB module_files
         "${SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR}/${module_dir}/*.swiftinterface")
    add_custom_command(
      TARGET swiftASTGen PRE_BUILD
      COMMAND ${CMAKE_COMMAND} -E make_directory ${SWIFT_SYNTAX_LIBRARIES_DEST_DIR}/${module_dir}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${module_files} ${SWIFT_SYNTAX_LIBRARIES_DEST_DIR}/${module_dir}/
      COMMENT "Copying ${module_dir}"
    )
  endforeach()

  target_include_directories(swiftASTGen PUBLIC
    ${SWIFT_SYNTAX_LIBRARIES_DEST_DIR})

  set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS swiftASTGen)
endif()
