# In the absence of fine grained tablegen dependencies we need to ensure that
# Swift's libraries all build after the LLVM & Clang tablegen-generated headers
# are generated. When building out-of-tree (as with build-script) LLVM & Clang's
# CMake configuration files create these targets as dummies so we can safely
# depend on them directly here.
# See https://github.com/apple/swift/issues/48583.
#
# LLVM_COMMON_DEPENDS is a construct from the LLVM build system. It is a special
# purpose variable that provides common dependencies for all libraries, and
# executables generated when it is set. CMake's scoping rules enforce that these
# new dependencies will only be added to targets created under Swift's lib
# directory.
list(APPEND LLVM_COMMON_DEPENDS intrinsics_gen clang-tablegen-targets)

# Add generated Swift Syntax headers to global dependencies.
list(APPEND LLVM_COMMON_DEPENDS swift-ast-generated-headers)

include(SwiftImplicitImport)

# Set up for linking against swift-syntax.
if (SWIFT_SWIFT_PARSER)
  # Ensure that we do not link the _StringProcessing module. But we can
  # only pass this flag for new-enough compilers that support it.
  swift_supports_implicit_module("string-processing"
    SWIFT_SUPPORTS_DISABLE_IMPLICIT_STRING_PROCESSING_MODULE_IMPORT)

  # Same for _Backtracing
  swift_supports_implicit_module("backtracing"
    SWIFT_SUPPORTS_DISABLE_IMPLICIT_BACKTRACING_MODULE_IMPORT)

  # Set up linking against the swift-syntax modules.
  # Link against the swift-syntax modules.
  set(SWIFT_SYNTAX_MODULES
    SwiftBasicFormat
    SwiftParser
    SwiftParserDiagnostics
    SwiftDiagnostics
    SwiftSyntax
    SwiftOperators
    SwiftSyntaxBuilder
    SwiftSyntaxMacros
  )

  # Compute the list of SwiftSyntax targets that we will link against.
  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND "SwiftSyntax::"
       OUTPUT_VARIABLE SWIFT_SYNTAX_TARGETS)

  set(SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR
      "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/lib/swift/host")
  set(SWIFT_HOST_LIBRARIES_DEST_DIR
      "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/swift/host")

  # Determine the SwiftSyntax shared library files that were built as
  # part of earlyswiftsyntax.
  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND ${CMAKE_SHARED_LIBRARY_PREFIX}
       OUTPUT_VARIABLE SWIFT_SYNTAX_SHARED_LIBRARIES)
  list(TRANSFORM SWIFT_SYNTAX_SHARED_LIBRARIES APPEND
       ${CMAKE_SHARED_LIBRARY_SUFFIX}
       OUTPUT_VARIABLE SWIFT_SYNTAX_SHARED_LIBRARIES)

  # The complete set of files that get installed.
  set(SWIFT_SYNTAX_DEST_FILES)

  # Copy over all of the shared libraries from earlyswiftsyntax so they can
  # be found via RPATH.
  foreach (sharedlib ${SWIFT_SYNTAX_SHARED_LIBRARIES})
    add_custom_command(
      OUTPUT ${SWIFT_HOST_LIBRARIES_DEST_DIR}/${sharedlib}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR}/${sharedlib} ${SWIFT_HOST_LIBRARIES_DEST_DIR}/${sharedlib}
      COMMENT "Copying ${sharedlib}"
    )

    list(APPEND SWIFT_SYNTAX_DEST_FILES ${SWIFT_HOST_LIBRARIES_DEST_DIR}/${sharedlib})
  endforeach()

  # Copy all of the Swift modules from earlyswiftsyntax so they can be found
  # in the same relative place within the build directory as in the final
  # toolchain.
  list(TRANSFORM SWIFT_SYNTAX_MODULES APPEND ".swiftmodule"
       OUTPUT_VARIABLE SWIFT_SYNTAX_MODULE_DIRS)
  foreach(module_dir ${SWIFT_SYNTAX_MODULE_DIRS})
    # Find all of the source module files.
    file(GLOB module_files
         "${SWIFT_SYNTAX_LIBRARIES_SOURCE_DIR}/${module_dir}/*.swiftinterface")

    # Determine the destination module files.
    set(dest_module_files)
    foreach(full_module_file ${module_files})
        get_filename_component(module_file ${full_module_file} NAME)
        list(APPEND dest_module_files
            "${SWIFT_HOST_LIBRARIES_DEST_DIR}/${module_dir}/${module_file}")
    endforeach()

    add_custom_command(
      OUTPUT ${dest_module_files}
      COMMAND ${CMAKE_COMMAND} -E make_directory ${SWIFT_HOST_LIBRARIES_DEST_DIR}/${module_dir}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${module_files} ${SWIFT_HOST_LIBRARIES_DEST_DIR}/${module_dir}/
      COMMENT "Copying ${module_dir}"
    )

    list(APPEND SWIFT_SYNTAX_DEST_FILES ${dest_module_files})
  endforeach()

  # Add a custom target to copy over the SwiftSyntax build products into
  # their final places.
  add_custom_target(swiftSyntaxLibraries DEPENDS ${SWIFT_SYNTAX_DEST_FILES})
endif()

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

# Add a new "pure" Swift host library.
#
# "Pure" Swift host libraries can only contain Swift code, and will be built
# with the host compiler. They are always expected to be part of the built
# compiler, without bootstrapping.
#
# All of these libraries depend on the swift-syntax stack, since they are
# meant to be part of the compiler.
#
# Usage:
#   add_pure_swift_host_library(name
#     [SHARED]
#     [STATIC]
#     [LLVM_LINK_COMPONENTS comp1 ...]
#     source1 [source2 source3 ...])
#
# name
#   Name of the library (e.g., swiftParse).
#
# SHARED
#   Build a shared library.
#
# STATIC
#   Build a static library.
#
# LLVM_LINK_COMPONENTS
#   LLVM components this library depends on.
#
# source1 ...
#   Sources to add into this library.
function(add_pure_swift_host_library name)
  if (NOT SWIFT_SWIFT_PARSER)
    message(STATUS "Not building ${name} because swift-syntax is not available")
    return()
  endif()

  # Option handling
  set(options
        SHARED
        STATIC)
  set(single_parameter_options)
  set(multiple_parameter_options
        DEPENDENCIES)

  cmake_parse_arguments(APSHL
                        "${options}"
                        "${single_parameter_options}"
                        "${multiple_parameter_options}"
                        ${ARGN})
  set(APSHL_SOURCES ${APSHL_UNPARSED_ARGUMENTS})

  translate_flags(APSHL "${options}")

  # Determine what kind of library we're building.
  if(APSHL_SHARED)
    set(libkind SHARED)
  elseif(APSHL_STATIC)
    set(libkind STATIC)
  endif()

  # Create the library.
  add_library(${name} ${libkind} ${APSHL_SOURCES})

  # Avoid introducing an implicit dependency on the string-processing library.
  if(SWIFT_SUPPORTS_DISABLE_IMPLICIT_STRING_PROCESSING_MODULE_IMPORT)
    target_compile_options(${name} PRIVATE
      $<$<COMPILE_LANGUAGE:Swift>:-Xfrontend>
      $<$<COMPILE_LANGUAGE:Swift>:-disable-implicit-string-processing-module-import>)
  endif()

  # Same for backtracing
  if (SWIFT_SUPPORTS_DISABLE_IMPLICIT_BACKTRACING_MODULE_IMPORT)
    target_compile_options(${name} PRIVATE
      $<$<COMPILE_LANGUAGE:Swift>:-Xfrontend>
      $<$<COMPILE_LANGUAGE:Swift>:-disable-implicit-backtracing-module-import>)
  endif()

  # The compat56 library is not available in current toolchains. The stage-0
  # compiler will build fine since the builder compiler is not aware of the 56
  # compat library, but the stage-1 and subsequent stage compilers will fail as
  # the stage-0 compiler is aware and will attempt to include the appropriate
  # compatibility library. We should turn this back on once we are building the
  # compiler correctly.
  # Note: This is safe at the moment because the 5.6 compat library only
  #       contains concurrency runtime fixes, and the compiler frontend does not
  #       use concurrency at the moment.
  target_compile_options(${name} PRIVATE
    $<$<COMPILE_LANGUAGE:Swift>:-runtime-compatibility-version>
    $<$<COMPILE_LANGUAGE:Swift>:none>)

  # Set the appropriate target triple.
  # FIXME: This should be set by CMake.
  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set(DEPLOYMENT_VERSION "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")
  endif()

  if(SWIFT_HOST_VARIANT_SDK STREQUAL ANDROID)
    set(DEPLOYMENT_VERSION ${SWIFT_ANDROID_API_LEVEL})
  endif()

  get_target_triple(target target_variant "${SWIFT_HOST_VARIANT_SDK}" "${SWIFT_HOST_VARIANT_ARCH}"
    MACCATALYST_BUILD_FLAVOR ""
    DEPLOYMENT_VERSION "${DEPLOYMENT_VERSION}")

  target_compile_options(${name} PRIVATE $<$<COMPILE_LANGUAGE:Swift>:-target;${target}>)

  # Workaround to touch the library and its objects so that we don't
  # continually rebuild (again, see corresponding change in swift-syntax).
  add_custom_command(
      TARGET ${name}
      POST_BUILD
      COMMAND "${CMAKE_COMMAND}" -E touch_nocreate $<TARGET_FILE:${name}> $<TARGET_OBJECTS:${name}>
      COMMAND_EXPAND_LISTS
      COMMENT "Update mtime of library outputs workaround")

  # Link against swift-syntax libraries.
  # TODO: Change to target_link_libraries when cmake is fixed
  force_target_link_libraries(${name} PUBLIC
    ${SWIFT_SYNTAX_TARGETS}
  )

  # Add dependency on the copied swift-syntax libraries.
  add_dependencies(${name} swiftSyntaxLibraries)

  # Link against dependencies.
  target_link_libraries(${name} PUBLIC
    ${APSHL_DEPENDENCIES}
  )

  # Make sure we can use the host libraries.
  target_include_directories(${name} PUBLIC
    ${SWIFT_HOST_LIBRARIES_DEST_DIR})

  # Export this target.
  set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS ${name})
endfunction()

add_subdirectory(APIDigester)
add_subdirectory(AST)
add_subdirectory(ASTGen)
add_subdirectory(ASTSectionImporter)
add_subdirectory(Basic)
add_subdirectory(ConstExtract)
add_subdirectory(ClangImporter)
add_subdirectory(Demangling)
add_subdirectory(DependencyScan)
add_subdirectory(Driver)
add_subdirectory(DriverTool)
add_subdirectory(Frontend)
add_subdirectory(FrontendTool)
add_subdirectory(Index)
add_subdirectory(IDE)
add_subdirectory(IDETool)
add_subdirectory(Immediate)
add_subdirectory(IRGen)
add_subdirectory(LLVMPasses)
add_subdirectory(Localization)
add_subdirectory(Macros)
add_subdirectory(Markup)
add_subdirectory(Migrator)
add_subdirectory(Option)
add_subdirectory(Parse)
add_subdirectory(PrintAsClang)
add_subdirectory(Refactoring)
add_subdirectory(RemoteAST)
add_subdirectory(Sema)
add_subdirectory(Serialization)
add_subdirectory(SwiftDemangle)
add_subdirectory(SwiftRemoteInspection)
add_subdirectory(SwiftRemoteMirror)
add_subdirectory(SIL)
add_subdirectory(SILGen)
add_subdirectory(SILOptimizer)
add_subdirectory(StaticMirror)
add_subdirectory(SymbolGraphGen)
add_subdirectory(Threading)
