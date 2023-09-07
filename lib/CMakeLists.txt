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

# Set up for linking against swift-syntax.
if (SWIFT_SWIFT_PARSER)
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
    SwiftSyntaxMacroExpansion
    SwiftCompilerPluginMessageHandling
  )

  # Compute the list of SwiftSyntax targets that we will link against.
  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND "SwiftSyntax::"
       OUTPUT_VARIABLE SWIFT_SYNTAX_TARGETS)

  set(SWIFT_SYNTAX_LIBRARIES_BUILD_DIR
      "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/lib/swift/host")
  set(SWIFT_HOST_LIBRARIES_DEST_DIR
      "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/swift/host")

  # Determine the SwiftSyntax shared library files that were built as
  # part of earlyswiftsyntax.
  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND "${CMAKE_SHARED_LIBRARY_PREFIX}"
       OUTPUT_VARIABLE SWIFT_SYNTAX_SHARED_LIBRARIES)
  list(TRANSFORM SWIFT_SYNTAX_SHARED_LIBRARIES APPEND
       "${CMAKE_SHARED_LIBRARY_SUFFIX}"
       OUTPUT_VARIABLE SWIFT_SYNTAX_SHARED_LIBRARIES)

  list(TRANSFORM SWIFT_SYNTAX_MODULES PREPEND "${CMAKE_IMPORT_LIBRARY_PREFIX}"
    OUTPUT_VARIABLE SWIFT_SYNTAX_IMPORT_LIBRARIES)
  list(TRANSFORM SWIFT_SYNTAX_IMPORT_LIBRARIES APPEND
    "${CMAKE_IMPORT_LIBRARY_SUFFIX}" OUTPUT_VARIABLE
    SWIFT_SYNTAX_IMPORT_LIBRARIES)

  # Interface library to collect swiftinterfaces and swiftmodules from
  # SwiftSyntax
  add_library(swiftSyntaxLibraries INTERFACE)

  # Copy over all of the shared libraries from earlyswiftsyntax so they can
  # be found via RPATH.
  foreach (sharedlib ${SWIFT_SYNTAX_SHARED_LIBRARIES})
    set(add_origin_rpath)
    if(SWIFT_HOST_VARIANT_SDK MATCHES "LINUX|ANDROID|OPENBSD|FREEBSD")
      # At runtime, use swiftCore in the current toolchain.
      swift_get_set_rpath_script_file(setrpath_command)
      set(add_origin_rpath COMMAND ${CMAKE_COMMAND}
        "-DSWIFT_SET_RPATH_FILE=${SWIFT_HOST_LIBRARIES_DEST_DIR}/${sharedlib}"
        "-DSWIFT_SET_RPATH_NEW_RPATH='$$ORIGIN:$$ORIGIN/../${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}'"
        -P "${setrpath_command}"
      )
    endif()

    if(CMAKE_SYSTEM_NAME MATCHES Windows)
      add_custom_command(OUTPUT ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${sharedlib}
                         DEPENDS "${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/bin/${sharedlib}"
                         COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/bin/${sharedlib} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${sharedlib})
      add_custom_target(copy_swiftSyntaxLibrary_${sharedlib}
                        DEPENDS ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${sharedlib}
                        COMMENT "copying ${sharedlib}")
      swift_install_in_component(PROGRAMS ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${sharedlib}
                                 DESTINATION bin
                                 COMPONENT compiler)
    else()
      add_custom_command(OUTPUT "${SWIFT_HOST_LIBRARIES_DEST_DIR}/${sharedlib}"
                         DEPENDS "${SWIFT_SYNTAX_LIBRARIES_BUILD_DIR}/${sharedlib}"
                         COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SWIFT_SYNTAX_LIBRARIES_BUILD_DIR}/${sharedlib} ${SWIFT_HOST_LIBRARIES_DEST_DIR}/${sharedlib}
                         ${add_origin_rpath})
      add_custom_target(copy_swiftSyntaxLibrary_${sharedlib}
                        DEPENDS "${SWIFT_HOST_LIBRARIES_DEST_DIR}/${sharedlib}"
                        COMMENT "Copying ${sharedlib}")
      swift_install_in_component(PROGRAMS "${SWIFT_HOST_LIBRARIES_DEST_DIR}/${sharedlib}"
                                 DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host"
                                 COMPONENT compiler)
    endif()

    add_dependencies(swiftSyntaxLibraries copy_swiftSyntaxLibrary_${sharedlib})
  endforeach()

  if(CMAKE_SYSTEM_NAME MATCHES Windows)
    foreach(implib ${SWIFT_SYNTAX_IMPORT_LIBRARIES})
      add_custom_command(OUTPUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/lib/swift/windows/${implib}
        DEPENDS ${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/lib/swift/host/${implib}
        COMMAND "${CMAKE_COMMAND}" -E copy_if_different ${SWIFT_PATH_TO_EARLYSWIFTSYNTAX_BUILD_DIR}/lib/swift/host/${implib} ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/lib/swift/windows/${implib})
      add_custom_target(copy_swiftSyntaxLibrary_${implib}
        DEPENDS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/lib/swift/windows/${implib}
        COMMENT "Copying ${implib}")
      swift_install_in_component(PROGRAMS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/lib/swift/windows/${implib}
        DESTINATION lib
        COMPONENT compiler)
      add_dependencies(swiftSyntaxLibraries copy_swiftSyntaxLibrary_${implib})
    endforeach()
  endif()

  # Copy all of the Swift modules from earlyswiftsyntax so they can be found
  # in the same relative place within the build directory as in the final
  # toolchain.
  list(TRANSFORM SWIFT_SYNTAX_MODULES APPEND ".swiftmodule"
       OUTPUT_VARIABLE SWIFT_SYNTAX_MODULE_DIRS)

  foreach(module_dir ${SWIFT_SYNTAX_MODULE_DIRS})
    # Find all of the source module files.
    file(GLOB module_files
      "${SWIFT_SYNTAX_LIBRARIES_BUILD_DIR}/${module_dir}/*.swiftinterface")

    # Determine the destination module files.
    set(dest_module_files)
    foreach(full_module_file ${module_files})
        get_filename_component(module_file ${full_module_file} NAME)
        list(APPEND dest_module_files
            "${SWIFT_HOST_LIBRARIES_DEST_DIR}/${module_dir}/${module_file}")
    endforeach()

    add_custom_command(
      OUTPUT ${dest_module_files}
      DEPENDS ${module_files}
      COMMAND ${CMAKE_COMMAND} -E make_directory ${SWIFT_HOST_LIBRARIES_DEST_DIR}/${module_dir}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${module_files} ${SWIFT_HOST_LIBRARIES_DEST_DIR}/${module_dir}/
    )

    add_custom_target(copy_swiftSyntaxModule_${module_dir}
      DEPENDS ${dest_module_files}
      COMMENT "Copying ${module_dir}"
    )

    swift_install_in_component(
      FILES ${dest_module_files}
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/${module_dir}"
      COMPONENT compiler
    )

    add_dependencies(swiftSyntaxLibraries copy_swiftSyntaxModule_${module_dir})
  endforeach()

  # Add copied SwiftSyntax libraries to global dependencies.
  list(APPEND LLVM_COMMON_DEPENDS swiftSyntaxLibraries)
endif()

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
