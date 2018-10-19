
function(add_sourcekit_symbol_exports target_name export_file)
  # Makefile.rules contains special cases for different platforms.
  # We restrict ourselves to Darwin for the time being.
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    add_custom_command(OUTPUT symbol.exports
      COMMAND sed -e "s/^/_/" < ${export_file} > symbol.exports
      DEPENDS ${export_file}
      VERBATIM
      COMMENT "Creating export file for ${target_name}")
    add_custom_target(${target_name}_exports DEPENDS symbol.exports)
    set_property(DIRECTORY APPEND
      PROPERTY ADDITIONAL_MAKE_CLEAN_FILES symbol.exports)

    get_property(srcs TARGET ${target_name} PROPERTY SOURCES)
    foreach(src ${srcs})
      get_filename_component(extension ${src} EXT)
      if(extension STREQUAL ".cpp")
        set(first_source_file ${src})
        break()
      endif()
    endforeach()

    # Force re-linking when the exports file changes. Actually, it
    # forces recompilation of the source file. The LINK_DEPENDS target
    # property only works for makefile-based generators.
    set_property(SOURCE ${first_source_file} APPEND PROPERTY
      OBJECT_DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/symbol.exports)

    set_property(TARGET ${target_name} APPEND_STRING PROPERTY
                 LINK_FLAGS " -Wl,-exported_symbols_list,${CMAKE_CURRENT_BINARY_DIR}/symbol.exports")

    add_dependencies(${target_name} ${target_name}_exports)
  endif()
endfunction()

# Add default compiler and linker flags to 'target'.
#
# FIXME: this is a HACK.  All SourceKit CMake code using this function
# should be rewritten to use 'add_swift_library'.
function(add_sourcekit_default_compiler_flags target)
  set(sdk "${SWIFT_HOST_VARIANT_SDK}")
  set(arch "${SWIFT_HOST_VARIANT_ARCH}")
  set(c_compile_flags)
  set(link_flags)

  # Add variant-specific flags.
  set(build_type "${CMAKE_BUILD_TYPE}")
  set(enable_assertions "${LLVM_ENABLE_ASSERTIONS}")
  set(analyze_code_coverage "${SWIFT_ANALYZE_CODE_COVERAGE}")
  _add_variant_c_compile_flags(
    SDK "${sdk}"
    ARCH "${arch}"
    BUILD_TYPE "${build_type}"
    ENABLE_ASSERTIONS "${enable_assertions}"
    ANALYZE_CODE_COVERAGE "${analyze_code_coverage}"
    ENABLE_LTO "${SWIFT_TOOLS_ENABLE_LTO}"
    RESULT_VAR_NAME c_compile_flags)
  _add_variant_link_flags(
    SDK "${sdk}"
    ARCH "${arch}"
    BUILD_TYPE "${build_type}"
    ENABLE_ASSERTIONS "${enable_assertions}"
    ENABLE_LTO "${SWIFT_TOOLS_ENABLE_LTO}"
    LTO_OBJECT_NAME "${target}-${sdk}-${arch}"
    ANALYZE_CODE_COVERAGE "${analyze_code_coverage}"
    RESULT_VAR_NAME link_flags)

  # TODO(compnerd) this should really use target_compile_options but the use
  # of keyword and non-keyword flags prevents this
  if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    list(APPEND c_compile_flags -fblocks)
  elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    if(SWIFT_COMPILER_IS_MSVC_LIKE)
      list(APPEND c_compile_flags -Xclang;-fblocks)
    else()
      list(APPEND c_compile_flags -fblocks)
    endif()
  endif()

  # Convert variables to space-separated strings.
  _list_escape_for_shell("${c_compile_flags}" c_compile_flags)
  _list_escape_for_shell("${link_flags}" link_flags)

  # Set compilation and link flags.
  set_property(TARGET "${target}" APPEND_STRING PROPERTY
      COMPILE_FLAGS " ${c_compile_flags}")
  set_property(TARGET "${target}" APPEND_STRING PROPERTY
      LINK_FLAGS " ${link_flags}")
endfunction()

# Add a new SourceKit library.
#
# Usage:
#   add_sourcekit_library(name     # Name of the library
#     [LINK_LIBS dep1 ...]         # Libraries this library will be linked with
#     [DEPENDS dep1 ...]           # Targets this library depends on
#     [LLVM_COMPONENT_DEPENDS comp1 ...]  # LLVM components this library depends on
#     [INSTALL_IN_COMPONENT comp]  # The Swift installation component that this library belongs to.
#     [SHARED]
#     source1 [source2 source3 ...]) # Sources to add into this library
macro(add_sourcekit_library name)
  cmake_parse_arguments(SOURCEKITLIB
      "SHARED"
      "INSTALL_IN_COMPONENT"
      "HEADERS;LINK_LIBS;DEPENDS;LLVM_COMPONENT_DEPENDS"
      ${ARGN})
  set(srcs ${SOURCEKITLIB_UNPARSED_ARGUMENTS})

  llvm_process_sources(srcs ${srcs})
  if(MSVC_IDE OR XCODE)
    # Add public headers
    file(RELATIVE_PATH lib_path
      ${SOURCEKIT_SOURCE_DIR}/lib/
      ${CMAKE_CURRENT_SOURCE_DIR}
    )
    if(NOT lib_path MATCHES "^[.][.]")
      file(GLOB_RECURSE headers
        ${SOURCEKIT_SOURCE_DIR}/include/SourceKit/${lib_path}/*.h
        ${SOURCEKIT_SOURCE_DIR}/include/SourceKit/${lib_path}/*.def
      )
      set_source_files_properties(${headers} PROPERTIES HEADER_FILE_ONLY ON)

      file(GLOB_RECURSE tds
        ${SOURCEKIT_SOURCE_DIR}/include/SourceKit/${lib_path}/*.td
      )
      source_group("TableGen descriptions" FILES ${tds})
      set_source_files_properties(${tds} PROPERTIES HEADER_FILE_ONLY ON)

      set(srcs ${srcs} ${headers} ${tds})
    endif()
  endif()
  if (MODULE)
    set(libkind MODULE)
  elseif(SOURCEKITLIB_SHARED)
    set(libkind SHARED)
  else()
    set(libkind)
  endif()
  add_library(${name} ${libkind} ${srcs})
  llvm_update_compile_flags(${name})

  set_output_directory(${name}
      BINARY_DIR ${SOURCEKIT_RUNTIME_OUTPUT_INTDIR}
      LIBRARY_DIR ${SOURCEKIT_LIBRARY_OUTPUT_INTDIR})

  if(LLVM_COMMON_DEPENDS)
    add_dependencies(${name} ${LLVM_COMMON_DEPENDS})
  endif(LLVM_COMMON_DEPENDS)

  if(SOURCEKITLIB_DEPENDS)
    add_dependencies(${name} ${SOURCEKITLIB_DEPENDS})
  endif(SOURCEKITLIB_DEPENDS)

  set(prefixed_link_libraries)
  foreach(dep ${SOURCEKITLIB_LINK_LIBS})
    if("${dep}" MATCHES "^clang")
      set(dep "${LLVM_LIBRARY_OUTPUT_INTDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}${dep}${CMAKE_STATIC_LIBRARY_SUFFIX}")
    endif()
    list(APPEND prefixed_link_libraries "${dep}")
  endforeach()
  set(SOURCEKITLIB_LINK_LIBS "${prefixed_link_libraries}")

  if("${libkind}" STREQUAL "SHARED")
    target_link_libraries("${name}" PRIVATE ${SOURCEKITLIB_LINK_LIBS})
  else()
    target_link_libraries("${name}" INTERFACE ${SOURCEKITLIB_LINK_LIBS})
  endif()

  swift_common_llvm_config(${name} ${SOURCEKITLIB_LLVM_COMPONENT_DEPENDS})

  if(SOURCEKITLIB_SHARED AND EXPORTED_SYMBOL_FILE)
    add_sourcekit_symbol_exports(${name} ${EXPORTED_SYMBOL_FILE})
  endif()

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    if(SOURCEKITLIB_SHARED)
      set_target_properties(${name} PROPERTIES BUILD_WITH_INSTALL_RPATH TRUE)
      set_target_properties(${name} PROPERTIES INSTALL_NAME_DIR "@rpath")
    endif()
  endif()

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    if(SOURCEKITLIB_SHARED)
      set_target_properties(${name} PROPERTIES BUILD_WITH_INSTALL_RPATH TRUE)
      set_target_properties(${name} PROPERTIES INSTALL_RPATH "$ORIGIN/../lib/swift/linux:/usr/lib/swift/linux")
    endif()
  endif()

  if("${SOURCEKITLIB_INSTALL_IN_COMPONENT}" STREQUAL "")
    if(SOURCEKITLIB_SHARED)
      set(SOURCEKITLIB_INSTALL_IN_COMPONENT tools)
    else()
      set(SOURCEKITLIB_INSTALL_IN_COMPONENT dev)
    endif()
  endif()
  swift_install_in_component("${SOURCEKITLIB_INSTALL_IN_COMPONENT}"
      TARGETS ${name}
      LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      RUNTIME DESTINATION "bin")
  swift_install_in_component("${SOURCEKITLIB_INSTALL_IN_COMPONENT}"
    FILES ${SOURCEKITLIB_HEADERS}
    DESTINATION "include/SourceKit")
  set_target_properties(${name} PROPERTIES FOLDER "SourceKit libraries")
  add_sourcekit_default_compiler_flags("${name}")
endmacro()

# Add a new SourceKit executable.
#
# Usage:
#   add_sourcekit_executable(name        # Name of the executable
#     [LINK_LIBS dep1 ...]               # Libraries this executable depends on
#     [LLVM_COMPONENT_DEPENDS comp1 ...] # LLVM components this executable
#                                        # depends on
#     [EXCLUDE_FROM_ALL]              # Whether to exclude this executable from
#                                     # the ALL_BUILD target
#     source1 [source2 source3 ...])  # Sources to add into this executable
macro(add_sourcekit_executable name)
  cmake_parse_arguments(SOURCEKITEXE
    "EXCLUDE_FROM_ALL"
    ""
    "LINK_LIBS;LLVM_COMPONENT_DEPENDS"
    ${ARGN})

  if (${SOURCEKITEXE_EXCLUDE_FROM_ALL})
    add_executable(${name} EXCLUDE_FROM_ALL ${SOURCEKITEXE_UNPARSED_ARGUMENTS})
  else()
    add_executable(${name} ${SOURCEKITEXE_UNPARSED_ARGUMENTS})
  endif()
  llvm_update_compile_flags(${name})
  set_output_directory(${name}
      BINARY_DIR ${SOURCEKIT_RUNTIME_OUTPUT_INTDIR}
      LIBRARY_DIR ${SOURCEKIT_LIBRARY_OUTPUT_INTDIR})

  # Add appropriate dependencies
  if(LLVM_COMMON_DEPENDS)
    add_dependencies(${name} ${LLVM_COMMON_DEPENDS})
  endif()

  target_link_libraries(${name} PRIVATE ${SOURCEKITEXE_LINK_LIBS})
  swift_common_llvm_config(${name} ${SOURCEKITEXE_LLVM_COMPONENT_DEPENDS})
  target_link_libraries(${name} PRIVATE ${LLVM_COMMON_LIBS})

  set_target_properties(${name} PROPERTIES FOLDER "SourceKit executables")
  if (NOT SWIFT_ASAN_BUILD)
    if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
      set_target_properties(${name}
        PROPERTIES
        LINK_FLAGS "-Wl,-exported_symbol,_main")
    endif()
    if(SWIFT_ANALYZE_CODE_COVERAGE)
      set_property(TARGET "${name}" APPEND_STRING PROPERTY
        LINK_FLAGS " -fprofile-instr-generate -fcoverage-mapping")
    endif()
  endif()
  add_sourcekit_default_compiler_flags("${name}")
endmacro()

# Add a new SourceKit framework.
#
# Usage:
#   add_sourcekit_framework(name     # Name of the framework
#     [LINK_LIBS dep1 ...]           # Libraries this framework will link with
#     [LLVM_COMPONENT_DEPENDS comp1 ...]  # LLVM components this framework depends on
#     [MODULEMAP modulemap]          # Module map file for this framework
#     [INSTALL_IN_COMPONENT comp]    # The Swift installation component that this framework belongs to.
#     source1 [source2 source3 ...]) # Sources to add into this framework
macro(add_sourcekit_framework name)
  cmake_parse_arguments(SOURCEKITFW
    "" "MODULEMAP;INSTALL_IN_COMPONENT" "LINK_LIBS;LLVM_COMPONENT_DEPENDS" ${ARGN})
  set(srcs ${SOURCEKITFW_UNPARSED_ARGUMENTS})

  set(lib_dir ${SOURCEKIT_LIBRARY_OUTPUT_INTDIR})
  set(framework_location "${lib_dir}/${name}.framework")

  if (NOT SOURCEKIT_DEPLOYMENT_OS MATCHES "^macosx")
    set(FLAT_FRAMEWORK_NAME "${name}")
    set(FLAT_FRAMEWORK_IDENTIFIER "com.apple.${name}")
    set(FLAT_FRAMEWORK_SHORT_VERSION_STRING "1.0")
    set(FLAT_FRAMEWORK_BUNDLE_VERSION "${SOURCEKIT_VERSION_STRING}")
    set(FLAT_FRAMEWORK_DEPLOYMENT_TARGET "${SOURCEKIT_DEPLOYMENT_TARGET}")
    configure_file(
      "${SOURCEKIT_SOURCE_DIR}/cmake/FlatFrameworkInfo.plist.in"
      "${CMAKE_CURRENT_BINARY_DIR}/${name}.Info.plist")
    add_custom_command(OUTPUT "${framework_location}/Info.plist"
      DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${name}.Info.plist"
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${CMAKE_CURRENT_BINARY_DIR}/${name}.Info.plist" "${framework_location}/Info.plist")
    list(APPEND srcs "${framework_location}/Info.plist")
  endif()

  llvm_process_sources(srcs ${srcs})
  add_library(${name} SHARED ${srcs})
  llvm_update_compile_flags(${name})

  set(headers)
  foreach(src ${srcs})
    get_filename_component(extension ${src} EXT)
    if(extension STREQUAL ".h")
      list(APPEND headers ${src})
    endif()
  endforeach()

  if(MSVC_IDE OR XCODE)
    set_source_files_properties(${headers} PROPERTIES HEADER_FILE_ONLY ON)
  endif(MSVC_IDE OR XCODE)

  if(LLVM_COMMON_DEPENDS)
    add_dependencies(${name} ${LLVM_COMMON_DEPENDS})
  endif(LLVM_COMMON_DEPENDS)

  target_link_libraries(${name} PRIVATE ${SOURCEKITFW_LINK_LIBS})
  swift_common_llvm_config(${name} ${SOURCEKITFW_LLVM_COMPONENT_DEPENDS})

  if (EXPORTED_SYMBOL_FILE)
    add_sourcekit_symbol_exports(${name} ${EXPORTED_SYMBOL_FILE})
  endif()

  if(SOURCEKITFW_MODULEMAP)
    set(modulemap "${CMAKE_CURRENT_SOURCE_DIR}/${SOURCEKITFW_MODULEMAP}")
    if (SOURCEKIT_DEPLOYMENT_OS MATCHES "^macosx")
      set(modules_dir "${framework_location}/Versions/A/Modules")
      add_custom_command(TARGET ${name} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy "${modulemap}" "${modules_dir}/module.modulemap"
        COMMAND ${CMAKE_COMMAND} -E create_symlink "Versions/Current/Modules" "${framework_location}/Modules")
    else()
      set(modules_dir "${framework_location}/Modules")
      add_custom_command(TARGET ${name} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy "${modulemap}" "${modules_dir}/module.modulemap")
    endif()
  endif()


  if (SOURCEKIT_DEPLOYMENT_OS MATCHES "^macosx")
    swift_install_in_component(${SOURCEKITFW_INSTALL_IN_COMPONENT}
        TARGETS ${name}
        LIBRARY DESTINATION lib${LLVM_LIBDIR_SUFFIX}
        ARCHIVE DESTINATION lib${LLVM_LIBDIR_SUFFIX}
        RUNTIME DESTINATION bin)
    set_target_properties(${name} PROPERTIES FOLDER "SourceKit frameworks")
    set_output_directory(${name}
        BINARY_DIR ${SOURCEKIT_RUNTIME_OUTPUT_INTDIR}
        LIBRARY_DIR ${SOURCEKIT_LIBRARY_OUTPUT_INTDIR})
    set_target_properties(${name} PROPERTIES FRAMEWORK TRUE)
    set_target_properties(${name} PROPERTIES PUBLIC_HEADER "${headers}")
    set_target_properties(${name} PROPERTIES MACOSX_FRAMEWORK_INFO_PLIST "${SOURCEKIT_SOURCE_DIR}/cmake/MacOSXFrameworkInfo.plist.in")
    set_target_properties(${name} PROPERTIES MACOSX_FRAMEWORK_IDENTIFIER "com.apple.${name}")
    set_target_properties(${name} PROPERTIES MACOSX_FRAMEWORK_SHORT_VERSION_STRING "1.0")
    set_target_properties(${name} PROPERTIES MACOSX_FRAMEWORK_BUNDLE_VERSION "${SOURCEKIT_VERSION_STRING}")
    set_target_properties(${name} PROPERTIES BUILD_WITH_INSTALL_RPATH TRUE)
    set_target_properties(${name} PROPERTIES INSTALL_NAME_DIR "@rpath")
  else()
    swift_install_in_component(${SOURCEKITFW_INSTALL_IN_COMPONENT}
        DIRECTORY ${framework_location}
        DESTINATION lib${LLVM_LIBDIR_SUFFIX}
        USE_SOURCE_PERMISSIONS)
    set_target_properties(${name} PROPERTIES FOLDER "SourceKit frameworks")
    set_output_directory(${name}
        BINARY_DIR ${framework_location}
        LIBRARY_DIR ${framework_location})
    set_target_properties(${name} PROPERTIES PREFIX "")
    set_target_properties(${name} PROPERTIES SUFFIX "")
    set_target_properties(${name} PROPERTIES BUILD_WITH_INSTALL_RPATH TRUE)
    set_target_properties(${name} PROPERTIES INSTALL_NAME_DIR "@rpath/${name}.framework")

    foreach(hdr ${headers})
      get_filename_component(hdrname ${hdr} NAME)
      add_custom_command(TARGET ${name} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy "${hdr}" "${framework_location}/Headers/${hdrname}")
    endforeach()
  endif()
  add_sourcekit_default_compiler_flags("${name}")
endmacro(add_sourcekit_framework)

# Add a new SourceKit XPC service to a framework.
#
# Usage:
#   add_sourcekit_xpc_service(name      # Name of the XPC service
#     [LINK_LIBS dep1 ...]              # Libraries this service will link with
#     [LLVM_COMPONENT_DEPENDS comp1 ...]   # LLVM components this service depends on
#     source1 [source2 source3 ...])    # Sources to add into this service
macro(add_sourcekit_xpc_service name framework_target)
  cmake_parse_arguments(SOURCEKITXPC "" "" "LINK_LIBS;LLVM_COMPONENT_DEPENDS" ${ARGN})
  set(srcs ${SOURCEKITXPC_UNPARSED_ARGUMENTS})

  set(lib_dir ${SOURCEKIT_LIBRARY_OUTPUT_INTDIR})
  set(framework_location "${lib_dir}/${framework_target}.framework")
  if (SOURCEKIT_DEPLOYMENT_OS MATCHES "^macosx")
    set(xpc_bundle_dir "${framework_location}/Versions/A/XPCServices/${name}.xpc")
    set(xpc_contents_dir "${xpc_bundle_dir}/Contents")
    set(xpc_bin_dir "${xpc_contents_dir}/MacOS")
  else()
    set(xpc_bundle_dir "${framework_location}/XPCServices/${name}.xpc")
    set(xpc_contents_dir "${xpc_bundle_dir}")
    set(xpc_bin_dir "${xpc_contents_dir}")
  endif()

  set(XPCSERVICE_NAME ${name})
  set(XPCSERVICE_IDENTIFIER "com.apple.${name}.${SOURCEKIT_VERSION_STRING}_${SOURCEKIT_PLATFORM_NAME}")
  set(XPCSERVICE_BUNDLE_VERSION "${SOURCEKIT_VERSION_STRING}")
  set(XPCSERVICE_SHORT_VERSION_STRING "1.0")
  configure_file(
    "${SOURCEKIT_SOURCE_DIR}/cmake/XPCServiceInfo.plist.in"
    "${CMAKE_CURRENT_BINARY_DIR}/${name}.Info.plist")
  add_custom_command(OUTPUT "${xpc_contents_dir}/Info.plist"
    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${name}.Info.plist"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${CMAKE_CURRENT_BINARY_DIR}/${name}.Info.plist" "${xpc_contents_dir}/Info.plist")
  list(APPEND srcs "${xpc_contents_dir}/Info.plist")

  add_llvm_executable(${name} ${srcs})
  set_target_properties(${name} PROPERTIES FOLDER "XPC Services")
  set_target_properties(${name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${xpc_bin_dir}")

  set_output_directory(${name}
    BINARY_DIR "${xpc_bin_dir}"
    LIBRARY_DIR "${xpc_bin_dir}")

  # Add appropriate dependencies
  if(LLVM_COMMON_DEPENDS)
    add_dependencies(${name} ${LLVM_COMMON_DEPENDS})
  endif(LLVM_COMMON_DEPENDS)

  target_link_libraries(${name} PRIVATE ${SOURCEKITXPC_LINK_LIBS})
  swift_common_llvm_config(${name} ${SOURCEKITXPC_LLVM_COMPONENT_DEPENDS})
  target_link_libraries(${name} PRIVATE ${LLVM_COMMON_LIBS})

  add_dependencies(${framework_target} ${name})

  # This is necessary to avoid having an rpath with an absolute build directory.
  # Without this, such an rpath is added during build time and preserved at install time.
  set_target_properties(${name} PROPERTIES
                        BUILD_WITH_INSTALL_RPATH On
                        INSTALL_RPATH "@loader_path/../lib"
                        INSTALL_NAME_DIR "@rpath")

  if (SOURCEKIT_DEPLOYMENT_OS MATCHES "^macosx")
    add_custom_command(TARGET ${name} POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E create_symlink "Versions/Current/XPCServices" XPCServices
      WORKING_DIRECTORY ${framework_location})
  endif()

  # ASan does not play well with exported_symbol option. This should be fixed soon.
  if(NOT SWIFT_ASAN_BUILD)
    if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
      set_target_properties(${name}
        PROPERTIES
        LINK_FLAGS "-Wl,-exported_symbol,_main")
    endif()
  endif()
  add_sourcekit_default_compiler_flags("${name}")
endmacro()
