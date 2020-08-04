
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
    set_target_properties(${target_name}_exports PROPERTIES
      FOLDER "SourceKit libraries")

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
# FIXME: this is a HACK.  All SourceKit CMake code using this function should be
# rewritten to use 'add_swift_host_library' or 'add_swift_target_library'.
function(add_sourcekit_default_compiler_flags target)
  set(sdk "${SWIFT_HOST_VARIANT_SDK}")
  set(arch "${SWIFT_HOST_VARIANT_ARCH}")
  set(c_compile_flags)
  set(link_flags)

  # Add variant-specific flags.
  set(build_type "${CMAKE_BUILD_TYPE}")
  set(enable_assertions "${LLVM_ENABLE_ASSERTIONS}")
  set(analyze_code_coverage "${SWIFT_ANALYZE_CODE_COVERAGE}")
  _add_host_variant_c_compile_flags(
    SDK "${sdk}"
    ARCH "${arch}"
    BUILD_TYPE "${build_type}"
    ENABLE_ASSERTIONS "${enable_assertions}"
    ANALYZE_CODE_COVERAGE "${analyze_code_coverage}"
    ENABLE_LTO "${SWIFT_TOOLS_ENABLE_LTO}"
    RESULT_VAR_NAME c_compile_flags)
  _add_host_variant_link_flags(
    SDK "${sdk}"
    ARCH "${arch}"
    BUILD_TYPE "${build_type}"
    ENABLE_ASSERTIONS "${enable_assertions}"
    ENABLE_LTO "${SWIFT_TOOLS_ENABLE_LTO}"
    LTO_OBJECT_NAME "${target}-${sdk}-${arch}"
    ANALYZE_CODE_COVERAGE "${analyze_code_coverage}"
    RESULT_VAR_NAME link_flags
    LINK_LIBRARIES_VAR_NAME link_libraries
    LIBRARY_SEARCH_DIRECTORIES_VAR_NAME library_search_directories)

  # Set compilation and link flags.
  if(${SWIFT_HOST_VARIANT_SDK} STREQUAL WINDOWS)
    swift_windows_include_for_arch(${SWIFT_HOST_VARIANT_ARCH}
      ${SWIFT_HOST_VARIANT_ARCH}_INCLUDE)
    target_include_directories(${target} SYSTEM PRIVATE
      ${${SWIFT_HOST_VARIANT_ARCH}_INCLUDE})
  endif()
  target_compile_options(${target} PRIVATE
    ${c_compile_flags} -fblocks)
  target_link_options(${target} PRIVATE
    ${link_flags})
  target_link_directories(${target} PRIVATE
    ${library_search_directories})
  target_link_libraries(${target} PRIVATE
    ${link_libraries})
endfunction()

# Add a new SourceKit library.
#
# Usage:
#   add_sourcekit_library(name     # Name of the library
#     [DEPENDS dep1 ...]           # Targets this library depends on
#     [LLVM_LINK_COMPONENTS comp1 ...]  # LLVM components this library depends on
#     [INSTALL_IN_COMPONENT comp]  # The Swift installation component that this library belongs to.
#     [SHARED]
#     source1 [source2 source3 ...]) # Sources to add into this library
macro(add_sourcekit_library name)
  cmake_parse_arguments(SOURCEKITLIB
      "SHARED"
      "INSTALL_IN_COMPONENT"
      "HEADERS;DEPENDS;LLVM_LINK_COMPONENTS"
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
  if(NOT SWIFT_BUILT_STANDALONE AND NOT CMAKE_C_COMPILER_ID MATCHES Clang)
    add_dependencies(${name} clang)
  endif()
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

  swift_common_llvm_config(${name} ${SOURCEKITLIB_LLVM_LINK_COMPONENTS})

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

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Android")
    if(SOURCEKITLIB_SHARED)
      set_target_properties(${name} PROPERTIES BUILD_WITH_INSTALL_RPATH TRUE)
      set_target_properties(${name} PROPERTIES INSTALL_RPATH "$ORIGIN/../lib/swift/android")
    endif()
  endif()

  if("${SOURCEKITLIB_INSTALL_IN_COMPONENT}" STREQUAL "")
    if(SOURCEKITLIB_SHARED)
      set(SOURCEKITLIB_INSTALL_IN_COMPONENT tools)
    else()
      set(SOURCEKITLIB_INSTALL_IN_COMPONENT dev)
    endif()
  endif()
  add_dependencies(${SOURCEKITLIB_INSTALL_IN_COMPONENT} ${name})
  swift_install_in_component(TARGETS ${name}
    LIBRARY
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      COMPONENT "${SOURCEKITLIB_INSTALL_IN_COMPONENT}"
    ARCHIVE
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
      COMPONENT "${SOURCEKITLIB_INSTALL_IN_COMPONENT}"
    RUNTIME
      DESTINATION "bin"
      COMPONENT "${SOURCEKITLIB_INSTALL_IN_COMPONENT}")
  swift_install_in_component(FILES ${SOURCEKITLIB_HEADERS}
                             DESTINATION "include/SourceKit"
                             COMPONENT "${SOURCEKITLIB_INSTALL_IN_COMPONENT}")
  set_target_properties(${name} PROPERTIES FOLDER "SourceKit libraries")
  add_sourcekit_default_compiler_flags("${name}")

  swift_is_installing_component("${SOURCEKITLIB_INSTALL_IN_COMPONENT}" is_installing)
  if(NOT is_installing)
    set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS ${name})
  else()
    set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS ${name})
  endif()
endmacro()

# Add a new SourceKit executable.
#
# Usage:
#   add_sourcekit_executable(name        # Name of the executable
#     [LLVM_LINK_COMPONENTS comp1 ...] # LLVM components this executable
#                                        # depends on
#     source1 [source2 source3 ...])  # Sources to add into this executable
macro(add_sourcekit_executable name)
  set(SOURCEKIT_EXECUTABLE_options)
  set(SOURCEKIT_EXECUTABLE_single_parameter_options)
  set(SOURCEKIT_EXECUTABLE_multiple_parameter_options LLVM_LINK_COMPONENTS)
  cmake_parse_arguments(SOURCEKITEXE "${SOURCEKIT_EXECUTABLE_options}"
    "${SOURCEKIT_EXECUTABLE_single_parameter_options}"
    "${SOURCEKIT_EXECUTABLE_multiple_parameter_options}" ${ARGN})

  add_executable(${name} ${SOURCEKITEXE_UNPARSED_ARGUMENTS})
  if(NOT SWIFT_BUILT_STANDALONE AND NOT CMAKE_C_COMPILER_ID MATCHES Clang)
    add_dependencies(${name} clang)
  endif()
  llvm_update_compile_flags(${name})
  set_output_directory(${name}
      BINARY_DIR ${SOURCEKIT_RUNTIME_OUTPUT_INTDIR}
      LIBRARY_DIR ${SOURCEKIT_LIBRARY_OUTPUT_INTDIR})

  # Add appropriate dependencies
  if(LLVM_COMMON_DEPENDS)
    add_dependencies(${name} ${LLVM_COMMON_DEPENDS})
  endif()

  swift_common_llvm_config(${name} ${SOURCEKITEXE_LLVM_LINK_COMPONENTS})
  target_link_libraries(${name} PRIVATE ${LLVM_COMMON_LIBS})

  set_target_properties(${name} PROPERTIES FOLDER "SourceKit executables")
  add_sourcekit_default_compiler_flags("${name}")
endmacro()

# Add a new SourceKit framework.
#
# Usage:
#   add_sourcekit_framework(name     # Name of the framework
#     [LLVM_LINK_COMPONENTS comp1 ...]  # LLVM components this framework depends on
#     [MODULEMAP modulemap]          # Module map file for this framework
#     [INSTALL_IN_COMPONENT comp]    # The Swift installation component that this framework belongs to.
#     source1 [source2 source3 ...]) # Sources to add into this framework
macro(add_sourcekit_framework name)
  cmake_parse_arguments(SOURCEKITFW
    "" "MODULEMAP;INSTALL_IN_COMPONENT" "LLVM_LINK_COMPONENTS" ${ARGN})
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

  swift_common_llvm_config(${name} ${SOURCEKITFW_LLVM_LINK_COMPONENTS})

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
    set_output_directory(${name}
        BINARY_DIR ${SOURCEKIT_RUNTIME_OUTPUT_INTDIR}
        LIBRARY_DIR ${SOURCEKIT_LIBRARY_OUTPUT_INTDIR})
    set_target_properties(${name} PROPERTIES
                          BUILD_WITH_INSTALL_RPATH TRUE
                          FOLDER "SourceKit frameworks"
                          FRAMEWORK TRUE
                          INSTALL_NAME_DIR "@rpath"
                          MACOSX_FRAMEWORK_INFO_PLIST "${SOURCEKIT_SOURCE_DIR}/cmake/MacOSXFrameworkInfo.plist.in"
                          MACOSX_FRAMEWORK_IDENTIFIER "com.apple.${name}"
                          MACOSX_FRAMEWORK_SHORT_VERSION_STRING "1.0"
                          MACOSX_FRAMEWORK_BUNDLE_VERSION "${SOURCEKIT_VERSION_STRING}"
                          PUBLIC_HEADER "${headers}")
    add_dependencies(${SOURCEKITFW_INSTALL_IN_COMPONENT} ${name})
    swift_install_in_component(TARGETS ${name}
                               FRAMEWORK
                                 DESTINATION lib${LLVM_LIBDIR_SUFFIX}
                                 COMPONENT ${SOURCEKITFW_INSTALL_IN_COMPONENT}
                               LIBRARY
                                 DESTINATION lib${LLVM_LIBDIR_SUFFIX}
                                 COMPONENT ${SOURCEKITFW_INSTALL_IN_COMPONENT}
                               ARCHIVE
                                 DESTINATION lib${LLVM_LIBDIR_SUFFIX}
                                 COMPONENT ${SOURCEKITFW_INSTALL_IN_COMPONENT}
                               RUNTIME
                                 DESTINATION bin
                                 COMPONENT ${SOURCEKITFW_INSTALL_IN_COMPONENT})
  else()
    set_output_directory(${name}
        BINARY_DIR ${framework_location}
        LIBRARY_DIR ${framework_location})
    set_target_properties(${name} PROPERTIES
                          BUILD_WITH_INSTALL_RPATH TRUE
                          FOLDER "SourceKit frameworks"
                          INSTALL_NAME_DIR "@rpath/${name}.framework"
                          PREFIX ""
                          SUFFIX "")
    swift_install_in_component(DIRECTORY ${framework_location}
                               DESTINATION lib${LLVM_LIBDIR_SUFFIX}
                               COMPONENT ${SOURCEKITFW_INSTALL_IN_COMPONENT}
                               USE_SOURCE_PERMISSIONS)

    foreach(hdr ${headers})
      get_filename_component(hdrname ${hdr} NAME)
      add_custom_command(TARGET ${name} PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy "${hdr}" "${framework_location}/Headers/${hdrname}")
    endforeach()
  endif()

  swift_is_installing_component("${SOURCEKITFW_INSTALL_IN_COMPONENT}" is_installing)
  if(NOT is_installing)
    set_property(GLOBAL APPEND PROPERTY SWIFT_BUILDTREE_EXPORTS ${name})
  else()
    set_property(GLOBAL APPEND PROPERTY SWIFT_EXPORTS ${name})
  endif()

  add_sourcekit_default_compiler_flags("${name}")
endmacro(add_sourcekit_framework)

# Add a new SourceKit XPC service to a framework.
#
# Usage:
#   add_sourcekit_xpc_service(name      # Name of the XPC service
#     [LLVM_LINK_COMPONENTS comp1 ...]   # LLVM components this service depends on
#     source1 [source2 source3 ...])    # Sources to add into this service
macro(add_sourcekit_xpc_service name framework_target)
  cmake_parse_arguments(SOURCEKITXPC "" "" "LLVM_LINK_COMPONENTS" ${ARGN})
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

  swift_common_llvm_config(${name} ${SOURCEKITXPC_LLVM_LINK_COMPONENTS})
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
