# SWIFTLIB_DIR is the directory in the build tree where Swift resource files
# should be placed.  Note that $CMAKE_CFG_INTDIR expands to "." for
# single-configuration builds.
set(SWIFTLIB_DIR
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift")
set(SWIFTSTATICLIB_DIR
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift_static")

function(_list_add_string_suffix input_list suffix result_var_name)
  set(result)
  foreach(element ${input_list})
    list(APPEND result "${element}${suffix}")
  endforeach()
  set("${result_var_name}" "${result}" PARENT_SCOPE)
endfunction()

function(add_dependencies_multiple_targets)
  cmake_parse_arguments(
      ADMT # prefix
      "" # options
      "" # single-value args
      "TARGETS;DEPENDS" # multi-value args
      ${ARGN})
  if(NOT "${ADMT_UNPARSED_ARGUMENTS}" STREQUAL "")
    message(FATAL_ERROR "unrecognized arguments: ${ADMT_UNPARSED_ARGUMENTS}")
  endif()

  if(NOT "${ADMT_DEPENDS}" STREQUAL "")
    foreach(target ${ADMT_TARGETS})
      add_dependencies("${target}" ${ADMT_DEPENDS})
    endforeach()
  endif()
endfunction()

# Compute the library subdirectory to use for the given sdk and
# architecture, placing the result in 'result_var_name'.
function(compute_library_subdir result_var_name sdk arch)
  set("${result_var_name}" "${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${arch}" PARENT_SCOPE)
endfunction()


# Use 16K segment alignment on 32-bit arm.
# The linker does this by default for iOS 8+ deployment,
# but we deploy to iOS 7.
function(append_darwin_segalign_link_flags
    arch result_var_name)
  set(result ${${result_var_name}})

  string(FIND "${arch}" "arm" armprefix)
  if ("${armprefix}" STREQUAL "0" AND NOT "${arch}" STREQUAL "arm64")
    # arch starts with "arm" and is not "arm64"
    list(APPEND result "-Xlinker" "-segalign" "-Xlinker" "0x4000")
  endif()

  set("${result_var_name}" "${result}" PARENT_SCOPE)
endfunction()


function(_add_variant_c_compile_link_flags
    sdk arch build_type enable_assertions result_var_name)
  set(result ${${result_var_name}})

  list(APPEND result
      "-isysroot" "${SWIFT_SDK_${sdk}_PATH}"
      "-target" "${SWIFT_SDK_${sdk}_ARCH_${arch}_TRIPLE}")

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    list(APPEND result
        "-arch" "${arch}"
        "-F" "${SWIFT_SDK_${sdk}_PATH}/../../../Developer/Library/Frameworks"
        "-m${SWIFT_SDK_${sdk}_VERSION_MIN_NAME}-version-min=${SWIFT_SDK_${sdk}_DEPLOYMENT_VERSION}")

    append_darwin_segalign_link_flags("${arch}" result)
  endif()

  set("${result_var_name}" "${result}" PARENT_SCOPE)
endfunction()

function(_add_variant_c_compile_flags
    sdk arch build_type enable_assertions result_var_name)
  set(result ${${result_var_name}})

  _add_variant_c_compile_link_flags(
      "${sdk}"
      "${arch}"
      "${build_type}"
      "${enable_assertions}"
      result)

  is_build_type_optimized("${build_type}" optimized)
  if(optimized)
    list(APPEND result "-O2")

    # Add -momit-leaf-frame-pointer on x86.
    if("${arch}" STREQUAL "i386" OR "${arch}" STREQUAL "x86_64")
      list(APPEND result "-momit-leaf-frame-pointer")
    endif()
  else()
    list(APPEND result "-O0")
  endif()

  is_build_type_with_debuginfo("${build_type}" debuginfo)
  if(debuginfo)
    list(APPEND result "-g")
  else()
    list(APPEND result "-g0")
  endif()

  if(enable_assertions)
    list(APPEND result "-UNDEBUG")
  else()
    list(APPEND result "-DNDEBUG")
  endif()

  set("${result_var_name}" "${result}" PARENT_SCOPE)
endfunction()

function(_add_variant_swift_compile_flags
    sdk arch build_type enable_assertions result_var_name)
  set(result ${${result_var_name}})

  list(APPEND result
      "-sdk" "${SWIFT_SDK_${sdk}_PATH}"
      "-target" "${SWIFT_SDK_${sdk}_ARCH_${arch}_TRIPLE}")

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    list(APPEND result
        "-F" "${SWIFT_SDK_${sdk}_PATH}/../../../Developer/Library/Frameworks")

    append_darwin_segalign_link_flags("${arch}" result)
  endif()

  is_build_type_optimized("${build_type}" optimized)
  if(optimized)
    list(APPEND result "-O")
  else()
    list(APPEND result "-Onone")
  endif()

  is_build_type_with_debuginfo("${build_type}" debuginfo)
  if(debuginfo)
    list(APPEND result "-g")
  endif()

  if(enable_assertions)
    list(APPEND result "-D" "INTERNAL_CHECKS_ENABLED")
  endif()

  set("${result_var_name}" "${result}" PARENT_SCOPE)
endfunction()

function(_add_variant_link_flags
    sdk arch build_type enable_assertions result_var_name)

  if("${sdk}" STREQUAL "")
    message(FATAL_ERROR "Should specify an SDK")
  endif()

  if("${arch}" STREQUAL "")
    message(FATAL_ERROR "Should specify an architecture")
  endif()

  set(result ${${result_var_name}})

  _add_variant_c_compile_link_flags(
      "${sdk}"
      "${arch}"
      "${build_type}"
      "${enable_assertions}"
      result)

  if("${sdk}" STREQUAL "LINUX")
    list(APPEND result "-lpthread" "-ldl")
  else()
    list(APPEND result "-lobjc")
  endif()

  set("${result_var_name}" "${result}" PARENT_SCOPE)
endfunction()

# Look up extra flags for a module that matches a regexp.
function(_add_extra_swift_flags_for_module module_name result_var_name)
  set(result_list)
  list(LENGTH SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS listlen)
  if (${listlen} GREATER 0)
    math(EXPR listlen "${listlen}-1")
    foreach(i RANGE 0 ${listlen} 2)
      list(GET SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS ${i} regex)
      if (module_name MATCHES "${regex}")
        math(EXPR ip1 "${i}+1")
        list(GET SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS ${ip1} flags)
        list(APPEND result_list ${flags})
        message(STATUS "Matched '${regex}' to module '${module_name}'. Compiling ${module_name} with special flags: ${flags}")
      endif()
    endforeach()
  endif()
  set("${result_var_name}" ${result_list} PARENT_SCOPE)
endfunction()

# Compile a swift file into an object file (as a library).
#
# Usage:
#   _compile_swift_files(OUTPUT objfile # Name of the resulting object file
#     SOURCES swift_src [swift_src...]  # Swift source files to compile
#     FLAGS -module-name foo            # Flags to add to the compilation
#     [SDK sdk]                         # SDK to build for
#     [ARCHITECTURE architecture]       # Architecture to build for
#     [DEPENDS cmake_target...]         # CMake targets on which the object
#                                       # file depends.
#     [IS_MAIN]                         # This is an executable, not a library
#     [IS_STDLIB]
#     [IS_STDLIB_CORE]                  # This is the core standard library
#     [OPT_FLAGS]                       # Optimization flags (overrides SWIFT_OPTIMIZE)
#     [MODULE_DIR]                      # Put .swiftmodule, .swiftdoc., and .o
#                                       # into this directory.
#     [MODULE_NAME]                     # The module name. If not specified, the name
#                                       # is derived from the output name
#     [IS_STDLIB]                       # Install produced files.
#     )
function(_compile_swift_files dependency_target_out_var_name)
  parse_arguments(SWIFTFILE
    "OUTPUT;SOURCES;FLAGS;DEPENDS;SDK;ARCHITECTURE;API_NOTES;OPT_FLAGS;MODULE_DIR;MODULE_NAME;INSTALL_IN_COMPONENT"
    "IS_MAIN;IS_STDLIB;IS_STDLIB_CORE;IS_SDK_OVERLAY"
    ${ARGN})

  # Check arguments.
  list(LENGTH SWIFTFILE_OUTPUT num_outputs)
  list(GET SWIFTFILE_OUTPUT 0 first_output)

  if (${num_outputs} EQUAL 0)
    message(FATAL_ERROR "OUTPUT must not be empty")
  endif()

  foreach(output ${SWIFTFILE_OUTPUT})
    if (NOT IS_ABSOLUTE "${output}")
      message(FATAL_ERROR "OUTPUT should be an absolute path")
    endif()
  endforeach()

  if(SWIFTFILE_IS_MAIN AND SWIFTFILE_IS_STDLIB)
    message(FATAL_ERROR "Cannot set both IS_MAIN and IS_STDLIB")
  endif()

  if("${SWIFTFILE_SDK}" STREQUAL "")
    message(FATAL_ERROR "Should specify an SDK")
  endif()

  if("${SWIFTFILE_ARCHITECTURE}" STREQUAL "")
    message(FATAL_ERROR "Should specify an architecture")
  endif()

  if("${SWIFTFILE_INSTALL_IN_COMPONENT}" STREQUAL "")
    message(FATAL_ERROR "INSTALL_IN_COMPONENT is required")
  endif()

  set(source_files)
  foreach(file ${SWIFTFILE_SOURCES})
    # Determine where this file is.
    get_filename_component(file_path ${file} PATH)
    if(IS_ABSOLUTE "${file_path}")
      list(APPEND source_files "${file}")
    else()
      list(APPEND source_files "${CMAKE_CURRENT_SOURCE_DIR}/${file}")
    endif()
  endforeach()

  # Compute flags for the Swift compiler.
  set(swift_flags)

  _add_variant_swift_compile_flags(
      "${SWIFTFILE_SDK}"
      "${SWIFTFILE_ARCHITECTURE}"
      "${SWIFT_STDLIB_BUILD_TYPE}"
      "${SWIFT_STDLIB_ASSERTIONS}"
      swift_flags)

  # Determine the subdirectory where the binary should be placed.
  compute_library_subdir(library_subdir
      "${SWIFTFILE_SDK}" "${SWIFTFILE_ARCHITECTURE}")

  # Allow import of other Swift modules we just built.
  list(APPEND swift_flags
      "-I" "${SWIFTLIB_DIR}/${library_subdir}")
  # FIXME: should we use '-resource-dir' here?  Seems like it has no advantage
  # over '-I' in this case.

  # If we have a custom module cache path, use it.
  if (SWIFT_MODULE_CACHE_PATH)
    list(APPEND swift_flags "-module-cache-path" "${SWIFT_MODULE_CACHE_PATH}")
  endif()

  # Don't include libarclite in any build products by default.
  list(APPEND swift_flags "-no-link-objc-runtime")

  if(SWIFT_VERIFY_ALL)
    list(APPEND swift_flags "-Xfrontend" "-sil-verify-all")
  endif()

  if(SWIFT_STDLIB_USE_ASSERT_CONFIG_RELEASE)
    list(APPEND swift_flags "-assert-config" "Release")
  endif()

  if(SWIFT_EMIT_SORTED_SIL_OUTPUT)
    list(APPEND swift_flags "-Xfrontend" "-emit-sorted-sil")
  endif()

  # FIXME: Cleaner way to do this?
  if(SWIFTFILE_IS_STDLIB_CORE)
    list(APPEND swift_flags
        "-nostdimport" "-parse-stdlib" "-module-name" "Swift"
        "-Xfrontend" "-sil-serialize-all")
  endif()

  if(SWIFTFILE_IS_SDK_OVERLAY)
    list(APPEND swift_flags "-autolink-force-load")
  endif()

  list(APPEND swift_flags ${SWIFT_EXPERIMENTAL_EXTRA_FLAGS})

  if (SWIFT_EXPERIMENTAL_ENABLE_GUARANTEED_SELF)
    list(APPEND swift_flags "-Xfrontend" "-enable-guaranteed-self")
  endif()

  if(SWIFTFILE_OPT_FLAGS)
    list(APPEND swift_flags ${SWIFTFILE_OPT_FLAGS})
  endif()

  list(APPEND swift_flags ${SWIFTFILE_FLAGS})

  set(obj_dirs)
  foreach(output ${SWIFTFILE_OUTPUT})
    get_filename_component(objdir "${output}" PATH)
    list(APPEND obj_dirs "${objdir}")
  endforeach()
  list(REMOVE_DUPLICATES obj_dirs)

  set(command_create_dirs)
  foreach(objdir ${obj_dirs})
    list(APPEND command_create_dirs
      COMMAND "${CMAKE_COMMAND}" -E make_directory "${objdir}")
  endforeach()

  set(module_file)
  set(module_doc_file)

  if(NOT SWIFTFILE_IS_MAIN)
    # Determine the directory where the module file should be placed.
    if (SWIFTFILE_MODULE_NAME)
      set(module_name "${SWIFTFILE_MODULE_NAME}")
      list(APPEND swift_flags
          "-module-name" "${module_name}")
    else()
      get_filename_component(module_name "${first_output}" NAME_WE)
    endif()
    if(SWIFTFILE_MODULE_DIR)
      set(module_dir "${SWIFTFILE_MODULE_DIR}")
    elseif(SWIFTFILE_IS_STDLIB)
      set(module_dir "${SWIFTLIB_DIR}/${library_subdir}")
    else()
      message(FATAL_ERROR "Don't know where to put the module files")
    endif()

    set(module_file "${module_dir}/${module_name}.swiftmodule")
    set(module_doc_file "${module_dir}/${module_name}.swiftdoc")
    list(APPEND swift_flags
        "-parse-as-library"
        "-emit-module" "-emit-module-path" "${module_file}")

    list(APPEND command_create_dirs
        COMMAND "${CMAKE_COMMAND}" -E make_directory "${module_dir}")

    # If we have extra regexp flags, check if we match any of the regexps. If so
    # add the relevant flags to our swift_flags.
    if (SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS)
      set(extra_swift_flags_for_module)
      _add_extra_swift_flags_for_module("${module_name}" extra_swift_flags_for_module)
      if (extra_swift_flags_for_module)
        list(APPEND swift_flags ${extra_swift_flags_for_module})
      endif()
    endif()
  endif()

  swift_install_in_component("${SWIFTFILE_INSTALL_IN_COMPONENT}"
      FILES "${module_file}" "${module_doc_file}"
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${library_subdir}")

  # Generate API notes if requested.
  set(command_create_apinotes)
  set(depends_create_apinotes)
  set(apinote_files)
  foreach(apinote_module ${SWIFTFILE_API_NOTES})
    set(apinote_file "${module_dir}/${apinote_module}.apinotesc")
    set(apinote_input_file
        "${SWIFT_API_NOTES_PATH}/${apinote_module}.apinotes")
    set(CLANG_APINOTES "${SWIFT_NATIVE_CLANG_TOOLS_PATH}/clang")

    list(APPEND command_create_apinotes
        COMMAND
          "${CLANG_APINOTES}" "-cc1apinotes" "-yaml-to-binary"
          "-o" "${apinote_file}"
          "-target" "${SWIFT_SDK_${SWIFTFILE_SDK}_ARCH_${SWIFTFILE_ARCHITECTURE}_TRIPLE}"
          "${apinote_input_file}")
    list(APPEND depends_create_apinotes "${apinote_input_file}")

    list(APPEND apinote_files "${apinote_file}")
    swift_install_in_component("${SWIFTFILE_INSTALL_IN_COMPONENT}"
        FILES ${apinote_file}
        DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${library_subdir}")
  endforeach()

  set(line_directive_tool "${SWIFT_SOURCE_DIR}/utils/line-directive")
  set(swift_compiler_tool "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/swiftc")
  set(swift_compiler_tool_dep)
  if(SWIFT_BUILD_TOOLS)
    # Depend on the binary itself, in addition to the symlink.
    set(swift_compiler_tool_dep "swift")
  endif()

  # If there are more than one output files, we assume that they are specified
  # otherwise e.g. with an output file map.
  set(output_option)
  if (${num_outputs} EQUAL 1)
    set(output_option "-o" ${first_output})
  endif()

  add_custom_command_target(
      dependency_target
      ${command_create_dirs}
      # Create API notes before compiling, because this will affect the APIs
      # the overlay sees.
      ${command_create_apinotes}
      COMMAND
        "${line_directive_tool}" "${source_files}" --
        "${swift_compiler_tool}" "-c" ${swift_flags}
        ${output_option} "${source_files}"
      OUTPUT
        ${SWIFTFILE_OUTPUT} "${module_file}" "${module_doc_file}"
        ${apinote_files}
      DEPENDS
        "${swift_compiler_tool}" ${swift_compiler_tool_dep}
        ${source_files} ${SWIFTFILE_DEPENDS}
        ${swift_ide_test_dependency} ${depends_create_apinotes}
      COMMENT "Compiling ${first_output}")
  set("${dependency_target_out_var_name}" "${dependency_target}" PARENT_SCOPE)

  # Make sure the build system knows the file is a generated object file.
  set_source_files_properties(${SWIFTFILE_OUTPUT}
      PROPERTIES
      GENERATED true
      EXTERNAL_OBJECT true
      LANGUAGE C
      OBJECT_DEPENDS "${source_files}")
endfunction()

# Process the sources within the given variable, pulling out any Swift
# sources to be compiled with 'swift' directly. This updates
# ${sourcesvar} in place with the resulting list and ${externalvar} with the
# list of externally-build sources.
#
# Usage:
#   handle_swift_sources(sourcesvar externalvar)
function(handle_swift_sources
    dependency_target_out_var_name sourcesvar externalvar name)
  parse_arguments(SWIFTSOURCES
      "DEPENDS;SDK;ARCHITECTURE;API_NOTES;COMPILE_FLAGS;INSTALL_IN_COMPONENT"
      "IS_MAIN;IS_STDLIB;IS_STDLIB_CORE;IS_SDK_OVERLAY"
      ${ARGN})
  translate_flag(${SWIFTSOURCES_IS_MAIN} "IS_MAIN" IS_MAIN_arg)
  translate_flag(${SWIFTSOURCES_IS_STDLIB} "IS_STDLIB" IS_STDLIB_arg)
  translate_flag(${SWIFTSOURCES_IS_STDLIB_CORE} "IS_STDLIB_CORE"
                 IS_STDLIB_CORE_arg)
  translate_flag(${SWIFTSOURCES_IS_SDK_OVERLAY} "IS_SDK_OVERLAY"
                 IS_SDK_OVERLAY_arg)

  if(SWIFTSOURCES_IS_MAIN)
    set(SWIFTSOURCES_INSTALL_IN_COMPONENT never_install)
  endif()

  # Check arguments.
  if ("${SWIFTSOURCES_SDK}" STREQUAL "")
    message(FATAL_ERROR "Should specify an SDK")
  endif()

  if ("${SWIFTSOURCES_ARCHITECTURE}" STREQUAL "")
    message(FATAL_ERROR "Should specify an architecture")
  endif()

  if("${SWIFTSOURCES_INSTALL_IN_COMPONENT}" STREQUAL "")
    message(FATAL_ERROR "INSTALL_IN_COMPONENT is required")
  endif()

  # Clear the result variable.
  set("${dependency_target_out_var_name}" "" PARENT_SCOPE)

  set(result)
  set(swift_sources)
  foreach(src ${${sourcesvar}})
    get_filename_component(extension ${src} EXT)
    if(extension STREQUAL ".swift")
      list(APPEND swift_sources ${src})
    else()
      list(APPEND result ${src})
    endif()
  endforeach()

  set(swift_compile_flags ${SWIFTSOURCES_COMPILE_FLAGS})
  if (NOT SWIFTSOURCES_IS_MAIN)
    list(APPEND swift_compile_flags "-module-link-name" "${name}")
  endif()

  if(swift_sources)
    # Special-case hack to create Swift.o for the core standard library.
    if(SWIFTSOURCES_IS_STDLIB_CORE)
      set(swift_obj_base "Swift")
    else()
      # Otherwise, get the name from the first swift input file.  Also a hack!
      # TODO: Fix it, <rdar://problem/17535693>.
      list(GET swift_sources 0 swift_obj_base)
      get_filename_component(swift_obj_base ${swift_obj_base} NAME_WE)
    endif()

    compute_library_subdir(SWIFTSOURCES_LIBRARY_SUBDIR
      "${SWIFTSOURCES_SDK}" "${SWIFTSOURCES_ARCHITECTURE}")
    set(objsubdir "/${SWIFTSOURCES_LIBRARY_SUBDIR}")

    file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}${objsubdir}")

    set(swift_obj
        "${CMAKE_CURRENT_BINARY_DIR}${objsubdir}/${swift_obj_base}${CMAKE_C_OUTPUT_EXTENSION}")

    # FIXME: We shouldn't /have/ to build things in a single process.
    # <rdar://problem/15972329>
    list(APPEND swift_compile_flags "-force-single-frontend-invocation")

    _compile_swift_files(
        dependency_target
        OUTPUT ${swift_obj}
        SOURCES ${swift_sources}
        DEPENDS ${SWIFTSOURCES_DEPENDS}
        FLAGS ${swift_compile_flags}
        SDK ${SWIFTSOURCES_SDK}
        ARCHITECTURE ${SWIFTSOURCES_ARCHITECTURE}
        API_NOTES ${SWIFTSOURCES_API_NOTES}
        ${IS_MAIN_arg}
        ${IS_STDLIB_arg}
        ${IS_STDLIB_CORE_arg}
        ${IS_SDK_OVERLAY_arg}
        ${STATIC_arg}
        INSTALL_IN_COMPONENT "${SWIFTSOURCES_INSTALL_IN_COMPONENT}")
    set("${dependency_target_out_var_name}" "${dependency_target}" PARENT_SCOPE)
    list(APPEND result ${swift_obj})
  endif()

  llvm_process_sources(result ${result})
  set(${sourcesvar} "${result}" PARENT_SCOPE)
  set(${externalvar} ${swift_sources} PARENT_SCOPE)
endfunction()

# Add a universal binary target created from the output of the given
# set of targets by running 'lipo'.
#
# Usage:
#   _add_swift_lipo_target(
#     target              # The name of the target to create
#     output              # The file to be created by this target
#     source_targets...   # The source targets whose outputs will be
#                         # lipo'd into the output.
#   )
function(_add_swift_lipo_target target output)
  if("${target}" STREQUAL "")
    message(FATAL_ERROR "target is required")
  endif()

  if("${output}" STREQUAL "")
    message(FATAL_ERROR "output is required")
  endif()

  set(source_targets ${ARGN})

  # Gather the source binaries.
  set(source_binaries)
  foreach(source_target ${source_targets})
    if(SWIFT_CMAKE_HAS_GENERATOR_EXPRESSIONS)
      list(APPEND source_binaries $<TARGET_FILE:${source_target}>)
    else()
      get_property(source_binary
          TARGET ${source_target}
          PROPERTY LOCATION)
      list(APPEND source_binaries "${source_binary}")
    endif()
  endforeach()

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    # Use lipo to create the final binary.
    add_custom_command_target(unused_var
        COMMAND "${LIPO}" "-create" "-output" "${output}" ${source_binaries}
        CUSTOM_TARGET_NAME "${target}"
        OUTPUT "${output}"
        DEPENDS ${source_targets})
  else()
    # We don't know how to create fat binaries for other platforms.
    add_custom_command_target(unused_var
        COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${source_binaries}" "${output}"
        CUSTOM_TARGET_NAME "${target}"
        OUTPUT "${output}"
        DEPENDS ${source_targets})
  endif()
endfunction()

# Add a single variant of a new Swift library.
#
# Usage:
#   _add_swift_library_single(
#     target
#     name
#     [SHARED]
#     [SDK sdk]
#     [ARCHITECTURE architecture]
#     [DEPENDS dep1 ...]
#     [LINK_LIBRARIES dep1 ...]
#     [FRAMEWORK_DEPENDS dep1 ...]
#     [COMPONENT_DEPENDS comp1 ...]
#     [C_COMPILE_FLAGS flag1...]
#     [SWIFT_COMPILE_FLAGS flag1...]
#     [LINK_FLAGS flag1...]
#     [API_NOTES_NON_OVERLAY]
#     [FILE_DEPENDS target1 ...]
#     [IS_STDLIB]
#     [IS_STDLIB_CORE]
#     [IS_SDK_OVERLAY]
#     INSTALL_IN_COMPONENT comp
#     source1 [source2 source3 ...])
#
# target
#   Name of the target (e.g., swiftParse-IOS-armv7).
#
# name
#   Name of the library (e.g., swiftParse).
#
# SHARED
#   Build a shared library.
#
# SDK sdk
#   SDK to build for.
#
# ARCHITECTURE
#   Architecture to build for.
#
# DEPENDS
#   Targets that this library depends on.
#
# LINK_LIBRARIES
#   Libraries this library depends on.
#
# FRAMEWORK_DEPENDS
#   System frameworks this library depends on.
#
# COMPONENT_DEPENDS
#   LLVM components this library depends on.
#
# C_COMPILE_FLAGS
#   Extra compile flags (C, C++, ObjC).
#
# SWIFT_COMPILE_FLAGS
#   Extra compile flags (Swift).
#
# LINK_FLAGS
#   Extra linker flags.
#
# API_NOTES_NON_OVERLAY
#   Generate API notes for non-overlayed modules with this target.
#
# FILE_DEPENDS
#   Additional files this library depends on.
#
# IS_STDLIB
#   Install library dylib and swift module files to lib/swift.
#
# IS_STDLIB_CORE
#   Compile as the standard library core.
#
# IS_SDK_OVERLAY
#   Treat the library as a part of the Swift SDK overlay.
#
# INSTALL_IN_COMPONENT comp
#   The Swift installation component that this library belongs to.
#
# source1 ...
#   Sources to add into this library
function(_add_swift_library_single target name)
  set(SWIFTLIB_SINGLE_options
      SHARED IS_STDLIB IS_STDLIB_CORE IS_SDK_OVERLAY
      API_NOTES_NON_OVERLAY)
  parse_arguments(SWIFTLIB_SINGLE
    "DEPENDS;LINK_LIBRARIES;FRAMEWORK_DEPENDS;COMPONENT_DEPENDS;C_COMPILE_FLAGS;SWIFT_COMPILE_FLAGS;LINK_FLAGS;PRIVATE_LINK_LIBRARIES;INTERFACE_LINK_LIBRARIES;FILE_DEPENDS;SDK;ARCHITECTURE;INSTALL_IN_COMPONENT"
    "${SWIFTLIB_SINGLE_options}"
    ${ARGN})

  set(SWIFTLIB_SINGLE_SOURCES ${SWIFTLIB_SINGLE_DEFAULT_ARGS})

  translate_flags(SWIFTLIB_SINGLE "${SWIFTLIB_SINGLE_options}")

  # Check arguments.
  if ("${SWIFTLIB_SINGLE_SDK}" STREQUAL "")
    message(FATAL_ERROR "Should specify an SDK")
  endif()

  if ("${SWIFTLIB_SINGLE_ARCHITECTURE}" STREQUAL "")
    message(FATAL_ERROR "Should specify an architecture")
  endif()

  if("${SWIFTLIB_SINGLE_INSTALL_IN_COMPONENT}" STREQUAL "")
    message(FATAL_ERROR "INSTALL_IN_COMPONENT is required")
  endif()

  # Determine the subdirectory where this library will be installed.
  set(SWIFTLIB_SINGLE_SUBDIR
      "${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR}/${SWIFTLIB_SINGLE_ARCHITECTURE}")

  # Include LLVM Bitcode slices for iOS, Watch OS, and Apple TV OS device libraries.
  if(SWIFT_ALLOW_BITCODE_SECTION)
    if("${SWIFTLIB_SINGLE_SDK}" STREQUAL "IOS" OR "${SWIFTLIB_SINGLE_SDK}" STREQUAL "TVOS" OR "${SWIFTLIB_SINGLE_SDK}" STREQUAL "WATCHOS")
      set(SWIFTLIB_SINGLE_C_COMPILE_FLAGS "${SWIFTLIB_SINGLE_C_COMPILE_FLAGS}" "-fembed-bitcode")
      set(SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS "${SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS}" "-embed-bitcode")
      set(SWIFTLIB_SINGLE_LINK_FLAGS "${SWIFTLIB_SINGLE_LINK_FLAGS}" "-Xlinker -bitcode_bundle -Xlinker -bitcode_hide_symbols")
    endif()
  endif()


  if(MSVC_IDE OR XCODE)
    string(REGEX MATCHALL "/[^/]+" split_path ${CMAKE_CURRENT_SOURCE_DIR})
    list(GET split_path -1 dir)
    file(GLOB_RECURSE SWIFTLIB_SINGLE_HEADERS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.h
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.def)

    file(GLOB_RECURSE SWIFTLIB_SINGLE_TDS
      ${SWIFT_SOURCE_DIR}/include/swift${dir}/*.td)
    source_group("TableGen descriptions" FILES ${SWIFTLIB_SINGLE_TDS})

    set(SWIFTLIB_SINGLE_SOURCES ${SWIFTLIB_SINGLE_SOURCES} ${SWIFTLIB_SINGLE_HEADERS} ${SWIFTLIB_SINGLE_TDS})
  endif(MSVC_IDE OR XCODE)

  if(MODULE)
    set(libkind MODULE)
  elseif(SWIFTLIB_SINGLE_SHARED)
    set(libkind SHARED)
  else()
    set(libkind)
  endif()

  handle_gyb_sources(
      gyb_dependency_targets
      SWIFTLIB_SINGLE_SOURCES
      "${SWIFTLIB_SINGLE_ARCHITECTURE}")

  if (SWIFT_RUNTIME_ENABLE_DTRACE)
    handle_dtrace_sources(
      dtrace_dependency_targets
      SWIFTLIB_SINGLE_SOURCES
      dtrace_include_directories)
  endif()

  # Figure out whether and which API notes to create.
  set(SWIFTLIB_SINGLE_API_NOTES)
  if(SWIFTLIB_SINGLE_API_NOTES_NON_OVERLAY)
    # Adopt all of the non-overlay API notes.
    foreach(framework_name ${SWIFT_API_NOTES_INPUTS})
      if (${framework_name} STREQUAL "WatchKit" AND
          ${SWIFTLIB_SINGLE_SDK} STREQUAL "OSX")
        # HACK: don't build WatchKit API notes for OS X.
      else()
        if (NOT IS_DIRECTORY "${SWIFT_SOURCE_DIR}/stdlib/public/SDK/${framework_name}")
          list(APPEND SWIFTLIB_SINGLE_API_NOTES "${framework_name}")
        endif()
      endif()
    endforeach()
  endif()

  # Remove the "swift" prefix from the name to determine the module name.
  string(REPLACE swift "" module_name "${name}")
  list(FIND SWIFT_API_NOTES_INPUTS "${module_name}" overlay_index)
  if(NOT ${overlay_index} EQUAL -1)
    set(SWIFTLIB_SINGLE_API_NOTES "${module_name}")
  endif()

  # FIXME: don't actually depend on the libraries in SWIFTLIB_SINGLE_LINK_LIBRARIES,
  # just any swiftmodule files that are associated with them.
  handle_swift_sources(
      swift_object_dependency_target
      SWIFTLIB_SINGLE_SOURCES
      SWIFTLIB_SINGLE_EXTERNAL_SOURCES ${name}
      DEPENDS
        ${gyb_dependency_targets}
        ${SWIFTLIB_SINGLE_FILE_DEPENDS}
        ${SWIFTLIB_SINGLE_LINK_LIBRARIES}
        ${SWIFTLIB_SINGLE_INTERFACE_LINK_LIBRARIES}
      SDK ${SWIFTLIB_SINGLE_SDK}
      ARCHITECTURE ${SWIFTLIB_SINGLE_ARCHITECTURE}
      API_NOTES ${SWIFTLIB_SINGLE_API_NOTES}
      COMPILE_FLAGS ${SWIFTLIB_SINGLE_SWIFT_COMPILE_FLAGS}
      ${SWIFTLIB_SINGLE_IS_STDLIB_keyword}
      ${SWIFTLIB_SINGLE_IS_STDLIB_CORE_keyword}
      ${SWIFTLIB_SINGLE_IS_SDK_OVERLAY_keyword}
      INSTALL_IN_COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}")

  add_library("${target}" ${libkind}
      ${SWIFTLIB_SINGLE_SOURCES}
      ${SWIFTLIB_SINGLE_EXTERNAL_SOURCES})

  if (dtrace_dependency_targets)
    add_dependencies("${target}" ${dtrace_dependency_targets})
    include_directories(${dtrace_include_directories})
  endif()

  llvm_update_compile_flags(${target})

  set_output_directory(${target}
      ${SWIFT_RUNTIME_OUTPUT_INTDIR}
      ${SWIFT_LIBRARY_OUTPUT_INTDIR})

  if(MODULE)
    set_target_properties("${target}" PROPERTIES
        PREFIX ""
        SUFFIX ${LLVM_PLUGIN_EXT})
  endif()

  # Add Swift source files to the project.
  source_group("Swift Sources" FILES ${SWIFTLIB_SINGLE_EXTERNAL_SOURCES})
  # ...and for now, mark them as HEADER_FILE_ONLY, so that Xcode doesn't try
  # to build them itself.
  set_source_files_properties(${SWIFTLIB_SINGLE_EXTERNAL_SOURCES} PROPERTIES
    HEADER_FILE_ONLY true)

  if(SWIFTLIB_SINGLE_IS_STDLIB)
    # Install runtime libraries to lib/swift instead of lib. This works around
    # the fact that -isysroot prevents linking to libraries in the system
    # /usr/lib if Swift is installed in /usr.
    set_target_properties("${target}" PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}
      ARCHIVE_OUTPUT_DIRECTORY ${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR})

    foreach(config ${CMAKE_CONFIGURATION_TYPES})
      string(TOUPPER ${config} config_upper)
      apply_xcode_substitutions("${config}" "${SWIFTLIB_DIR}" config_lib_dir)
      set_target_properties(${target} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR}
        ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR})
    endforeach()
  endif()

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set_target_properties("${target}"
        PROPERTIES
        INSTALL_NAME_DIR "@rpath"
        BUILD_WITH_INSTALL_RPATH YES)
  endif()

  set_target_properties("${target}" PROPERTIES FOLDER "Swift libraries")

  # Configure the static library target.
  # Set compile and link flags for the non-static target.
  # Do these LAST.

  set(target_static)
  if(SWIFTLIB_SINGLE_IS_STDLIB AND SWIFT_BUILD_STATIC_STDLIB)
    set(target_static "${target}-static")

    # We have already compiled Swift sources.  Link everything into a static
    # library.
    add_library(${target_static} STATIC
        ${SWIFTLIB_SINGLE_SOURCES}

        # Note: the dummy.c source file provides no definitions. However,
        # it forces Xcode to properly link the static library.
        ${SWIFT_SOURCE_DIR}/cmake/dummy.c)

    set_output_directory(${target_static}
        ${SWIFT_RUNTIME_OUTPUT_INTDIR} ${SWIFT_LIBRARY_OUTPUT_INTDIR})

    foreach(config ${CMAKE_CONFIGURATION_TYPES})
      string(TOUPPER ${config} config_upper)
      apply_xcode_substitutions(
          "${config}" "${SWIFTSTATICLIB_DIR}" config_lib_dir)
      set_target_properties(${target_static} PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR}
        ARCHIVE_OUTPUT_DIRECTORY_${config_upper} ${config_lib_dir}/${SWIFTLIB_SINGLE_SUBDIR})
    endforeach()

    set_target_properties(${target_static} PROPERTIES
      LIBRARY_OUTPUT_DIRECTORY ${SWIFTSTATICLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}
      ARCHIVE_OUTPUT_DIRECTORY ${SWIFTSTATICLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR})
  endif()

  set_target_properties(${target}
      PROPERTIES
      # Library name (without the variant information)
      OUTPUT_NAME ${name})
  if(target_static)
    set_target_properties(${target_static}
        PROPERTIES
        OUTPUT_NAME ${name})
  endif()

  # Don't build standard libraries by default.  We will enable building
  # standard libraries that the user requested; the rest can be built on-demand.
  if(SWIFTLIB_SINGLE_IS_STDLIB)
    foreach(t "${target}" ${target_static})
      set_target_properties(${t} PROPERTIES EXCLUDE_FROM_ALL TRUE)
    endforeach()
  endif()

  # Handle linking and dependencies.
  add_dependencies_multiple_targets(
      TARGETS "${target}" ${target_static}
      DEPENDS
        ${SWIFTLIB_SINGLE_DEPENDS}
        ${gyb_dependency_targets}
        "${swift_object_dependency_target}"
        ${LLVM_COMMON_DEPENDS})

  # HACK: On some systems or build directory setups, CMake will not find static
  # archives of Clang libraries in the Clang build directory, and it will pass
  # them as '-lclangFoo'.  Some other logic in CMake would reorder libraries
  # specified with this syntax, which breaks linking.
  set(prefixed_link_libraries)
  foreach(dep ${SWIFTLIB_SINGLE_LINK_LIBRARIES})
    if("${dep}" MATCHES "^clang")
      set(dep "${LLVM_LIBRARY_OUTPUT_INTDIR}/lib${dep}.a")
    endif()
    if("${dep}" STREQUAL "cmark")
      set(dep "${CMARK_LIBRARY_DIR}/lib${dep}.a")
    endif()
    list(APPEND prefixed_link_libraries "${dep}")
  endforeach()
  set(SWIFTLIB_SINGLE_LINK_LIBRARIES "${prefixed_link_libraries}")

  if("${libkind}" STREQUAL "SHARED")
    target_link_libraries("${target}" PRIVATE ${SWIFTLIB_SINGLE_LINK_LIBRARIES})
  else()
    target_link_libraries("${target}" INTERFACE ${SWIFTLIB_SINGLE_LINK_LIBRARIES})
  endif()

  if(target_static)
    _list_add_string_suffix(
        "${SWIFTLIB_SINGLE_LINK_LIBRARIES}"
        "-static"
        target_static_depends)
    # FIXME: should this be target_link_libraries?
    add_dependencies_multiple_targets(
        TARGETS "${target_static}"
        DEPENDS ${target_static_depends})
  endif()

  # Link against system frameworks.
  foreach(FRAMEWORK ${SWIFTLIB_SINGLE_FRAMEWORK_DEPENDS})
    foreach(t "${target}" ${target_static})
      target_link_libraries("${t}" PUBLIC "-framework ${FRAMEWORK}")
    endforeach()
  endforeach()

  swift_common_llvm_config("${target}" ${SWIFTLIB_SINGLE_COMPONENT_DEPENDS})

  # Collect compile and link flags for the static and non-static targets.
  # Don't set PROPERTY COMPILE_FLAGS or LINK_FLAGS directly.
  set(c_compile_flags ${SWIFTLIB_SINGLE_C_COMPILE_FLAGS})
  set(link_flags ${SWIFTLIB_SINGLE_LINK_FLAGS})

  # Add variant-specific flags.
  if(SWIFTLIB_SINGLE_IS_STDLIB)
    set(build_type "${SWIFT_STDLIB_BUILD_TYPE}")
    set(enable_assertions "${SWIFT_STDLIB_ASSERTIONS}")
  else()
    set(build_type "${CMAKE_BUILD_TYPE}")
    set(enable_assertions "${LLVM_ENABLE_ASSERTIONS}")
  endif()
  _add_variant_c_compile_flags(
      "${SWIFTLIB_SINGLE_SDK}"
      "${SWIFTLIB_SINGLE_ARCHITECTURE}"
      "${build_type}"
      "${enable_assertions}"
      c_compile_flags)
  _add_variant_link_flags(
      "${SWIFTLIB_SINGLE_SDK}"
      "${SWIFTLIB_SINGLE_ARCHITECTURE}"
      "${build_type}"
      "${enable_assertions}"
      link_flags)

  # Configure plist creation for OS X.
  set(PLIST_INFO_PLIST "Info.plist" CACHE STRING "Plist name")
  if(APPLE AND SWIFTLIB_SINGLE_IS_STDLIB)
    set(PLIST_INFO_NAME ${name})
    set(PLIST_INFO_UTI "com.apple.dt.runtime.${name}")
    set(PLIST_INFO_VERSION "${SWIFT_VERSION}")
    if (LLVM_SUBMIT_VERSION)
      set(PLIST_INFO_BUILD_VERSION
          "${LLVM_SUBMIT_VERSION}.${LLVM_SUBMIT_SUBVERSION}")
    endif()

    set(PLIST_INFO_PLIST_OUT "${PLIST_INFO_PLIST}")
    list(APPEND link_flags
         "-Wl,-sectcreate,__TEXT,__info_plist,${CMAKE_CURRENT_BINARY_DIR}/${PLIST_INFO_PLIST_OUT}")
    configure_file(
        "${SWIFT_SOURCE_DIR}/stdlib/${PLIST_INFO_PLIST}.in"
        "${PLIST_INFO_PLIST_OUT}"
        @ONLY
        NEWLINE_STYLE UNIX)

    # If Application Extensions are enabled, pass the linker flag marking
    # the dylib as safe.
    if (CXX_SUPPORTS_FAPPLICATION_EXTENSION AND (NOT DISABLE_APPLICATION_EXTENSION))
      list(APPEND link_flags "-Wl,-application_extension")
    endif()

    set(PLIST_INFO_UTI)
    set(PLIST_INFO_NAME)
    set(PLIST_INFO_VERSION)
    set(PLIST_INFO_BUILD_VERSION)
  endif()

  # On Linux add the linker script that coalesces protocol conformance
  # sections. This wouldn't be necessary if the link was done by the swift
  # binary: rdar://problem/19007002
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    list(APPEND link_flags
        "-Xlinker" "-T"
        "-Xlinker" "${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}/swift.ld")
  endif()

  # Convert variables to space-separated strings.
  string(REPLACE ";" " " c_compile_flags "${c_compile_flags}")
  string(REPLACE ";" " " link_flags "${link_flags}")

  # Set compilation and link flags.
  set_property(TARGET "${target}" APPEND_STRING PROPERTY
      COMPILE_FLAGS " ${c_compile_flags}")
  set_property(TARGET "${target}" APPEND_STRING PROPERTY
    LINK_FLAGS " ${link_flags} -L${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFTLIB_SINGLE_SUBDIR} -L${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR} -L${SWIFTLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}")
  target_link_libraries("${target}" PRIVATE
      ${SWIFTLIB_SINGLE_PRIVATE_LINK_LIBRARIES})
  if(${SWIFTLIB_SINGLE_INTERFACE_LINK_LIBRARIES})
    message(FATAL_ERROR "${SWIFTLIB_SINGLE_INTERFACE_LINK_LIBRARIES}")
  endif()
  target_link_libraries("${target}" INTERFACE
      ${SWIFTLIB_SINGLE_INTERFACE_LINK_LIBRARIES})
  set_property(TARGET "${target}" PROPERTY
      LINKER_LANGUAGE "CXX")

  if(target_static)
    set_property(TARGET "${target_static}" APPEND_STRING PROPERTY
        COMPILE_FLAGS " ${c_compile_flags}")
    set_property(TARGET "${target_static}" APPEND_STRING PROPERTY
      LINK_FLAGS " ${link_flags} -L${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFTLIB_SINGLE_SUBDIR} -L${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/../lib/swift/${SWIFT_SDK_${SWIFTLIB_SINGLE_SDK}_LIB_SUBDIR} -L${SWIFTSTATICLIB_DIR}/${SWIFTLIB_SINGLE_SUBDIR}")
    target_link_libraries("${target_static}" PRIVATE
        ${SWIFTLIB_SINGLE_PRIVATE_LINK_LIBRARIES})
  endif()

  # Do not add code here.
endfunction()

# Add a new Swift library.
#
# Usage:
#   add_swift_library(name
#     [SHARED]
#     [DEPENDS dep1 ...]
#     [LINK_LIBRARIES dep1 ...]
#     [INTERFACE_LINK_LIBRARIES dep1 ...]
#     [SWIFT_MODULE_DEPENDS dep1 ...]
#     [FRAMEWORK_DEPENDS dep1 ...]
#     [COMPONENT_DEPENDS comp1 ...]
#     [FILE_DEPENDS target1 ...]
#     [TARGET_SDKS sdk1...]
#     [C_COMPILE_FLAGS flag1...]
#     [SWIFT_COMPILE_FLAGS flag1...]
#     [LINK_FLAGS flag1...]
#     [API_NOTES_NON_OVERLAY]
#     [INSTALL]
#     [IS_STDLIB]
#     [IS_STDLIB_CORE]
#     [TARGET_LIBRARY]
#     INSTALL_IN_COMPONENT comp
#     source1 [source2 source3 ...])
#
# name
#   Name of the library (e.g., swiftParse).
#
# SHARED
#   Build a shared library.
#
# DEPENDS
#   Targets that this library depends on.
#
# LINK_LIBRARIES
#   Libraries this library depends on.
#
# SWIFT_MODULE_DEPENDS
#   Swift modules this library depends on.
#
# SWIFT_MODULE_DEPENDS_OSX
#   Swift modules this library depends on when built for OS X.
#
# SWIFT_MODULE_DEPENDS_IOS_TVOS
#   Swift modules this library depends on when built for iOS or tvOS.
#
# FRAMEWORK_DEPENDS
#   System frameworks this library depends on.
#
# COMPONENT_DEPENDS
#   LLVM components this library depends on.
#
# FILE_DEPENDS
#   Additional files this library depends on.
#
# TARGET_SDKS
#   The set of SDKs in which this library is included. If empty, the library
#   is included in all SDKs.
#
# C_COMPILE_FLAGS
#   Extra compiler flags (C, C++, ObjC).
#
# SWIFT_COMPILE_FLAGS
#   Extra compiler flags (Swift).
#
# LINK_FLAGS
#   Extra linker flags.
#
# API_NOTES_NON_OVERLAY
#   Generate API notes for non-overlayed modules with this target.
#
# IS_STDLIB
#   Treat the library as a part of the Swift standard library.
#   IS_STDLIB implies TARGET_LIBRARY.
#
# IS_STDLIB_CORE
#   Compile as the Swift standard library core.
#
# IS_SDK_OVERLAY
#   Treat the library as a part of the Swift SDK overlay.
#   IS_SDK_OVERLAY implies TARGET_LIBRARY and IS_STDLIB.
#
# TARGET_LIBRARY
#   Build library for the target SDKs.
#
# INSTALL_IN_COMPONENT comp
#   The Swift installation component that this library belongs to.
#
# source1 ...
#   Sources to add into this library.
function(add_swift_library name)
  set(SWIFTLIB_options
      SHARED IS_STDLIB IS_STDLIB_CORE IS_SDK_OVERLAY TARGET_LIBRARY
      API_NOTES_NON_OVERLAY)
  parse_arguments(SWIFTLIB
    "DEPENDS;LINK_LIBRARIES;SWIFT_MODULE_DEPENDS;SWIFT_MODULE_DEPENDS_OSX;SWIFT_MODULE_DEPENDS_IOS_TVOS;FRAMEWORK_DEPENDS;COMPONENT_DEPENDS;FILE_DEPENDS;TARGET_SDKS;C_COMPILE_FLAGS;SWIFT_COMPILE_FLAGS;LINK_FLAGS;PRIVATE_LINK_LIBRARIES;INTERFACE_LINK_LIBRARIES;INSTALL_IN_COMPONENT"
      "${SWIFTLIB_options}"
      ${ARGN})
  set(SWIFTLIB_SOURCES ${SWIFTLIB_DEFAULT_ARGS})

  # Infer arguments.

  if(SWIFTLIB_IS_SDK_OVERLAY)
    set(SWIFTLIB_SHARED TRUE)
    set(SWIFTLIB_IS_STDLIB TRUE)
    set(SWIFTLIB_TARGET_LIBRARY TRUE)

    # There are no experimental SDK overlays.
    set(SWIFTLIB_INSTALL_IN_COMPONENT sdk-overlay)
  endif()

  # Standard library is always a target library.
  if(SWIFTLIB_IS_STDLIB)
    set(SWIFTLIB_TARGET_LIBRARY TRUE)
  endif()

  if(NOT SWIFTLIB_TARGET_LIBRARY)
    set(SWIFTLIB_INSTALL_IN_COMPONENT dev)
  endif()

  # If target SDKs are not specified, build for all known SDKs.
  if("${SWIFTLIB_TARGET_SDKS}" STREQUAL "")
    set(SWIFTLIB_TARGET_SDKS ${SWIFT_SDKS})
  endif()

  # All Swift code depends on the standard library, except for the standard
  # library itself.
  if(SWIFTLIB_TARGET_LIBRARY AND NOT SWIFTLIB_IS_STDLIB_CORE)
    list(APPEND SWIFTLIB_SWIFT_MODULE_DEPENDS Core)
  endif()

  if((NOT "${SWIFT_BUILD_STDLIB}") AND
     (NOT "${SWIFTLIB_SWIFT_MODULE_DEPENDS}" STREQUAL ""))
    list(REMOVE_ITEM SWIFTLIB_SWIFT_MODULE_DEPENDS
        Core SwiftPrivate)
  endif()

  translate_flags(SWIFTLIB "${SWIFTLIB_options}")

  if("${SWIFTLIB_INSTALL_IN_COMPONENT}" STREQUAL "")
    message(FATAL_ERROR "INSTALL_IN_COMPONENT is required")
  endif()

  if(SWIFTLIB_TARGET_LIBRARY)
    # If we are building this library for targets, loop through the various
    # SDKs building the variants of this library.
    list_intersect(
        "${SWIFTLIB_TARGET_SDKS}" "${SWIFT_SDKS}" SWIFTLIB_TARGET_SDKS)
    foreach(sdk ${SWIFTLIB_TARGET_SDKS})
      set(THIN_INPUT_TARGETS)

      # For each architecture supported by this SDK
      foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
        # Configure variables for this subdirectory.
        set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
        set(VARIANT_NAME "${name}${VARIANT_SUFFIX}")

        # Map dependencies over to the appropriate variants.
        set(swiftlib_link_libraries)
        foreach(lib ${SWIFTLIB_LINK_LIBRARIES})
          if(TARGET "${lib}${VARIANT_SUFFIX}")
            list(APPEND swiftlib_link_libraries "${lib}${VARIANT_SUFFIX}")
          else()
            list(APPEND swiftlib_link_libraries "${lib}")
          endif()
        endforeach()

        set(swiftlib_module_depends_flattened ${SWIFTLIB_SWIFT_MODULE_DEPENDS})
        if("${sdk}" STREQUAL "OSX")
          list(APPEND swiftlib_module_depends_flattened
              ${SWIFTLIB_SWIFT_MODULE_DEPENDS_OSX})
        elseif("${sdk}" STREQUAL "IOS" OR "${sdk}" STREQUAL "IOS_SIMULATOR" OR "${sdk}" STREQUAL "TVOS" OR "${sdk}" STREQUAL "TVOS_SIMULATOR")
          list(APPEND swiftlib_module_depends_flattened
              ${SWIFTLIB_SWIFT_MODULE_DEPENDS_IOS_TVOS})
        endif()

        set(swiftlib_module_dependency_targets)
        foreach(mod ${swiftlib_module_depends_flattened})
          list(APPEND swiftlib_module_dependency_targets
              "swift${mod}${VARIANT_SUFFIX}")
        endforeach()

        set(swiftlib_private_link_libraries_targets
            ${swiftlib_module_dependency_targets})
        foreach(lib ${SWIFTLIB_PRIVATE_LINK_LIBRARIES})
          if(TARGET "${lib}${VARIANT_SUFFIX}")
            list(APPEND swiftlib_private_link_libraries_targets
                "${lib}${VARIANT_SUFFIX}")
          else()
            list(APPEND swiftlib_private_link_libraries_targets "${lib}")
          endif()
        endforeach()

        # Add this library variant.
        _add_swift_library_single(
          ${VARIANT_NAME}
          ${name}
          ${SWIFTLIB_SHARED_keyword}
          ${SWIFTLIB_SOURCES}
          SDK ${sdk}
          ARCHITECTURE ${arch}
          DEPENDS ${SWIFTLIB_DEPENDS}
          LINK_LIBRARIES ${swiftlib_link_libraries}
          FRAMEWORK_DEPENDS ${SWIFTLIB_FRAMEWORK_DEPENDS}
          COMPONENT_DEPENDS ${SWIFTLIB_COMPONENT_DEPENDS}
          FILE_DEPENDS ${SWIFTLIB_FILE_DEPENDS} ${swiftlib_module_dependency_targets}
          C_COMPILE_FLAGS ${SWIFTLIB_C_COMPILE_FLAGS}
          SWIFT_COMPILE_FLAGS ${SWIFTLIB_SWIFT_COMPILE_FLAGS}
          LINK_FLAGS ${SWIFTLIB_LINK_FLAGS}
          PRIVATE_LINK_LIBRARIES ${swiftlib_private_link_libraries_targets}
          ${SWIFTLIB_API_NOTES_NON_OVERLAY_keyword}
          ${SWIFTLIB_IS_STDLIB_keyword}
          ${SWIFTLIB_IS_STDLIB_CORE_keyword}
          ${SWIFTLIB_IS_SDK_OVERLAY_keyword}
          INSTALL_IN_COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}")

        # Add dependencies on the (not-yet-created) custom lipo target.
        foreach(DEP ${SWIFTLIB_LINK_LIBRARIES})
          add_dependencies(${VARIANT_NAME}
              "${DEP}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}")
        endforeach()

        if (SWIFT_BUILD_STATIC_STDLIB AND SWIFTLIB_IS_STDLIB)
          # Add dependencies on the (not-yet-created) custom lipo target.
          foreach(DEP ${SWIFTLIB_LINK_LIBRARIES})
            add_dependencies("${VARIANT_NAME}-static"
                "${DEP}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-static")
          endforeach()
        endif()

        # Note this thin library.
        list(APPEND THIN_INPUT_TARGETS ${VARIANT_NAME})
      endforeach()

      # Determine the name of the universal library.
      if(SWIFTLIB_SHARED)
        set(UNIVERSAL_LIBRARY_NAME
          "${SWIFTLIB_DIR}/${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${name}${CMAKE_SHARED_LIBRARY_SUFFIX}")
      else()
        set(UNIVERSAL_LIBRARY_NAME
          "${SWIFTLIB_DIR}/${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}${name}${CMAKE_STATIC_LIBRARY_SUFFIX}")
      endif()

      set(lipo_target "${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}")
      _add_swift_lipo_target(
          ${lipo_target}
          "${UNIVERSAL_LIBRARY_NAME}"
          ${THIN_INPUT_TARGETS})

      # Determine the subdirectory where this library will be installed.
      set(resource_dir_sdk_subdir "${SWIFT_SDK_${sdk}_LIB_SUBDIR}")

      if("${resource_dir_sdk_subdir}" STREQUAL "")
        message(FATAL_ERROR "internal error: the variable should be non-empty")
      endif()

      if(SWIFTLIB_IS_STDLIB)
        if(SWIFTLIB_SHARED)
          set(resource_dir "swift")
          set(file_permissions
              OWNER_READ OWNER_WRITE OWNER_EXECUTE
              GROUP_READ GROUP_EXECUTE
              WORLD_READ WORLD_EXECUTE)
        else()
          set(resource_dir "swift_static")
          set(file_permissions
              OWNER_READ OWNER_WRITE
              GROUP_READ
              WORLD_READ)
        endif()

        set(install_paths
            "lib${LLVM_LIBDIR_SUFFIX}/${resource_dir}/${resource_dir_sdk_subdir}")
        if(SWIFTLIB_SHARED AND
           (NOT "${SWIFT_STDLIB_DYLIB_EXTRA_INSTALL_PATHS}" STREQUAL ""))
          list(LENGTH SWIFT_STDLIB_DYLIB_EXTRA_INSTALL_PATHS listlen)
          if(${listlen} GREATER 0)
            math(EXPR listlen "${listlen}-1")
            foreach(i RANGE 0 ${listlen} 2)
              list(GET SWIFT_STDLIB_DYLIB_EXTRA_INSTALL_PATHS ${i} match_sdk_name)
              if("${sdk}" STREQUAL "${match_sdk_name}")
                math(EXPR ip1 "${i}+1")
                list(GET SWIFT_STDLIB_DYLIB_EXTRA_INSTALL_PATHS ${ip1} dylib_install_path)
                list(APPEND install_paths "${dylib_install_path}")
              endif()
            endforeach()
          endif()
        endif()

        foreach(p ${install_paths})
          swift_install_in_component("${SWIFTLIB_INSTALL_IN_COMPONENT}"
              FILES "${UNIVERSAL_LIBRARY_NAME}"
              DESTINATION "${p}"
              PERMISSIONS ${file_permissions})
        endforeach()
      endif()

      # If we built static variants of the library, create a lipo target for
      # them.
      set(lipo_target_static)
      if (SWIFT_BUILD_STATIC_STDLIB AND SWIFTLIB_IS_STDLIB)
        set(THIN_INPUT_TARGETS_STATIC)
        foreach(TARGET ${THIN_INPUT_TARGETS})
          list(APPEND THIN_INPUT_TARGETS_STATIC "${TARGET}-static")
        endforeach()

        set(lipo_target_static
            "${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-static")
        set(UNIVERSAL_LIBRARY_NAME
            "${SWIFTSTATICLIB_DIR}/${SWIFT_SDK_${sdk}_LIB_SUBDIR}/${CMAKE_STATIC_LIBRARY_PREFIX}${name}${CMAKE_STATIC_LIBRARY_SUFFIX}")
        _add_swift_lipo_target(
            ${lipo_target_static}
            "${UNIVERSAL_LIBRARY_NAME}"
            ${THIN_INPUT_TARGETS_STATIC})
        swift_install_in_component("${SWIFTLIB_INSTALL_IN_COMPONENT}"
            FILES "${UNIVERSAL_LIBRARY_NAME}"
            DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift_static/${resource_dir_sdk_subdir}"
            PERMISSIONS
              OWNER_READ OWNER_WRITE
              GROUP_READ
              WORLD_READ)
      endif()

      # Add Swift standard library targets as dependencies to the top-level
      # convenience target.
      if(SWIFTLIB_IS_STDLIB)
        foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
          set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
          add_dependencies("swift-stdlib${VARIANT_SUFFIX}"
              ${lipo_target}
              ${lipo_target_static})
        endforeach()
      endif()
    endforeach()
  else()
    set(sdk "${SWIFT_HOST_VARIANT_SDK}")
    set(arch "${SWIFT_HOST_VARIANT_ARCH}")

    _add_swift_library_single(
      ${name}
      ${name}
      ${SWIFTLIB_SHARED_keyword}
      ${SWIFTLIB_SOURCES}
      SDK ${sdk}
      ARCHITECTURE ${arch}
      DEPENDS ${SWIFTLIB_DEPENDS}
      LINK_LIBRARIES ${SWIFTLIB_LINK_LIBRARIES}
      FRAMEWORK_DEPENDS ${SWIFTLIB_FRAMEWORK_DEPENDS}
      COMPONENT_DEPENDS ${SWIFTLIB_COMPONENT_DEPENDS}
      FILE_DEPENDS ${SWIFTLIB_FILE_DEPENDS}
      C_COMPILE_FLAGS ${SWIFTLIB_C_COMPILE_FLAGS}
      SWIFT_COMPILE_FLAGS ${SWIFTLIB_SWIFT_COMPILE_FLAGS}
      LINK_FLAGS ${SWIFTLIB_LINK_FLAGS}
      PRIVATE_LINK_LIBRARIES ${SWIFTLIB_PRIVATE_LINK_LIBRARIES}
      INTERFACE_LINK_LIBRARIES ${SWIFTLIB_INTERFACE_LINK_LIBRARIES}
      ${SWIFTLIB_API_NOTES_NON_OVERLAY_keyword}
      ${SWIFTLIB_IS_STDLIB_keyword}
      ${SWIFTLIB_IS_STDLIB_CORE_keyword}
      ${SWIFTLIB_IS_SDK_OVERLAY_keyword}
      INSTALL_IN_COMPONENT "${SWIFTLIB_INSTALL_IN_COMPONENT}")
  endif()
endfunction()

# Add an executable compiled for a given variant.
#
# Don't use directly, use add_swift_executable and add_swift_target_executable
# instead.
#
# See add_swift_executable for detailed documentation.
#
# Additional parameters:
#   [SDK sdk]
#     SDK to build for.
#
#   [ARCHITECTURE architecture]
#     Architecture to build for.
#
#   [LINK_FAT_LIBRARIES lipo_target1 ...]
#     Fat libraries to link with.
function(_add_swift_executable_single name)
  # Parse the arguments we were given.
  parse_arguments(SWIFTEXE_SINGLE
    "DEPENDS;COMPONENT_DEPENDS;LINK_LIBRARIES;SDK;ARCHITECTURE;LINK_FAT_LIBRARIES"
    "EXCLUDE_FROM_ALL;DONT_STRIP_NON_MAIN_SYMBOLS;DISABLE_ASLR"
    ${ARGN})

  set(SWIFTEXE_SINGLE_SOURCES ${SWIFTEXE_SINGLE_DEFAULT_ARGS})

  translate_flag(${SWIFTEXE_SINGLE_EXCLUDE_FROM_ALL}
      "EXCLUDE_FROM_ALL"
      SWIFTEXE_SINGLE_EXCLUDE_FROM_ALL_FLAG)

  # Check arguments.
  if (NOT SWIFTEXE_SINGLE_SDK)
    message(FATAL_ERROR "Should specify an SDK")
  endif()

  if (NOT SWIFTEXE_SINGLE_ARCHITECTURE)
    message(FATAL_ERROR "Should specify an architecture")
  endif()

  # Determine compiler flags.
  set(c_compile_flags)
  set(link_flags)

  # Add variant-specific flags.
  _add_variant_c_compile_flags(
      "${SWIFTEXE_SINGLE_SDK}"
      "${SWIFTEXE_SINGLE_ARCHITECTURE}"
      "${CMAKE_BUILD_TYPE}"
      "${LLVM_ENABLE_ASSERTIONS}"
      c_compile_flags)
  _add_variant_link_flags(
      "${SWIFTEXE_SINGLE_SDK}"
      "${SWIFTEXE_SINGLE_ARCHITECTURE}"
      "${CMAKE_BUILD_TYPE}"
      "${LLVM_ENABLE_ASSERTIONS}"
      link_flags)

  list(APPEND link_flags
      "-L${SWIFTLIB_DIR}/${SWIFT_SDK_${SWIFTEXE_SINGLE_SDK}_LIB_SUBDIR}")

  foreach(FAT_LIBRARY ${SWIFTEXE_SINGLE_LINK_FAT_LIBRARIES})
    list(APPEND link_flags "-l${FAT_LIBRARY}")
  endforeach()

  if(SWIFTEXE_SINGLE_DISABLE_ASLR)
    list(APPEND link_flags "-Wl,-no_pie")
  endif()

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    list(APPEND link_flags
        "-Xlinker" "-rpath"
        "-Xlinker" "@executable_path/../lib/swift/${SWIFT_SDK_${SWIFTEXE_SINGLE_SDK}_LIB_SUBDIR}")
  endif()

  # Find the names of dependency library targets.
  #
  # We don't add the ${ARCH} to the target suffix because we want to link
  # against fat libraries.
  _list_add_string_suffix(
      "${SWIFTEXE_SINGLE_LINK_FAT_LIBRARIES}"
      "-${SWIFT_SDK_${SWIFTEXE_SINGLE_SDK}_LIB_SUBDIR}"
      SWIFTEXE_SINGLE_LINK_FAT_LIBRARIES_TARGETS)

  handle_swift_sources(
      dependency_target
      SWIFTEXE_SINGLE_SOURCES SWIFTEXE_SINGLE_EXTERNAL_SOURCES ${name}
      DEPENDS
        ${SWIFTEXE_SINGLE_DEPENDS}
        ${SWIFTEXE_SINGLE_LINK_FAT_LIBRARIES_TARGETS}
      SDK ${SWIFTEXE_SINGLE_SDK}
      ARCHITECTURE ${SWIFTEXE_SINGLE_ARCHITECTURE}
      IS_MAIN)

  add_executable(${name}
      ${SWIFTEXE_SINGLE_EXCLUDE_FROM_ALL_FLAG}
      ${SWIFTEXE_SINGLE_SOURCES}
      ${SWIFTEXE_SINGLE_EXTERNAL_SOURCES})

  add_dependencies_multiple_targets(
      TARGETS "${name}"
      DEPENDS
        ${dependency_target}
        ${LLVM_COMMON_DEPENDS}
        ${SWIFTEXE_SINGLE_DEPENDS}
        ${SWIFTEXE_SINGLE_LINK_FAT_LIBRARIES_TARGETS})
  llvm_update_compile_flags("${name}")

  # Convert variables to space-separated strings.
  string(REPLACE ";" " " c_compile_flags "${c_compile_flags}")
  string(REPLACE ";" " " link_flags "${link_flags}")

  set_property(TARGET ${name} APPEND_STRING PROPERTY
      COMPILE_FLAGS " ${c_compile_flags}")
  set_property(TARGET ${name} APPEND_STRING PROPERTY
      LINK_FLAGS " ${link_flags}")

  set_output_directory(${name} ${SWIFT_RUNTIME_OUTPUT_INTDIR} ${SWIFT_LIBRARY_OUTPUT_INTDIR})

  # Add Swift source files to the project.
  source_group("Swift Sources" FILES ${SWIFTEXE_SINGLE_EXTERNAL_SOURCES})
  # ...and for now, mark them as HEADER_FILE_ONLY, so that Xcode doesn't try
  # to build them itself.
  set_source_files_properties(${SWIFTEXE_SINGLE_EXTERNAL_SOURCES}
      PROPERTIES
      HEADER_FILE_ONLY true)

  target_link_libraries("${name}" ${SWIFTEXE_SINGLE_LINK_LIBRARIES})
  swift_common_llvm_config("${name}" ${SWIFTEXE_SINGLE_COMPONENT_DEPENDS})

  set_target_properties(${name}
      PROPERTIES FOLDER "Swift executables")
endfunction()

# Add an executable for the target machine.
#
# See add_swift_executable for detailed documentation.
#
# Additional parameters:
#   [LINK_FAT_LIBRARIES lipo_target1 ...]
#     Fat libraries to link with.
function(add_swift_target_executable name)
  # Parse the arguments we were given.
  parse_arguments(SWIFTEXE_TARGET
    "DEPENDS;COMPONENT_DEPENDS;LINK_FAT_LIBRARIES"
    "EXCLUDE_FROM_ALL;DONT_STRIP_NON_MAIN_SYMBOLS;DISABLE_ASLR"
    ${ARGN})

  set(SWIFTEXE_TARGET_SOURCES ${SWIFTEXE_TARGET_DEFAULT_ARGS})

  translate_flag(${SWIFTEXE_TARGET_EXCLUDE_FROM_ALL}
      "EXCLUDE_FROM_ALL"
      SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG)
  translate_flag(${SWIFTEXE_TARGET_DONT_STRIP_NON_MAIN_SYMBOLS}
      "DONT_STRIP_NON_MAIN_SYMBOLS"
      SWIFTEXE_TARGET_DONT_STRIP_NON_MAIN_SYMBOLS_FLAG)
  translate_flag(${SWIFTEXE_TARGET_DISABLE_ASLR}
      "DISABLE_ASLR"
      SWIFTEXE_DISABLE_ASLR_FLAG)

  # All Swift executables depend on the standard library.
  list(APPEND SWIFTEXE_TARGET_LINK_FAT_LIBRARIES swiftCore)

  if(NOT "${SWIFT_BUILD_STDLIB}")
    list(REMOVE_ITEM SWIFTEXE_TARGET_LINK_FAT_LIBRARIES
        swiftCore swiftSwiftPrivate)
  endif()

  foreach(sdk ${SWIFT_SDKS})
    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
      set(VARIANT_NAME "${name}${VARIANT_SUFFIX}")

      set(SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG_CURRENT
          ${SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG})
      if(NOT "${VARIANT_SUFFIX}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SUFFIX}")
        # By default, don't build executables for target SDKs to avoid building
        # target stdlibs.
        set(SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG_CURRENT "EXCLUDE_FROM_ALL")
      endif()

      # Don't add the ${arch} to the suffix.  We want to link against fat
      # libraries.
      _list_add_string_suffix(
          "${SWIFTEXE_TARGET_DEPENDS}"
          "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}"
          SWIFTEXE_TARGET_DEPENDS_with_suffix)
      _add_swift_executable_single(
          ${VARIANT_NAME}
          ${SWIFTEXE_TARGET_SOURCES}
          DEPENDS ${SWIFTEXE_TARGET_DEPENDS_with_suffix}
          COMPONENT_DEPENDS ${SWIFTEXE_TARGET_COMPONENT_DEPENDS}
          SDK "${sdk}"
          ARCHITECTURE "${arch}"
          LINK_FAT_LIBRARIES ${SWIFTEXE_TARGET_LINK_FAT_LIBRARIES}
          ${SWIFTEXE_TARGET_EXCLUDE_FROM_ALL_FLAG_CURRENT}
          ${SWIFTEXE_TARGET_DONT_STRIP_NON_MAIN_SYMBOLS_FLAG}
          ${SWIFTEXE_DISABLE_ASLR_FLAG})
    endforeach()
  endforeach()
endfunction()

# Add an executable for the host machine.
#
# Usage:
#   add_swift_executable(name
#     [DEPENDS dep1 ...]
#     [COMPONENT_DEPENDS comp1 ...]
#     [FILE_DEPENDS target1 ...]
#     [LINK_LIBRARIES target1 ...]
#     [EXCLUDE_FROM_ALL]
#     [DONT_STRIP_NON_MAIN_SYMBOLS]
#     [DISABLE_ASLR]
#     source1 [source2 source3 ...])
#
#   name
#     Name of the executable (e.g., swift).
#
#   LIBRARIES
#     Libraries this executable depends on, without variant suffixes.
#
#   COMPONENT_DEPENDS
#     LLVM components this executable depends on.
#
#   FILE_DEPENDS
#     Additional files this executable depends on.
#
#   LINK_LIBRARIES
#     Libraries to link with.
#
#   EXCLUDE_FROM_ALL
#     Whether to exclude this executable from the ALL_BUILD target.
#
#   DONT_STRIP_NON_MAIN_SYMBOLS
#     Should we not strip non main symbols.
#
#   DISABLE_ASLR
#     Should we compile with -Wl,-no_pie so that ASLR is disabled?
#
#   source1 ...
#     Sources to add into this executable.
function(add_swift_executable name)
  # Parse the arguments we were given.
  parse_arguments(SWIFTEXE
    "DEPENDS;COMPONENT_DEPENDS;LINK_LIBRARIES"
    "EXCLUDE_FROM_ALL;DONT_STRIP_NON_MAIN_SYMBOLS;DISABLE_ASLR"
    ${ARGN})

  translate_flag(${SWIFTEXE_EXCLUDE_FROM_ALL}
      "EXCLUDE_FROM_ALL"
      SWIFTEXE_EXCLUDE_FROM_ALL_FLAG)
  translate_flag(${SWIFTEXE_DONT_STRIP_NON_MAIN_SYMBOLS}
      "DONT_STRIP_NON_MAIN_SYMBOLS"
      SWIFTEXE_DONT_STRIP_NON_MAIN_SYMBOLS_FLAG)
  translate_flag(${SWIFTEXE_DISABLE_ASLR}
      "DISABLE_ASLR"
      SWIFTEXE_DISABLE_ASLR_FLAG)

  set(SWIFTEXE_SOURCES ${SWIFTEXE_DEFAULT_ARGS})

  # Note: host tools don't get a variant suffix.
  _add_swift_executable_single(
      ${name}
      ${SWIFTEXE_SOURCES}
      DEPENDS ${SWIFTEXE_DEPENDS}
      COMPONENT_DEPENDS ${SWIFTEXE_COMPONENT_DEPENDS}
      LINK_LIBRARIES ${SWIFTEXE_LINK_LIBRARIES}
      SDK ${SWIFT_HOST_VARIANT_SDK}
      ARCHITECTURE ${SWIFT_HOST_VARIANT_ARCH}
      ${SWIFTEXE_EXCLUDE_FROM_ALL_FLAG}
      ${SWIFTEXE_DONT_STRIP_NON_MAIN_SYMBOLS_FLAG}
      ${SWIFTEXE_DISABLE_ASLR_FLAG})
endfunction()

function(add_swift_llvm_loadable_module name)
  add_llvm_loadable_module(${name} ${ARGN})
  set(sdk "${SWIFT_HOST_VARIANT_SDK}")
  set(arch "${SWIFT_HOST_VARIANT_ARCH}")

  # Determine compiler flags.
  set(c_compile_flags)
  _add_variant_c_compile_flags(
      "${sdk}"
      "${arch}"
      "${CMAKE_BUILD_TYPE}"
      "${LLVM_ENABLE_ASSERTIONS}"
      c_compile_flags)

  set(link_flags)
  _add_variant_link_flags(
      "${sdk}"
      "${arch}"
      "${CMAKE_BUILD_TYPE}"
      "${LLVM_ENABLE_ASSERTIONS}"
      link_flags)

  # Convert variables to space-separated strings.
  string(REPLACE ";" " " c_compile_flags "${c_compile_flags}")
  string(REPLACE ";" " " link_flags "${link_flags}")

  set_property(TARGET ${name} APPEND_STRING PROPERTY
      COMPILE_FLAGS " ${c_compile_flags}")
  set_property(TARGET ${name} APPEND_STRING PROPERTY
      LINK_FLAGS " ${link_flags}")
endfunction()
