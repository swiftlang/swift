# Process the sources within the given variable, pulling out any Swift
# sources to be compiled with 'swift' directly. This updates
# ${sourcesvar} in place with the resulting list and ${externalvar} with the
# list of externally-build sources.
#
# Usage:
#   handle_swift_sources(sourcesvar externalvar)
function(handle_swift_sources
    dependency_target_out_var_name sourcesvar externalvar name)
  cmake_parse_arguments(SWIFTSOURCES
      "IS_MAIN;IS_STDLIB;IS_STDLIB_CORE;IS_SDK_OVERLAY"
      "SDK;ARCHITECTURE;INSTALL_IN_COMPONENT"
      "DEPENDS;API_NOTES;COMPILE_FLAGS"
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
  if("${SWIFTSOURCES_SDK}" STREQUAL "CYGWIN")
    list(APPEND swift_compile_flags -DCYGWIN)
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

    if(SWIFTSOURCES_IS_STDLIB_CORE)
      list(APPEND swift_compile_flags "-Xcc" "-D__SWIFT_CURRENT_DYLIB=swiftCore")
    endif()

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

# Add Swift source files to the (Xcode) project.
#
# Usage:
#   add_swift_source_group(sources)
function(add_swift_source_group sources)
  source_group("Swift Sources" FILES ${sources})
  # Mark the source files as HEADER_FILE_ONLY, so that Xcode doesn't try
  # to build them itself.
  set_source_files_properties(${sources}
    PROPERTIES
    HEADER_FILE_ONLY true)
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
#     [EMIT_SIB]                        # Emit the file as a sib file instead of a .o
#     )
function(_compile_swift_files dependency_target_out_var_name)
  cmake_parse_arguments(SWIFTFILE
    "IS_MAIN;IS_STDLIB;IS_STDLIB_CORE;IS_SDK_OVERLAY;EMIT_SIB"
    "OUTPUT;MODULE_NAME;INSTALL_IN_COMPONENT"
    "SOURCES;FLAGS;DEPENDS;SDK;ARCHITECTURE;API_NOTES;OPT_FLAGS;MODULE_DIR"
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

  if(SWIFT_SIL_VERIFY_ALL)
    list(APPEND swift_flags "-Xfrontend" "-sil-verify-all")
  endif()

  if(SWIFT_STDLIB_ENABLE_RESILIENCE AND SWIFTFILE_IS_STDLIB)
    list(APPEND swift_flags "-Xfrontend" "-enable-resilience")
  endif()

  if(SWIFT_EMIT_SORTED_SIL_OUTPUT)
    list(APPEND swift_flags "-Xfrontend" "-emit-sorted-sil")
  endif()

  # FIXME: Cleaner way to do this?
  if(SWIFTFILE_IS_STDLIB_CORE)
    list(APPEND swift_flags
        "-nostdimport" "-parse-stdlib" "-module-name" "Swift")
    list(APPEND swift_flags "-Xfrontend" "-group-info-path"
                            "-Xfrontend" "${GROUP_INFO_JSON_FILE}")
    if (NOT SWIFT_STDLIB_ENABLE_RESILIENCE)
      if (SWIFT_STDLIB_SIL_SERIALIZE_ALL)
        list(APPEND swift_flags "-Xfrontend" "-sil-serialize-all")
      endif()
    endif()
  endif()

  if(SWIFTFILE_IS_SDK_OVERLAY)
    list(APPEND swift_flags "-autolink-force-load")
  endif()

  list(APPEND swift_flags ${SWIFT_EXPERIMENTAL_EXTRA_FLAGS})

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

    if (NOT SWIFTFILE_EMIT_SIB)
      # Right now sib files seem to not be output when we emit a module. So
      # don't emit it.
      set(module_file "${module_dir}/${module_name}.swiftmodule")
      set(module_doc_file "${module_dir}/${module_name}.swiftdoc")
      list(APPEND swift_flags
          "-parse-as-library"
          "-emit-module" "-emit-module-path" "${module_file}")
    else()
      list(APPEND swift_flags "-parse-as-library")
    endif()

    list(APPEND command_create_dirs
        COMMAND "${CMAKE_COMMAND}" -E make_directory "${module_dir}")

    # If we have extra regexp flags, check if we match any of the regexps. If so
    # add the relevant flags to our swift_flags.
    if (SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS OR SWIFT_EXPERIMENTAL_EXTRA_NEGATIVE_REGEXP_FLAGS)
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

  set(line_directive_tool "${SWIFT_SOURCE_DIR}/utils/line-directive")
  set(swift_compiler_tool "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/swiftc")
  set(swift_compiler_tool_dep)
  if(SWIFT_INCLUDE_TOOLS)
    # Depend on the binary itself, in addition to the symlink.
    set(swift_compiler_tool_dep "swift")
  endif()

  # Generate API notes if requested.
  set(command_create_apinotes)
  set(depends_create_apinotes)
  set(apinote_files)

  # If we use sib don't emit api notes for now.
  if (NOT SWIFTFILE_EMIT_SIB)
    foreach(apinote_module ${SWIFTFILE_API_NOTES})
      set(apinote_file "${module_dir}/${apinote_module}.apinotesc")
      set(apinote_input_file
        "${SWIFT_API_NOTES_PATH}/${apinote_module}.apinotes")

      list(APPEND command_create_apinotes
        COMMAND
        "${swift_compiler_tool}" "-apinotes" "-yaml-to-binary"
        "-o" "${apinote_file}"
        "-target" "${SWIFT_SDK_${SWIFTFILE_SDK}_ARCH_${SWIFTFILE_ARCHITECTURE}_TRIPLE}"
        "${apinote_input_file}")
      list(APPEND depends_create_apinotes "${apinote_input_file}")

      list(APPEND apinote_files "${apinote_file}")
      swift_install_in_component("${SWIFTFILE_INSTALL_IN_COMPONENT}"
        FILES ${apinote_file}
        DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${library_subdir}")
    endforeach()
  endif()

  # If there are more than one output files, we assume that they are specified
  # otherwise e.g. with an output file map.
  set(output_option)
  if (${num_outputs} EQUAL 1)
    set(output_option "-o" ${first_output})
  endif()

  set(main_command "-c")
  if (SWIFTFILE_EMIT_SIB)
    # Change the command to emit-sib if we are asked to emit sib
    set(main_command "-emit-sib")
  endif()

  if (SWIFT_CHECK_INCREMENTAL_COMPILATION)
    set(swift_compiler_tool "${SWIFT_SOURCE_DIR}/utils/check-incremental" "${swift_compiler_tool}")
  endif()

  set(outputs
    ${SWIFTFILE_OUTPUT} "${module_file}" "${module_doc_file}"
    ${apinote_files})

  if(XCODE)
    # HACK: work around an issue with CMake Xcode generator and the Swift
    # driver.
    #
    # The Swift driver does not update the mtime of the output files if the
    # existing output files on disk are identical to the ones that are about
    # to be written.  This behavior confuses the makefiles used in CMake Xcode
    # projects: the makefiles will not consider everything up to date after
    # invoking the compiler.  As a result, the standard library gets rebuilt
    # multiple times during a single build.
    #
    # To work around this issue we touch the output files so that their mtime
    # always gets updated.
    set(command_touch_outputs
      COMMAND "${CMAKE_COMMAND}" -E touch ${outputs})
  endif()

  add_custom_command_target(
      dependency_target
      ${command_create_dirs}
      # Create API notes before compiling, because this will affect the APIs
      # the overlay sees.
      ${command_create_apinotes}
      COMMAND
        "${line_directive_tool}" "${source_files}" --
        "${swift_compiler_tool}" "${main_command}" ${swift_flags}
        ${output_option} "${source_files}"
      ${command_touch_outputs}
      OUTPUT ${outputs}
      DEPENDS
        ${swift_compiler_tool_dep}
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

