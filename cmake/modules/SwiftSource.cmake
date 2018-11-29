include(SwiftUtils)

# Process the sources within the given variable, pulling out any Swift
# sources to be compiled with 'swift' directly. This updates
# ${sourcesvar} in place with the resulting list and ${externalvar} with the
# list of externally-build sources.
#
# Usage:
#   handle_swift_sources(sourcesvar externalvar)
function(handle_swift_sources
    dependency_target_out_var_name
    dependency_module_target_out_var_name
    dependency_sib_target_out_var_name
    dependency_sibopt_target_out_var_name
    dependency_sibgen_target_out_var_name
    sourcesvar externalvar name)
  cmake_parse_arguments(SWIFTSOURCES
      "IS_MAIN;IS_STDLIB;IS_STDLIB_CORE;IS_SDK_OVERLAY;EMBED_BITCODE"
      "SDK;ARCHITECTURE;INSTALL_IN_COMPONENT"
      "DEPENDS;API_NOTES;COMPILE_FLAGS;MODULE_NAME"
      ${ARGN})
  translate_flag(${SWIFTSOURCES_IS_MAIN} "IS_MAIN" IS_MAIN_arg)
  translate_flag(${SWIFTSOURCES_IS_STDLIB} "IS_STDLIB" IS_STDLIB_arg)
  translate_flag(${SWIFTSOURCES_IS_STDLIB_CORE} "IS_STDLIB_CORE"
                 IS_STDLIB_CORE_arg)
  translate_flag(${SWIFTSOURCES_IS_SDK_OVERLAY} "IS_SDK_OVERLAY"
                 IS_SDK_OVERLAY_arg)
  translate_flag(${SWIFTSOURCES_EMBED_BITCODE} "EMBED_BITCODE"
                 EMBED_BITCODE_arg)

  if(SWIFTSOURCES_IS_MAIN)
    set(SWIFTSOURCES_INSTALL_IN_COMPONENT never_install)
  endif()

  # Check arguments.
  precondition(SWIFTSOURCES_SDK "Should specify an SDK")
  precondition(SWIFTSOURCES_ARCHITECTURE "Should specify an architecture")
  precondition(SWIFTSOURCES_INSTALL_IN_COMPONENT "INSTALL_IN_COMPONENT is required")

  # Clear the result variable.
  set("${dependency_target_out_var_name}" "" PARENT_SCOPE)
  set("${dependency_module_target_out_var_name}" "" PARENT_SCOPE)
  set("${dependency_sib_target_out_var_name}" "" PARENT_SCOPE)
  set("${dependency_sibopt_target_out_var_name}" "" PARENT_SCOPE)
  set("${dependency_sibgen_target_out_var_name}" "" PARENT_SCOPE)

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
    compute_library_subdir(SWIFTSOURCES_LIBRARY_SUBDIR
      "${SWIFTSOURCES_SDK}" "${SWIFTSOURCES_ARCHITECTURE}")
    set(objsubdir "/${SWIFTSOURCES_LIBRARY_SUBDIR}")

    file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}${objsubdir}")

    set(swift_obj
        "${CMAKE_CURRENT_BINARY_DIR}${objsubdir}/${SWIFTSOURCES_MODULE_NAME}${CMAKE_C_OUTPUT_EXTENSION}")

    # FIXME: We shouldn't /have/ to build things in a single process.
    # <rdar://problem/15972329>
    list(APPEND swift_compile_flags "-force-single-frontend-invocation")

    if(SWIFTSOURCES_IS_STDLIB_CORE)
      list(APPEND swift_compile_flags "-Xcc" "-D__SWIFT_CURRENT_DYLIB=swiftCore")
    endif()

    _compile_swift_files(
        dependency_target
        module_dependency_target
        sib_dependency_target
        sibopt_dependency_target
        sibgen_dependency_target
        OUTPUT ${swift_obj}
        SOURCES ${swift_sources}
        DEPENDS ${SWIFTSOURCES_DEPENDS}
        FLAGS ${swift_compile_flags}
        SDK ${SWIFTSOURCES_SDK}
        ARCHITECTURE ${SWIFTSOURCES_ARCHITECTURE}
        API_NOTES ${SWIFTSOURCES_API_NOTES}
        MODULE_NAME ${SWIFTSOURCES_MODULE_NAME}
        ${IS_MAIN_arg}
        ${IS_STDLIB_arg}
        ${IS_STDLIB_CORE_arg}
        ${IS_SDK_OVERLAY_arg}
        ${EMBED_BITCODE_arg}
        ${STATIC_arg}
        INSTALL_IN_COMPONENT "${SWIFTSOURCES_INSTALL_IN_COMPONENT}")
    set("${dependency_target_out_var_name}" "${dependency_target}" PARENT_SCOPE)
    set("${dependency_module_target_out_var_name}" "${module_dependency_target}" PARENT_SCOPE)
    set("${dependency_sib_target_out_var_name}" "${sib_dependency_target}" PARENT_SCOPE)
    set("${dependency_sibopt_target_out_var_name}" "${sibopt_dependency_target}" PARENT_SCOPE)
    set("${dependency_sibgen_target_out_var_name}" "${sibgen_dependency_target}" PARENT_SCOPE)

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
#   _compile_swift_files(
#     dependency_target_out_var_name
#     dependency_module_target_out_var_name
#     dependency_sib_target_out_var_name    # -Onone sib target
#     dependency_sibopt_target_out_var_name # -O sib target
#     dependency_sibgen_target_out_var_name # -sibgen target
#     OUTPUT objfile                    # Name of the resulting object file
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
#     [MODULE_NAME]                     # The module name.
#     [INSTALL_IN_COMPONENT]            # Install produced files.
#     [EMBED_BITCODE]                   # Embed LLVM bitcode into the .o files
#     )
function(_compile_swift_files
    dependency_target_out_var_name dependency_module_target_out_var_name
    dependency_sib_target_out_var_name dependency_sibopt_target_out_var_name
    dependency_sibgen_target_out_var_name)
  cmake_parse_arguments(SWIFTFILE
    "IS_MAIN;IS_STDLIB;IS_STDLIB_CORE;IS_SDK_OVERLAY;EMBED_BITCODE"
    "OUTPUT;MODULE_NAME;INSTALL_IN_COMPONENT"
    "SOURCES;FLAGS;DEPENDS;SDK;ARCHITECTURE;API_NOTES;OPT_FLAGS;MODULE_DIR"
    ${ARGN})

  # Check arguments.
  list(LENGTH SWIFTFILE_OUTPUT num_outputs)
  list(GET SWIFTFILE_OUTPUT 0 first_output)

  precondition(num_outputs MESSAGE "OUTPUT must not be empty")

  foreach(output ${SWIFTFILE_OUTPUT})
    if (NOT IS_ABSOLUTE "${output}")
      message(FATAL_ERROR "OUTPUT should be an absolute path")
    endif()
  endforeach()

  if(SWIFTFILE_IS_MAIN AND SWIFTFILE_IS_STDLIB)
    message(FATAL_ERROR "Cannot set both IS_MAIN and IS_STDLIB")
  endif()

  precondition(SWIFTFILE_SDK MESSAGE "Should specify an SDK")
  precondition(SWIFTFILE_ARCHITECTURE MESSAGE "Should specify an architecture")
  precondition(SWIFTFILE_INSTALL_IN_COMPONENT MESSAGE "INSTALL_IN_COMPONENT is required")

  if ("${SWIFTFILE_MODULE_NAME}" STREQUAL "")
    get_filename_component(SWIFTFILE_MODULE_NAME "${first_output}" NAME_WE)
    message(SEND_ERROR
      "No module name specified; did you mean to use MODULE_NAME "
      "${SWIFTFILE_MODULE_NAME}?")
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

  # The standard library and overlays are always built with resilience.
  if(SWIFTFILE_IS_STDLIB)
    list(APPEND swift_flags "-Xfrontend" "-enable-resilience")
  endif()

  if(SWIFT_STDLIB_USE_NONATOMIC_RC)
    list(APPEND swift_flags "-Xfrontend" "-assume-single-threaded")
  endif()

  if(SWIFTFILE_IS_STDLIB)
    list(APPEND swift_flags "-Xfrontend" "-enable-sil-ownership")
    list(APPEND swift_flags "-Xfrontend" "-enable-mandatory-semantic-arc-opts")
  endif()

  if(NOT SWIFT_ENABLE_STDLIBCORE_EXCLUSIVITY_CHECKING AND SWIFTFILE_IS_STDLIB)
    list(APPEND swift_flags "-Xfrontend" "-enforce-exclusivity=unchecked")
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
  endif()

  # Force swift 5 mode for Standard Library.
  if (SWIFTFILE_IS_STDLIB)
    list(APPEND swift_flags "-swift-version" "5")
  endif()
  
  # Force swift 4 compatibility mode for overlays.
  if (SWIFTFILE_IS_SDK_OVERLAY)
    list(APPEND swift_flags "-swift-version" "4")
  endif()

  if(SWIFTFILE_IS_SDK_OVERLAY)
    list(APPEND swift_flags "-autolink-force-load")
  endif()

  if (SWIFTFILE_IS_STDLIB_CORE OR SWIFTFILE_IS_SDK_OVERLAY)
    list(APPEND swift_flags "-warn-swift3-objc-inference-complete")
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
    if(SWIFTFILE_MODULE_DIR)
      set(module_dir "${SWIFTFILE_MODULE_DIR}")
    elseif(SWIFTFILE_IS_STDLIB)
      set(module_dir "${SWIFTLIB_DIR}/${library_subdir}")
    else()
      message(FATAL_ERROR "Don't know where to put the module files")
    endif()

    list(APPEND swift_flags "-parse-as-library")

    set(module_base "${module_dir}/${SWIFTFILE_MODULE_NAME}")
    set(module_file "${module_base}.swiftmodule")
    set(sib_file "${module_base}.Onone.sib")
    set(sibopt_file "${module_base}.O.sib")
    set(sibgen_file "${module_base}.sibgen")
    set(module_doc_file "${module_base}.swiftdoc")
    set(interface_file "${module_base}.swiftinterface")

    list(APPEND command_create_dirs
        COMMAND "${CMAKE_COMMAND}" -E make_directory "${module_dir}")

    # If we have extra regexp flags, check if we match any of the regexps. If so
    # add the relevant flags to our swift_flags.
    if (SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS OR SWIFT_EXPERIMENTAL_EXTRA_NEGATIVE_REGEXP_FLAGS)
      set(extra_swift_flags_for_module)
      _add_extra_swift_flags_for_module("${SWIFTFILE_MODULE_NAME}" extra_swift_flags_for_module)
      if (extra_swift_flags_for_module)
        list(APPEND swift_flags ${extra_swift_flags_for_module})
      endif()
    endif()
  endif()

  swift_install_in_component("${SWIFTFILE_INSTALL_IN_COMPONENT}"
    FILES "${module_file}" "${module_doc_file}" "${interface_file}"
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

  foreach(apinote_module ${SWIFTFILE_API_NOTES})
    set(apinote_file "${module_dir}/${apinote_module}.apinotes")
    set(apinote_input_file
      "${SWIFT_API_NOTES_PATH}/${apinote_module}.apinotes")

    list(APPEND command_create_apinotes
      COMMAND
      "${CMAKE_COMMAND}" "-E" "copy_if_different"
      "${apinote_input_file}" "${apinote_file}")
    list(APPEND depends_create_apinotes "${apinote_input_file}")

    list(APPEND apinote_files "${apinote_file}")
    swift_install_in_component("${SWIFTFILE_INSTALL_IN_COMPONENT}"
      FILES ${apinote_file}
      DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${library_subdir}")
  endforeach()

  # If there are more than one output files, we assume that they are specified
  # otherwise e.g. with an output file map.
  set(output_option)
  if (${num_outputs} EQUAL 1)
    set(output_option "-o" ${first_output})
  endif()

  set(embed_bitcode_option)
  if (SWIFTFILE_EMBED_BITCODE)
    set(embed_bitcode_option "-embed-bitcode")
  endif()

  set(main_command "-c")
  if (SWIFT_CHECK_INCREMENTAL_COMPILATION)
    set(swift_compiler_tool "${SWIFT_SOURCE_DIR}/utils/check-incremental" "${swift_compiler_tool}")
  endif()

  if (SWIFT_REPORT_STATISTICS)
    list(GET obj_dirs 0 first_obj_dir)
    list(APPEND swift_flags "-stats-output-dir" ${first_obj_dir})
  endif()

  set(standard_outputs ${SWIFTFILE_OUTPUT})
  set(apinotes_outputs ${apinote_files})
  set(module_outputs "${module_file}" "${module_doc_file}" "${interface_file}")
  set(sib_outputs "${sib_file}")
  set(sibopt_outputs "${sibopt_file}")
  set(sibgen_outputs "${sibgen_file}")

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
    set(command_touch_standard_outputs
      COMMAND "${CMAKE_COMMAND}" -E touch ${standard_outputs})
    set(command_touch_apinotes_outputs
      COMMAND "${CMAKE_COMMAND}" -E touch ${apinotes_outputs})
    set(command_touch_module_outputs
      COMMAND "${CMAKE_COMMAND}" -E touch ${module_outputs})
    set(command_touch_sib_outputs
      COMMAND "${CMAKE_COMMAND}" -E touch ${sib_outputs})
    set(command_touch_sibopt_outputs
      COMMAND "${CMAKE_COMMAND}" -E touch ${sibopt_outputs})
    set(command_touch_sibgen_outputs
      COMMAND "${CMAKE_COMMAND}" -E touch ${sibgen_outputs})
  endif()

  # First generate the obj dirs
  add_custom_command_target(
      obj_dirs_dependency_target
      ${command_create_dirs}
      COMMAND ""
      OUTPUT ${obj_dirs}
      COMMENT "Generating obj dirs for ${first_output}")

  # Generate the api notes if we need them.
  if (apinotes_outputs)
    add_custom_command_target(
        api_notes_dependency_target
        # Create API notes before compiling, because this will affect the APIs
        # the overlay sees.
        ${command_create_apinotes}
        ${command_touch_apinotes_outputs}
        COMMAND ""
        OUTPUT ${apinotes_outputs}
        DEPENDS
          ${depends_create_apinotes}
          ${obj_dirs_dependency_target}
        COMMENT "Copying API notes for ${first_output}")
  endif()

  # Then we can compile both the object files and the swiftmodule files
  # in parallel in this target for the object file, and ...

  # Windows doesn't support long command line paths, of 8191 chars or over. We
  # need to work around this by avoiding long command line arguments. This can
  # be achieved by writing the list of file paths to a file, then reading that
  # list in the Python script.
  string(RANDOM file_name)
  set(file_path "${CMAKE_CURRENT_BINARY_DIR}/${file_name}.txt")
  string(REPLACE ";" "'\n'" source_files_quoted "${source_files}")
  file(WRITE "${file_path}" "'${source_files_quoted}'")
  
  add_custom_command_target(
      dependency_target
      COMMAND
        "${PYTHON_EXECUTABLE}" "${line_directive_tool}" "@${file_path}" --
        "${swift_compiler_tool}" "${main_command}" ${swift_flags}
        ${output_option} ${embed_bitcode_option} "@${file_path}"
      ${command_touch_standard_outputs}
      OUTPUT ${standard_outputs}
      DEPENDS
        ${swift_compiler_tool_dep}
        ${file_path} ${source_files} ${SWIFTFILE_DEPENDS}
        ${swift_ide_test_dependency} ${api_notes_dependency_target}
        ${obj_dirs_dependency_target}
      COMMENT "Compiling ${first_output}")
  set("${dependency_target_out_var_name}" "${dependency_target}" PARENT_SCOPE)

  # This is the target to generate:
  #
  # 1. *.swiftmodule
  # 2. *.swiftdoc
  # 3. *.swiftinterface
  # 4. *.Onone.sib
  # 5. *.O.sib
  # 6. *.sibgen
  #
  # Only 1,2,3 are built by default. 4,5,6 are utility targets for use by
  # engineers and thus even though the targets are generated, the targets are
  # not built by default.
  #
  # We only build these when we are not producing a main file. We could do this
  # with sib/sibgen, but it is useful for looking at the stdlib.
  if (NOT SWIFTFILE_IS_MAIN)
    add_custom_command_target(
        module_dependency_target
        COMMAND
          "${CMAKE_COMMAND}" "-E" "remove" "-f" "${module_file}"
        COMMAND
          "${CMAKE_COMMAND}" "-E" "remove" "-f" "${module_doc_file}"
        COMMAND
          "${CMAKE_COMMAND}" "-E" "remove" "-f" "${interface_file}"
        COMMAND
          "${PYTHON_EXECUTABLE}" "${line_directive_tool}" "@${file_path}" --
          "${swift_compiler_tool}" "-emit-module" "-o" "${module_file}"
          "-emit-parseable-module-interface" ${swift_flags} "@${file_path}"
        ${command_touch_module_outputs}
        OUTPUT ${module_outputs}
        DEPENDS
          ${swift_compiler_tool_dep}
          ${source_files} ${SWIFTFILE_DEPENDS}
          ${swift_ide_test_dependency} ${api_notes_dependency_target}
          ${obj_dirs_dependency_target}
        COMMENT "Generating ${module_file}")
    set("${dependency_module_target_out_var_name}" "${module_dependency_target}" PARENT_SCOPE)

    # This is the target to generate the .sib files. It is not built by default.
    add_custom_command_target(
        sib_dependency_target
        COMMAND
          "${PYTHON_EXECUTABLE}" "${line_directive_tool}" "@${file_path}" --
          "${swift_compiler_tool}" "-emit-sib" "-o" "${sib_file}" ${swift_flags} -Onone
          "@${file_path}"
        ${command_touch_sib_outputs}
        OUTPUT ${sib_outputs}
        DEPENDS
          ${swift_compiler_tool_dep}
          ${source_files} ${SWIFTFILE_DEPENDS}
          ${obj_dirs_dependency_target}
        COMMENT "Generating ${sib_file}"
        EXCLUDE_FROM_ALL)
    set("${dependency_sib_target_out_var_name}" "${sib_dependency_target}" PARENT_SCOPE)

    add_custom_command_target(
        sibopt_dependency_target
        COMMAND
          "${PYTHON_EXECUTABLE}" "${line_directive_tool}" "@${file_path}" --
          "${swift_compiler_tool}" "-emit-sib" "-o" "${sibopt_file}" ${swift_flags} -O
          "@${file_path}"
        ${command_touch_sibopt_outputs}
        OUTPUT ${sibopt_outputs}
        DEPENDS
          ${swift_compiler_tool_dep}
          ${source_files} ${SWIFTFILE_DEPENDS}
          ${obj_dirs_dependency_target}
        COMMENT "Generating ${sibopt_file}"
        EXCLUDE_FROM_ALL)
    set("${dependency_sibopt_target_out_var_name}" "${sibopt_dependency_target}" PARENT_SCOPE)

    # This is the target to generate the .sibgen files. It is not built by default.
    add_custom_command_target(
        sibgen_dependency_target
        COMMAND
          "${PYTHON_EXECUTABLE}" "${line_directive_tool}" "@${file_path}" --
          "${swift_compiler_tool}" "-emit-sibgen" "-o" "${sibgen_file}" ${swift_flags}
          "@${file_path}"
        ${command_touch_sibgen_outputs}
        OUTPUT ${sibgen_outputs}
        DEPENDS
          ${swift_compiler_tool_dep}
          ${source_files} ${SWIFTFILE_DEPENDS}
          ${obj_dirs_dependency_target}
          COMMENT "Generating ${sibgen_file}"
          EXCLUDE_FROM_ALL)
    set("${dependency_sibgen_target_out_var_name}" "${sibgen_dependency_target}" PARENT_SCOPE)
  endif()


  # Make sure the build system knows the file is a generated object file.
  set_source_files_properties(${SWIFTFILE_OUTPUT}
      PROPERTIES
      GENERATED true
      EXTERNAL_OBJECT true
      LANGUAGE C
      OBJECT_DEPENDS "${source_files}")
endfunction()

