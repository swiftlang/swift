
# Try to find dtrace if we are asked to use it.
if (SWIFT_RUNTIME_ENABLE_DTRACE)
  find_program(SWIFT_DTRACE "dtrace")
  if (NOT SWIFT_DTRACE)
    message(FATAL_ERROR "DTrace was not found!")
  endif()

# Create a target to process .d files with the 'dtrace' tool.
#
# handle_dtrace_sources(
#     dependency_out_var_name
#     sources_var_name
#     dtrace_include_paths)
#
# dependency_out_var_name
#   The name of a variable, to be set in the parent scope to the list of
#   targets that invoke gyb.  Every target that depends on the generated
#   sources should depend on ${dependency_out_var_name} targets.
#
# sources_var_name
#   The name used for sources. We filter out any .d files.
#
# dtrace_include_paths
#   Paths where we have generated dtrace header files. Necessary to ensure that
#   the files can be included properly. Pass into include_directories.
function(handle_dtrace_sources dependency_out_var_name sources_var_name dtrace_include_paths_var_name)
  set(extra_dtrace_flags "")

  set(dtrace_flags
      -h
      ${extra_dtrace_flags})

  set(dtrace_include_paths)

  set(dependency_targets)
  set(new_sources)
  foreach (src ${${sources_var_name}})
    string(REGEX REPLACE "[.]d" ".h" header "${src}")
    if(src STREQUAL header)
      list(APPEND new_sources "${src}")
    else()
      set(dir "${CMAKE_CURRENT_BINARY_DIR}/dtrace")
      set(output_file_name "${dir}/${header}")
      list(APPEND dtrace_include_paths "${dir}")
      string(MD5 output_file_name_hash "${output_file_name}")
      add_custom_command_target(
        dependency_target
        COMMAND
          "${CMAKE_COMMAND}" -E make_directory "${dir}"
        COMMAND
          "${SWIFT_DTRACE}" "${dtrace_flags}" -o "${output_file_name}.tmp" -s "${src}"
        COMMAND
          "${CMAKE_COMMAND}" -E copy_if_different "${output_file_name}.tmp" "${output_file_name}"
        COMMAND
          "${CMAKE_COMMAND}" -E remove "${output_file_name}.tmp"
        OUTPUT "${output_file_name}"
        DEPENDS "${src}"
        COMMENT "Generating dtrace header ${header} from ${src}"
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        IDEMPOTENT)
        list(APPEND dependency_targets "${dependency_target}")
    endif()
  endforeach()
  set("${dependency_out_var_name}" "${dependency_targets}" PARENT_SCOPE)
  set("${sources_var_name}" "${new_sources}" PARENT_SCOPE)
  set("${dtrace_include_paths_var_name}" "${dtrace_include_paths}" PARENT_SCOPE)
endfunction()

endif()
