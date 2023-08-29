set(SWIFT_SET_RPATH_SCRIPT_FILE "${CMAKE_CURRENT_LIST_FILE}")

function(swift_get_set_rpath_script_file out_var)
  set(${out_var} "${SWIFT_SET_RPATH_SCRIPT_FILE}" PARENT_SCOPE)
endfunction()

# Actual RPATH_CHANGE operation to the file.
function(_swift_set_rpath_impl file new_rpath)
  # FIXME: Handle non-ELF files. We can't use RPATH_SET because it's only available CMake 3.21.0
  execute_process(
    COMMAND readelf -Wd "${file}"
    COMMAND grep -Po "R(UN)?PATH.*\\[\\K[^\\]]*"
    OUTPUT_VARIABLE current_rpath
  )
  string(STRIP "${current_rpath}" current_rpath)

  # NOTE: RPATH_CHANGE is not documented, and works only for ELF and XCOFF.
  file(RPATH_CHANGE FILE "${file}" OLD_RPATH "${current_rpath}" NEW_RPATH "${new_rpath}")
endfunction()

# For 'cmake -P <scirpt>'.
if (SWIFT_SET_RPATH_FILE AND SWIFT_SET_RPATH_NEW_RPATH)
  _swift_set_rpath_impl("${SWIFT_SET_RPATH_FILE}" "${SWIFT_SET_RPATH_NEW_RPATH}")
endif()
