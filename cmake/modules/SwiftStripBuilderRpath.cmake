set(SWIFT_SET_RPATH_SCRIPT_FILE "${CMAKE_CURRENT_LIST_FILE}")

function(swift_get_set_rpath_script_file out_var)
  set(${out_var} "${SWIFT_SET_RPATH_SCRIPT_FILE}" PARENT_SCOPE)
endfunction()

# Actual RPATH_SET operation to the file.
function(_swift_set_rpath_impl file new_rpath)
  # NOTE: RPATH_SET is not documented, and works only for ELF and XCOFF. 
  file(RPATH_SET FILE "${file}" NEW_RPATH "${new_rpath}")
endfunction()

# For 'install(SCRIPT <script> CODE swift_set_rpath(...)'.
function(_swift_file_set_rpath_installed file new_rpath)
  set(DESTDIR $ENV{DESTDIR})
  if(NOT IS_ABSOLUTE "${file}")
    string(PREPEND file "${CMAKE_INSTALL_PREFIX}/")
  endif()
  string(PREPEND file "${DESTDIR}")
  _swift_set_rpath_impl("${file}" "${new_rpath}")
endfunction()

# Add 'install' script that set runtime path to the specified file.
function(swift_install_file_set_rpath file install_rpath component)
  if(NOT(SWIFT_HOST_VARIANT_SDK STREQUAL "LINUX"))
      return()
  endif()
  if((NOT SWIFT_SWIFT_PARSER) AND NOT(BOOTSTRAPPING_MODE STREQUAL "HOSTTOOLS"))
      return()
  endif()

  swift_get_set_rpath_script_file(script)

  swift_install_in_component(
    SCRIPT "${script}" 
    CODE "_swift_file_set_rpath_installed(\"${file}\" \"${install_rpath}\")"
    COMPONENT ${component}
  )
endfunction()

# Add 'install' script that set runtime path to the target but only startinig with '$ORIGIN'
function(swift_install_strip_builder_rpath)
  cmake_parse_arguments(RPATH
                        ""
                        "DESTINATION;COMPONENT"
                        "TARGETS"
                        ${ARGN})

  foreach(target ${RPATH_TARGETS})
    # Filter out RPATHs *not* starting with $ORIGIN
    set(stripped_rpaths)
    get_target_property(install_rpaths ${target} INSTALL_RPATH)
    foreach(path ${install_rpaths})
        if (path MATCHES "^\\$ORIGIN")
        list(APPEND stripped_rpaths "${path}")
        endif()
    endforeach()

    list(JOIN stripped_rpaths ":" install_rpath_str)
    swift_install_file_set_rpath(
      "${RPATH_DESTINATION}/$<TARGET_FILE_NAME:${target}>"
      "${install_rpath_str}"
      ${RPATH_COMPONENT}
    )
  endforeach()
endfunction()

# For 'cmake -P <scirpt>'.
if (SWIFT_SET_RPATH_FILE AND SWIFT_SET_RPATH_NEW_RPATH)
  _swift_set_rpath_impl("${SWIFT_SET_RPATH_FILE}" "${SWIFT_SET_RPATH_NEW_RPATH}")
endif()
