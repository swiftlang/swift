function(swift_is_installing_component component result_var_name)
  precondition(component MESSAGE "Component name is required")

  if("${component}" STREQUAL "never_install")
    set("${result_var_name}" FALSE PARENT_SCOPE)
  else()
    list(FIND _SWIFT_KNOWN_INSTALL_COMPONENTS "${component}" index)
    if(${index} EQUAL -1)
      message(FATAL_ERROR "unknown install component: ${component}")
    endif()

    string(TOUPPER "${component}" var_name_piece)
    string(REPLACE "-" "_" var_name_piece "${var_name_piece}")
    set("${result_var_name}" "${SWIFT_INSTALL_${var_name_piece}}" PARENT_SCOPE)
  endif()
endfunction()

# swift_install_in_component(<COMPONENT NAME>
#   <same parameters as install()>)
#
# Executes the specified installation actions if the named component is
# requested to be installed.
#
# This function accepts the same parameters as install().
function(swift_install_in_component component)
  precondition(component MESSAGE "Component name is required")

  swift_is_installing_component("${component}" is_installing)
  if(is_installing)
    install(${ARGN})
  endif()
endfunction()

macro(swift_configure_install_components install_components)
  foreach(component ${_SWIFT_KNOWN_INSTALL_COMPONENTS})
    string(TOUPPER "${component}" var_name_piece)
    string(REPLACE "-" "_" var_name_piece "${var_name_piece}")
    set(SWIFT_INSTALL_${var_name_piece} FALSE)
  endforeach()

  foreach(component ${install_components})
    list(FIND _SWIFT_KNOWN_INSTALL_COMPONENTS "${component}" index)
    if(${index} EQUAL -1)
      message(FATAL_ERROR "unknown install component: ${component}")
    endif()

    string(TOUPPER "${component}" var_name_piece)
    string(REPLACE "-" "_" var_name_piece "${var_name_piece}")
    set(SWIFT_INSTALL_${var_name_piece} TRUE)
  endforeach()
endmacro()

