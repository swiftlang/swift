function(swift_get_configuration_types result_var_name)
  if(CMAKE_CONFIGURATION_TYPES)
    set("${result_var_name}" "${CMAKE_CONFIGURATION_TYPES}" PARENT_SCOPE)
  else()
    set("${result_var_name}" "." PARENT_SCOPE)
  endif()
endfunction()

