function(select_swift_overlay_dependenies result_var_name)
  set(options)
  set(single_parameter_options
        SDK)
  set(multiple_parameter_options
        SWIFT_MODULE_DEPENDS_IOS
        SWIFT_MODULE_DEPENDS_OSX
        SWIFT_MODULE_DEPENDS_TVOS
        SWIFT_MODULE_DEPENDS_WATCHOS)

  cmake_parse_arguments(SOD
                        "${options}"
                        "${single_parameter_options}"
                        "${multiple_parameter_options}"
                        ${ARGN})
  set(module_depends)
  if(${SOD_SDK} STREQUAL OSX)
    list(APPEND module_depends ${SOD_SWIFT_MODULE_DEPENDS_OSX})
  elseif(${SOD_SDK} STREQUAL IOS OR ${SOD_SDK} STREQUAL IOS_SIMULATOR)
    list(APPEND module_depends ${SOD_SWIFT_MODULE_DEPENDS_IOS})
  elseif(${SOD_SDK} STREQUAL TVOS OR ${SOD_SDK} STREQUAL TVOS_SIMULATOR)
    list(APPEND module_depends ${SOD_SWIFT_MODULE_DEPENDS_TVOS})
  elseif(${SOD_SDK} STREQUAL WATCHOS OR ${SOD_SDK} STREQUAL WATCHOS_SIMULATOR)
    list(APPEND module_depends ${SOD_SWIFT_MODULE_DEPENDS_WATCHOS})
  endif()
  set("${result_var_name}" "${module_depends}" PARENT_SCOPE)
endfunction()
