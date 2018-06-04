# Module import guard
if(DEFINED SWIFT_UTILS_MODULE_LOADED)
  return()
endif()
set(SWIFT_UTILS_MODULE_LOADED TRUE)


include(CMakeParseArguments)


function(precondition var)
  set(options NEGATE)
  set(single_value_args MESSAGE)
  set(multi_value_args)

  cmake_parse_arguments(PRE
    "${options}"
    "${single_value_args}"
    "${multi_value_args}"
    ${ARGN})

  if(PRE_NEGATE)
    if(${var})
      if(PRE_MESSAGE)
        message(FATAL_ERROR "Error! ${PRE_MESSAGE}")
      else()
        message(FATAL_ERROR
          "Error! Variable ${var} is true or not empty. "
          "The value of ${var} is ${${var}}.")
      endif()
    endif()
  else()
    if(NOT ${var})
      if(PRE_MESSAGE)
        message(FATAL_ERROR "Error! ${PRE_MESSAGE}")
      else()
        message(FATAL_ERROR
          "Error! Variable ${var} is false, empty or not set.")
      endif()
    endif()
  endif()
endfunction()


# Assert is 'NOT ${LHS} ${OP} ${RHS}' is true.
function(precondition_binary_op OP LHS RHS)
  set(options NEGATE)
  set(single_value_args MESSAGE)
  set(multi_value_args)

  cmake_parse_arguments(PRE_BIN_OP
    "${options}"
    "${single_value_args}"
    "${multi_value_args}"
    ${ARGN})

  if(PRE_BIN_OP_NEGATE)
    if(${LHS} ${OP} ${RHS})
      if(PRE_BIN_OP_MESSAGE)
        message(FATAL_ERROR "Error! ${PRE_BIN_OP_MESSAGE}")
      else()
        message(FATAL_ERROR "Error! ${LHS} ${OP} ${RHS} is true!")
      endif()
    endif()
  else()
    if(NOT ${LHS} ${OP} ${RHS})
      if(PRE_BIN_OP_MESSAGE)
        message(FATAL_ERROR "Error! ${PRE_BIN_OP_MESSAGE}")
      else()
        message(FATAL_ERROR "Error! ${LHS} ${OP} ${RHS} is false!")
      endif()
    endif()
  endif()
endfunction()


# Translate a yes/no variable to the presence of a given string in a
# variable.
#
# Usage:
#   translate_flag(is_set flag_name result_var)
#
# If is_set is true, sets ${result_var} to ${flag_name}. Otherwise,
# unsets ${result_var}.
function(translate_flag is_set flag_name result_var)
  if(${is_set})
    set(${result_var} "${flag_name}" PARENT_SCOPE)
  else()
    set(${result_var} "" PARENT_SCOPE)
  endif()
endfunction()


macro(translate_flags prefix options)
  foreach(var ${options})
    translate_flag("${${prefix}_${var}}" "${var}" "${prefix}_${var}_keyword")
  endforeach()
endmacro()


# Set ${outvar} to ${${invar}}, asserting if ${invar} is not set.
function(precondition_translate_flag invar outvar)
  precondition(${invar})
  set(${outvar} "${${invar}}" PARENT_SCOPE)
endfunction()


function(is_build_type_optimized build_type result_var)
  if("${build_type}" STREQUAL "Debug")
    set(${result_var} FALSE PARENT_SCOPE)
  elseif("${build_type}" STREQUAL "RelWithDebInfo"
      OR "${build_type}" STREQUAL "Release"
      OR "${build_type}" STREQUAL "MinSizeRel")
    set(${result_var} TRUE PARENT_SCOPE)
  else()
    message(FATAL_ERROR "Unknown build type: ${build_type}")
  endif()
endfunction()


function(is_build_type_with_debuginfo build_type result_var)
  if("${build_type}" STREQUAL "Debug"
      OR "${build_type}" STREQUAL "RelWithDebInfo")
    set(${result_var} TRUE PARENT_SCOPE)
  elseif("${build_type}" STREQUAL "Release"
      OR "${build_type}" STREQUAL "MinSizeRel")
    set(${result_var} FALSE PARENT_SCOPE)
  else()
    message(FATAL_ERROR "Unknown build type: ${build_type}")
  endif()
endfunction()


# Set variable to value if value is not null or false. Otherwise set variable to
# default_value.
function(set_with_default var value)
  set(options)
  set(single_value_args DEFAULT)
  set(multi_value_args)

  cmake_parse_argument(SWD
    "${options}"
    "${single_value_args}"
    "${multi_value_args}"
    ${ARGN})

  precondition(SWD_DEFAULT
    MESSAGE "Must specify a default argument")

  if("${value}")
    set(${var} "${value}" PARENT_SCOPE)
  else()
    set(${var} "${SWD_DEFAULT}" PARENT_SCOPE)
  endif()
endfunction()


function(swift_create_post_build_symlink target)
  set(options
    IS_DIRECTORY)
  set(single_value_args
    COMMENT
    DESTINATION
    SOURCE
    WORKING_DIRECTORY)
  set(multi_value_args)

  cmake_parse_arguments(CS
    "${options}"
    "${single_value_args}"
    "${multi_value_args}"
    ${ARGN})

  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    if(CS_IS_DIRECTORY)
      set(cmake_symlink_option "copy_directory")
    else()
      set(cmake_symlink_option "copy_if_different")
    endif()
  else()
    set(cmake_symlink_option "create_symlink")
  endif()

  add_custom_command(TARGET "${target}" POST_BUILD
    COMMAND
      "${CMAKE_COMMAND}"
      "-E" "${cmake_symlink_option}"
      "${CS_SOURCE}"
      "${CS_DESTINATION}"
    WORKING_DIRECTORY "${CS_WORKING_DIRECTORY}"
    COMMENT "${CS_COMMENT}")
endfunction()


function(dump_swift_vars)
  set(SWIFT_STDLIB_GLOBAL_CMAKE_CACHE)

  get_cmake_property(variable_names VARIABLES)
  foreach(variable_name ${variable_names})
    if("${variable_name}" MATCHES "^SWIFT")
      set(set_string "set(${variable_name} \"${${variable_name}}\")")

      # FIXME: Not sure if this was originally meant to export the variables
      # to the parent scope or not. This variable isn't used anywhere else
      # otherwise.
      string(CONCAT SWIFT_STDLIB_GLOBAL_CMAKE_CACHE
        "${SWIFT_STDLIB_GLOBAL_CMAKE_CACHE}"
        "${set_string}\n")

      message("${set_string}")
    endif()
  endforeach()
endfunction()


function(is_sdk_requested name result_var)
  if(SWIFT_HOST_VARIANT_SDK STREQUAL "${name}")
    set(${result_var} TRUE PARENT_SCOPE)
  else()
    if("${name}" IN_LIST SWIFT_SDKS)
      set(${result_var} TRUE PARENT_SCOPE)
    else()
      set(${result_var} FALSE PARENT_SCOPE)
    endif()
  endif()
endfunction()
