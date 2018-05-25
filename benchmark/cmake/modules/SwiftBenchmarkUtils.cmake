
include(CMakeParseArguments)

function(precondition var)
  cmake_parse_arguments(
    PRECONDITION # prefix
    "NEGATE" # options
    "MESSAGE" # single-value args
    "" # multi-value args
    ${ARGN})

  if (PRECONDITION_NEGATE)
    if (${var})
      if (PRECONDITION_MESSAGE)
        message(FATAL_ERROR "Error! ${PRECONDITION_MESSAGE}")
      else()
        message(FATAL_ERROR "Error! Variable ${var} is true or not empty. The value of ${var} is ${${var}}.")
      endif()
    endif()
  else()
    if (NOT ${var})
      if (PRECONDITION_MESSAGE)
        message(FATAL_ERROR "Error! ${PRECONDITION_MESSAGE}")
      else()
        message(FATAL_ERROR "Error! Variable ${var} is false, empty or not set.")
      endif()
    endif()
  endif()
endfunction()

# Translate a yes/no variable to the presence of a given string in a
# variable.
#
# Usage:
#   translate_flag(is_set flag_name var_name)
#
# If is_set is true, sets ${var_name} to ${flag_name}. Otherwise,
# unsets ${var_name}.
function(translate_flag is_set flag_name var_name)
  if(${is_set})
    set("${var_name}" "${flag_name}" PARENT_SCOPE)
  else()
    set("${var_name}" "" PARENT_SCOPE)
  endif()
endfunction()


# Run a shell command and assign output to a variable or fail with an error.
# Example usage:
#   runcmd(COMMAND "xcode-select" "-p"
#          VARIABLE xcodepath
#          ERROR "Unable to find current Xcode path")
function(runcmd)
  cmake_parse_arguments(RUNCMD "" "VARIABLE;ERROR" "COMMAND" ${ARGN})
  execute_process(
      COMMAND ${RUNCMD_COMMAND}
      OUTPUT_VARIABLE ${RUNCMD_VARIABLE}
      RESULT_VARIABLE result
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT "${result}" MATCHES "0")
    message(FATAL_ERROR "${RUNCMD_ERROR}")
  endif()
  set(${RUNCMD_VARIABLE} ${${RUNCMD_VARIABLE}} PARENT_SCOPE)
endfunction(runcmd)
