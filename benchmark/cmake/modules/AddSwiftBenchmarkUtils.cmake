
include(CMakeParseArguments)

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
