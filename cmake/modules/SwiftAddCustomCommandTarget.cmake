include(CMakeParseArguments)
include(SwiftUtils)

# Populate the variable 'args' in the parent scope with a keyword
# argument list.  We read the variables options, ${k}_keyword, and
# ACCT_${k} from the parent scope, for each ${k} in the list of
# keyword names other than COMMAND accepted by
# add_custom_command_target.
#
#   ${k}_keyword must expand to ${k} if ${k} was passed to
#   add_custom_command_target, and be empty otherwise.
#
#   ACCT_${k} must expand to the list of arguments to
#   add_custom_command_target marked by ${k}, and be empty otherwise.
#
function(_make_acct_argument_list)
  set(args)
  foreach(k ${ARGN})
    if(${k} IN_LIST options)
      list(APPEND args ${${k}_keyword})
    else()
      list(APPEND args ${${k}_keyword} ${ACCT_${k}})
    endif()
  endforeach()
  set(args ${args} PARENT_SCOPE)
endfunction()

# Add a custom command/target pair.  Use this instead of
# add_custom_command because it provides proper dependency tracking
# when used with parallel builds and the 'Unix Makefiles' generator.
# See https://www.cmake.org/Bug/view.php?id=10082
#
# The CMake documentation for add_custom_command quoth,
#
#    "Do not list the output in more than one independent target that
#     may build in parallel or the two instances of the rule may
#     conflict (instead use add_custom_target to drive the command and
#     make the other targets depend on that one)."
#
# This function implements the suggested pattern.
#
# add_custom_command_target(
#   dependency_out_var_name
#
#   COMMAND command1 [ARGS] [args1...]
#   [COMMAND command2 [ARGS] [args2...] ...]
#
#   OUTPUT output1 [output2 ...]
#   [MAIN_DEPENDENCY depend]
#   [DEPENDS [depends...]]
#   [IMPLICIT_DEPENDS <lang1> depend1
#   [<lang2> depend2] ...]
#   [WORKING_DIRECTORY dir]
#   [COMMENT comment] [VERBATIM] [APPEND]
#   [ALL]
#   [SOURCES src1 [src2...]])
#
# dependency_out_var_name is the name of a variable, to be set in the
# parent scope with the name of a target that all targets using the
# OUTPUT should depend on.  For example:
#
#   add_custom_command_target(
#       TheDependency
#       COMMAND echo "int main() {}" ">"  z.c
#       OUTPUT z.c
#       VERBATIM
#       DEPENDS z.c.gyb)
#
#   add_executable(exe1 z.c)
#   add_dependencies(exe1 ${TheDependency})
#   add_executable(exe2 z.c)
#   add_dependencies(exe2 ${TheDependency})
#
# **Note1**: all COMMAND arguments must immediately follow
# dependency_out_var_name or this function will misbehave.
#
# **Note2**: any subdirectories that define targets dependent on
# OUTPUT ${o} should invoke:
#
#   set_source_files_properties(${o} PROPERTIES GENERATED true)
#
# All arguments other than ALL, SOURCES, and dependency_out_var_name
# are forwarded to add_custom_command; arguments ALL, SOURCES, and
# WORKING_DIRECTORY are forwarded to add_custom_target.  See the
# documentation of those functions for a description of all arguments.
#
# How This Function Works
#
# CMake offers one way to add new build rules: add_custom_command.
# Most people, however, overlook its actual semantics.
# add_custom_command does *not* create a target.  The CMake
# documentation declareth,
#
#   "A target created in the same directory (CMakeLists.txt file) that
#    specifies any output of the custom command as a source file is
#    given a rule to generate the file using the command at build
#    time."
#
# Therefore, when two targets built in parallel depend on an output of
# the same custom command, they may race to rebuild that output.
# Hilarity ensues.  You might not notice this effect depending on the
# generator you use, but it happens with 'Unix Makefiles'.
#
# By injecting a target into the dependency graph between the custom
# command output and any targets that depend on that output, we force
# the output to be built before starting on any of its dependent
# targets.
function(add_custom_command_target dependency_out_var_name)
  # Parse the arguments.  We don't look for COMMAND arguments because
  # they don't follow the pattern supported by cmake_parse_arguments.
  # As a result, they end up in ACCT_UNPARSED_ARGUMENTS and are
  # forwarded verbatim.
  set(options ALL VERBATIM APPEND IDEMPOTENT EXCLUDE_FROM_ALL)
  set(single_value_args
      MAIN_DEPENDENCY WORKING_DIRECTORY COMMENT CUSTOM_TARGET_NAME)
  set(multi_value_args OUTPUT DEPENDS IMPLICIT_DEPENDS SOURCES)
  cmake_parse_arguments(
      ACCT # prefix
      "${options}" "${single_value_args}" "${multi_value_args}" ${ARGN})
  set(ACCT_COMMANDS ${ACCT_UNPARSED_ARGUMENTS})

  if("${ACCT_CUSTOM_TARGET_NAME}" STREQUAL "")
    # CMake doesn't allow '/' characters in filenames, so replace them with '-'
    list(GET ACCT_OUTPUT 0 output_filename)
    string(REPLACE "${CMAKE_BINARY_DIR}/" "" target_name "${output_filename}")
    string(REPLACE "${CMAKE_SOURCE_DIR}/" "" target_name "${target_name}")
    string(REPLACE "${CMAKE_CFG_INTDIR}/" "" target_name "${target_name}")
    string(REPLACE "/" "-" target_name "${target_name}")
  else()
    set(target_name "${ACCT_CUSTOM_TARGET_NAME}")
  endif()

  if((NOT ACCT_IDEMPOTENT) OR (ACCT_IDEMPOTENT AND NOT TARGET "${target_name}"))
    # For each keyword argument k that was passed to this function, set
    # ${k}_keyword to ${k}.  That will allow us to use the incantation
    # '${${k}_keyword} ${ACCT_${k}}' to forward the arguments on.
    foreach(var ${options} ${single_value_args} ${multi_value_args})
      translate_flag(ACCT_${var} ${var} ${var}_keyword)
    endforeach()

    _make_acct_argument_list(
      OUTPUT MAIN_DEPENDENCY DEPENDS
      IMPLICIT_DEPENDS WORKING_DIRECTORY COMMENT VERBATIM APPEND)
    add_custom_command(${ACCT_COMMANDS} ${args})

    _make_acct_argument_list(ALL WORKING_DIRECTORY SOURCES)
    add_custom_target(
        "${target_name}" ${args}
        DEPENDS ${ACCT_OUTPUT}
        COMMENT "${ACCT_OUTPUT}")
    set_target_properties(
        "${target_name}" PROPERTIES
        FOLDER "add_custom_command_target artifacts")
    if (ACCT_EXCLUDE_FROM_ALL)
      set_target_properties(
        "${target_name}" PROPERTIES
        EXCLUDE_FROM_ALL TRUE)
    endif()
  endif()

  # "Return" the name of the custom target
  set("${dependency_out_var_name}" "${target_name}" PARENT_SCOPE)
endfunction()
