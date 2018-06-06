# Module import guard
if(DEFINED SWIFT_MANPAGE_MODULE_LOADED)
  return()
endif()
set(SWIFT_MANPAGE_MOUDLE_LOADED TRUE)


include(CMakeParseArguments)


find_program(POD2MAN pod2man)


# Create a target to create a man page from a pod file.
#
# manpage(
#     SOURCE foobar.pod
#     PAGE_HEADER "text"
#     MAN_FILE_BASENAME foobar
#     MAN_SECTION N
#     INSTALL_IN_COMPONENT comp
#     )
function(manpage)
  set(options)
  set(single_value_args
    INSTALL_IN_COMPONENT
    MAN_FILE_BASENAME
    MAN_SECTION
    PAGE_HEADER
    SOURCE)
  set(multi_value_args)

  cmake_parse_arguments(MP
    "${options}"
    "${single_value_args}"
    "${mutli_value_args}"
    ${ARGN})

  if(NOT POD2MAN)
    message(FATAL_ERROR "Need pod2man installed to generate man page")
  endif()

  set(output_file_name
    "${CMAKE_CURRENT_BINARY_DIR}/${MP_MAN_FILE_BASENAME}.${MP_MAN_SECTION}")

  add_custom_command_target(unused_var
    COMMAND
      "${POD2MAN}"
      "--section" "${MP_MAN_SECTION}"
      "--center" "${MP_PAGE_HEADER}"
      "--release=\"swift ${SWIFT_VERSION}\""
      "--name" "${MP_MAN_FILE_BASENAME}"
      "--stderr"
      "${MP_SOURCE}" > "${output_file_name}"
    OUTPUT "${output_file_name}"
    DEPENDS "${MP_SOURCE}"
    ALL)

  swift_install_in_component("${MP_INSTALL_IN_COMPONENT}"
    FILES "${output_file_name}"
    DESTINATION "share/man/man${MP_MAN_SECTION}")
endfunction()

