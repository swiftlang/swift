function(set_if_arch_bitness var_name)
  cmake_parse_arguments(
      SIA # prefix
      "" # options
      "ARCH;CASE_32_BIT;CASE_64_BIT" # single-value args
      "" # multi-value args
      ${ARGN})

  if("${SIA_ARCH}" STREQUAL "i386" OR
     "${SIA_ARCH}" STREQUAL "armv7")
    set("${var_name}" "${SIA_CASE_32_BIT}" PARENT_SCOPE)
  elseif("${SIA_ARCH}" STREQUAL "x86_64" OR
         "${SIA_ARCH}" STREQUAL "arm64")
    set("${var_name}" "${SIA_CASE_64_BIT}" PARENT_SCOPE)
  else()
    message(FATAL_ERROR "Unknown architecture: ${SIA_ARCH}")
  endif()
endfunction()

