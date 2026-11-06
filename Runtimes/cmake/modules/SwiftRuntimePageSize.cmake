include_guard(GLOBAL)

# This is the maximum page size supported by the runtime being built, not the
# page size of the build host. Keep compilation and ELF linking in agreement.
function(swift_runtime_validate_page_size page_size)
  if(NOT "${page_size}" MATCHES "^([1-9][0-9]*|0[xX][0-9a-fA-F]+)$")
    message(FATAL_ERROR "Runtime page size must be a positive power of two: '${page_size}'")
  endif()
  math(EXPR remainder "${page_size} & (${page_size} - 1)")
  if(page_size LESS_EQUAL 0 OR NOT remainder EQUAL 0)
    message(FATAL_ERROR "Runtime page size must be a positive power of two: '${page_size}'")
  endif()
endfunction()

function(swift_runtime_get_page_size result target_triple page_size)
  if("${page_size}" STREQUAL "")
    if(target_triple MATCHES "-android")
      set(page_size 16384)
    elseif(target_triple MATCHES "^(aarch64|arm64)-.*-linux")
      set(page_size 65536)
    elseif(target_triple MATCHES "^arm64[^-]*-apple-")
      set(page_size 16384)
    else()
      set(page_size 4096)
    endif()
  endif()
  swift_runtime_validate_page_size("${page_size}")
  math(EXPR page_size "${page_size}")
  set(${result} "${page_size}" PARENT_SCOPE)
endfunction()

# The driver consumes these response files when linking installed static
# runtimes, without access to the CMake target's usage requirements.
function(swift_runtime_configure_link_file input output page_size)
  swift_runtime_validate_page_size("${page_size}")
  configure_file("${input}" "${output}" @ONLY NEWLINE_STYLE LF)
  file(APPEND "${output}" "\n-Wl,-z,max-page-size=${page_size}\n")
endfunction()
