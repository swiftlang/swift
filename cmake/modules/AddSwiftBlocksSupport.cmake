# Adds blocks support in a cross platform manner.
function(swift_add_blocks_support target_name)
  if(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set(c_compile_flags)
    # TODO(compnerd) this should really use target_compile_options but the use
    # of keyword and non-keyword flags prevents this
    if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
      list(APPEND c_compile_flags -fblocks)
    elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
      if(SWIFT_COMPILER_IS_MSVC_LIKE)
        list(APPEND c_compile_flags -Xclang;-fblocks)
      else()
        list(APPEND c_compile_flags -fblocks)
      endif()
    endif()
    set_property(TARGET ${target_name} APPEND_STRING PROPERTY
        COMPILE_FLAGS " ${c_compile_flags}")
  endif()
endfunction()
