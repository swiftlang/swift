# Toolchain-only build products

set(CXX_COMPILE_FLAGS)
set(CXX_LINK_FLAGS)


set(compile_flags
  # Build the runtime with -Wall to catch, e.g., uninitialized variables
  # warnings.
  "-Wall"

  # C++ code in the runtime and standard library should generally avoid
  # introducing static constructors or destructors.
  "-Wglobal-constructors"
  "-Wexit-time-destructors"

  # We don't want runtime C++ code to export symbols we didn't explicitly
  # choose to.
  "-fvisibility=hidden")


# Build the runtime with -Wall to catch, e.g., uninitialized variables
# warnings.
if(SWIFT_COMPILER_IS_MSVC_LIKE)
  list(APPEND compile_flags "/W3")
else()
  list(APPEND compile_flags "-Wall")
endif()


foreach(flag ${compile_flags})
  check_cxx_compiler_flag("${flag}" is_supported)
  if(is_supported)
    list(APPEND CXX_COMPILE_FLAGS "${flag}")
  endif()
endforeach()
unset(compile_flags)


if("Thread" IN_LIST SWIFT_RUNTIME_USE_SANITIZERS)
  list(APPEND CXX_LINK_FLAGS "-fsanitize=thread")
endif()


add_subdirectory(legacy_layouts)
add_subdirectory(Compatibility50)
