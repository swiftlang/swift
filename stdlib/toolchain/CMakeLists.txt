# Toolchain-only build products

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../cmake/modules)
include(AddSwiftStdlib)

set(CXX_COMPILE_FLAGS)
set(CXX_LINK_FLAGS)


set(compile_flags
  # Build the runtime with -Wall to catch, e.g., uninitialized variables
  # warnings.
  "-Wall"

  # C++ code in the runtime and standard library should generally avoid
  # introducing static constructors or destructors.
  "-Wglobal-constructors"
  "-Wexit-time-destructors")


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

# Compatibility libraries build in a special alternate universe that can't
# directly link to most OS runtime libraries, and have to access the
# runtime being patched only through public ABI.
list(APPEND CXX_COMPILE_FLAGS "-DSWIFT_COMPATIBILITY_LIBRARY=1")

set(COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_OSX "10.9")
set(COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_IOS "7.0")
set(COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_TVOS "9.0")
set(COMPATIBILITY_MINIMUM_DEPLOYMENT_VERSION_WATCHOS "2.0")

add_subdirectory(legacy_layouts)
add_subdirectory(Compatibility50)
add_subdirectory(Compatibility51)
add_subdirectory(CompatibilityDynamicReplacements)
