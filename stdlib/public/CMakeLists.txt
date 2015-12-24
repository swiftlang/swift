# C++ code in the runtime and standard library should generally avoid
# introducing static constructors or destructors.
set(SWIFT_CORE_CXX_FLAGS)

check_cxx_compiler_flag("-Werror -Wglobal-constructors" CXX_SUPPORTS_GLOBAL_CONSTRUCTORS_WARNING)
if(CXX_SUPPORTS_GLOBAL_CONSTRUCTORS_WARNING)
  list(APPEND SWIFT_CORE_CXX_FLAGS "-Wglobal-constructors")
endif()

check_cxx_compiler_flag("-Wexit-time-destructors" CXX_SUPPORTS_EXIT_TIME_DESTRUCTORS_WARNING)
if(CXX_SUPPORTS_EXIT_TIME_DESTRUCTORS_WARNING)
  list(APPEND SWIFT_CORE_CXX_FLAGS "-Wexit-time-destructors")
endif()

if(SWIFT_BUILD_STDLIB)
  # These must be kept in dependency order so that any referenced targets
  # exist at the time we look for them in add_swift_*.
  add_subdirectory(SwiftShims)
  add_subdirectory(runtime)
  add_subdirectory(stubs)
  add_subdirectory(core)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  if(SWIFT_BUILD_SDK_OVERLAY)
    add_subdirectory(SDK)
  endif()
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_subdirectory(Glibc)
endif()
