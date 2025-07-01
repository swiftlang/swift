# This file is designed to setup reasonable defaults for the various settings so
# that configuring a build for a given platform is likely to build
# out-of-the-box without customization. This does not mean that it is the only
# way that will work, or that it represents a shipping configuration.
# User-specified configurations should be done through cache files or by setting
# the variable with `-DSwiftCore_*` on the commandline.

set(SwiftCore_ENABLE_BACKTRACING_default OFF) # TODO: enable this by default

set(SwiftCore_ENABLE_STDIN_default ON)
set(SwiftCore_ENABLE_TYPE_PRINTING_default ON)

set(SwiftCore_ENABLE_STRICT_AVAILABILITY_default OFF)

set(SwiftCore_BACKTRACER_PATH_default "")
set(SwiftCore_VERIFY_BACKTRACER_PATH_default OFF)

# Provide a boolean option that a user can optionally enable.
# Variables are defaulted based on the value of `<variable>_default`.
# If no such default variable exists, the option is defaults to `OFF`.
macro(defaulted_option variable helptext)
  if(NOT DEFINED ${variable}_default)
    set(${variable}_default OFF)
  endif()
  option(${variable} "${helptext}" ${${variable}_default})
endmacro()

# Create a defaulted cache entry
# Entries are defaulted on the value of `<variable>_default`.
# If no such default variable exists, the variable is not created.
macro(defaulted_set variable type helptext)
  if(DEFINED ${variable}_default)
    set(${variable} ${${variable}_default} CACHE ${type} ${helptext})
  endif()
endmacro()

if(APPLE)
  set(SwiftCore_ENABLE_LIBRARY_EVOLUTION_default ON)
  set(SwiftCore_ENABLE_CRASH_REPORTER_CLIENT_default ON)
  set(SwiftCore_ENABLE_OBJC_INTEROP_default ON)
  set(SwiftCore_ENABLE_REFLECTION_default ON)
  set(SwiftCore_ENABLE_FATALERROR_BACKTRACE_default ON)
  set(SwiftCore_ENABLE_RUNTIME_OS_VERSIONING_default ON)
  set(SwiftCore_ENABLE_OVERRIDABLE_RETAIN_RELEASE_default ON)
  set(SwiftCore_ENABLE_CONCURRENCY_default NO)
  set(SwiftCore_THREADING_PACKAGE_default "DARWIN")
  set(SwiftCore_ENABLE_PRESPECIALIZATION_default ON)
  set(SwiftCore_CONCURRENCY_GLOBAL_EXECUTOR_default "dispatch")
elseif(CMAKE_SYSTEM_NAME STREQUAL "WASM")
  set(SwiftCore_OBJECT_FORMAT_default "elf")
  set(SwiftCore_THREADING_PACKAGE_default "NONE")
  set(SwiftCore_ENABLE_CONCURRENCY_default NO)
  set(SwiftCore_CONCURRENCY_GLOBAL_EXECUTOR_default "none")
elseif(LINUX OR ANDROID OR BSD)
  set(SwiftCore_OBJECT_FORMAT_default "elf")

  set(SwiftCore_ENABLE_REFLECTION_default ON)
  set(SwiftCore_ENABLE_FATALERROR_BACKTRACE_default ON)
  if(LINUX)
    set(SwiftCore_THREADING_PACKAGE_default "LINUX")
    set(SwiftCore_ENABLE_PRESPECIALIZATION_default ON)
  else()
    set(SwiftCore_THREADING_PACKAGE_default "PTHREADS")
  endif()
  set(SwiftCore_ENABLE_CONCURRENCY_default NO)
  set(SwiftCore_CONCURRENCY_GLOBAL_EXECUTOR_default "dispatch")
elseif(WIN32)
  set(SwiftCore_OBJECT_FORMAT_default "coff")

  set(SwiftCore_ENABLE_LIBRARY_EVOLUTION_default ${BUILD_SHARED_LIBS})
  set(SwiftCore_ENABLE_REFLECTION_default ON)
  set(SwiftCore_ENABLE_FATALERROR_BACKTRACE_default ON)
  set(SwiftCore_ENABLE_OVERRIDABLE_RETAIN_RELEASE_default ON)
  set(SwiftCore_ENABLE_CONCURRENCY_default NO)
  set(SwiftCore_THREADING_PACKAGE_default "WIN32")
  set(SwiftCore_ENABLE_PRESPECIALIZATION_default ON)
  set(SwiftCore_CONCURRENCY_GLOBAL_EXECUTOR_default "dispatch")

  set(SwiftCore_ENABLE_VECTOR_TYPES_default ON)
  set(SwiftCore_ENABLE_FILESYSTEM_SUPPORT_default ON)
  set(SwiftCore_INSTALL_NESTED_SUBDIR_default ON)
endif()

include("${SwiftCore_VENDOR_MODULE_DIR}/DefaultSettings.cmake" OPTIONAL)
