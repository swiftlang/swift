# This file is designed to setup reasonable defaults for the various settings so
# that configuring a build for a given platform is likely to build
# out-of-the-box without customization. This does not mean that it is the only
# way that will work, or that it represents a shipping configuration.
# User-specified configurations should be done through cache files or by setting
# the variable with `-DSwiftCore_*` on the commandline.

set(SwiftCore_ENABLE_BACKTRACING_default OFF) # TODO: enable this by default
set(SwiftCore_ENABLE_COMMANDLINE_SUPPORT_default OFF) # TODO: enable this by default

set(SwiftCore_ENABLE_STDIN_default ON)
set(SwiftCore_ENABLE_TYPE_PRINTING_default ON)

set(SwiftCore_BACKTRACER_PATH_default "")

# Provide a boolean option that a user can optionally enable.
# Variables are defaulted based on the value of `<variable>_default`.
# If no such default variable exists, the option is defaults to `OFF`.
macro(defaulted_option variable helptext)
  if(NOT DEFINED ${variable}_default)
    set(${variable}_default OFF)
  endif()
  option(${variable} ${helptext} ${${variable}_default})
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
  set(SwiftCore_ENABLE_RUNTIME_OS_VERSIONING ON)
  set(SwiftCore_ENABLE_OVERRIDABLE_RETAIN_RELEASE ON)
  set(SwiftCore_THREADING_PACKAGE "DARWIN")
elseif(CMAKE_SYSTEM_NAME STREQUAL "WASM")
  set(SwiftCore_OBJECT_FORMAT_default "elf")
  set(SwiftCore_THREADING_PACKAGE "NONE")
elseif(LINUX OR ANDROID OR BSD)
  set(SwiftCore_OBJECT_FORMAT_default "elf")
  if(LINUX)
    set(SwiftCore_THREADING_PACKAGE "LINUX")
  else()
    set(SwiftCore_THREADING_PACKAGE "PTHREADS")
  endif()
elseif(WIN32)
  set(SwiftCore_OBJECT_FORMAT_default "coff")
  set(SwiftCore_THREADING_PACKAGE "WIN32")
endif()

include("${SwiftCore_VENDOR_MODULE_DIR}/DefaultSettings.cmake" OPTIONAL)
