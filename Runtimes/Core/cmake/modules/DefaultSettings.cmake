# This file is designed to setup reasonable defaults for the various settings so
# that configuring a build for a given platform is likely to build
# out-of-the-box without customization. This does not mean that it is the only
# way that will work, or that it represents a shipping configuration.

set(SwiftCore_ENABLE_BACKTRACING_default OFF) # TODO: enable this by default
set(SwiftCore_ENABLE_COMMANDLINE_SUPPORT_default OFF) # TODO: enable this by default

set(SwiftCore_ENABLE_TYPE_PRINTING_default ON)

set(SwiftCore_BACKTRACER_PATH_default "")

macro(defaulted_option variable helptext)
  if(NOT DEFINED ${variable}_default)
    set(${variable}_default OFF)
  endif()
  option(${variable} ${helptext} ${${variable}_default})
endmacro()

if(APPLE)
  set(SwiftCore_ENABLE_LIBRARY_EVOLUTION_default ON)
  set(SwiftCore_ENABLE_CRASH_REPORTER_CLIENT_default ON)
  set(SwiftCore_ENABLE_OBJC_INTEROP_default ON)
  set(SwiftCore_ENABLE_REFLECTION_default ON)
elseif(CMAKE_SYSTEM_NAME STREQUAL "WASM")
  set(SwiftCore_OBJECT_FORMAT_default "elf")
elseif(LINUX OR ANDROID OR BSD)
  set(SwiftCore_OBJECT_FORMAT_default "elf")
elseif(WIN32)
  set(SwiftCore_OBJECT_FORMAT_default "coff")
endif()
