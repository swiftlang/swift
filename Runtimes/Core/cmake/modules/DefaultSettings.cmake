# This file is designed to setup reasonable defaults for the various settings so
# that configuring a build for a given platform is likely to build
# out-of-the-box without customization. This does not mean that it is the only
# way that will work. The config files under `cmake/configs` are build
# configurations that are actually shipping.

set(SwiftCore_ENABLE_CRASH_REPORTER_CLIENT_default OFF)
set(SwiftCore_ENABLE_OBJC_INTEROP_default OFF)
set(SwiftCore_ENABLE_TYPE_PRINTING_default ON)
set(SwiftCore_ENABLE_CLOBBER_FREED_OBJECTS_default OFF)
set(SwiftCore_ENABLE_BACKTRACING_default OFF)
set(SwiftCore_BACKTRACER_PATH_default "")

if(APPLE)
  set(SwiftCore_ENABLE_CRASH_REPORTER_CLIENT_default ON)
elseif(CMAKE_SYSTEM_NAME STREQUAL "WASM")
  set(SwiftCore_OBJECT_FORMAT_default "elf")
elseif(LINUX)
  set(SwiftCore_OBJECT_FORMAT_default "elf")
elseif(WIN32)
  set(SwiftCore_OBJECT_FORMAT_default "coff")
endif()
