# This file is designed to setup reasonable defaults for the various settings so
# that configuring a build for a given platform is likely to build
# out-of-the-box without customization. This does not mean that it is the only
# way that will work, or that it represents a shipping configuration.
# User-specified configurations should be done through cache files or by setting
# the variable with `-DSwiftOverlay_*` on the commandline.

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
  set(SwiftOverlay_ENABLE_REFLECTION_default ON)
elseif(CMAKE_SYSTEM_NAME STREQUAL "WASM")
  set(SwiftOverlay_ENABLE_REFLECTION_default OFF)
elseif(LINUX OR ANDROID OR BSD)
  set(SwiftOverlay_ENABLE_REFLECTION_default OFF)
elseif(WIN32)
  set(SwiftOverlay_ENABLE_REFLECTION_default ON)
endif()

include("${SwiftOverlay_VENDOR_MODULE_DIR}/DefaultSettings.cmake" OPTIONAL)
