# Redirects configuration flow to the embedded-specific CMakeLists file when
# building the Swift runtime in an embedded configuration.
#
# CMake will ends its processing of the current file at the location of the
# `swift_embedded_guard` command when building the embedded runtime.
macro(swift_embedded_guard)
if(SwiftCore_EMBEDDED)
  include(CMakeLists.embedded.txt)
  return()
endif()
endmacro()
