# This module sets the Swift version number variable consistently across the
# Swift runtime projects.

## Result Variable
#
# ``SWIFT_RUNTIME_VERSION``
#   The computed version number applied to apply to the project.
#   If ``SWIFT_RUNTIME_VERSION`` is set prior to entering the module, the version
#   is not modified.

block(PROPAGATE SWIFT_RUNTIME_VERSION)
  if(SWIFT_RUNTIME_VERSION)
    return()
  endif()

  if($ENV{BUILD_NUMBER})
    # Microsoft build numbers limit each version number component to [0 - 65535]
    # https://learn.microsoft.com/en-us/windows/win32/sbscs/assembly-versions
    math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
    set(BUILD_NUMBER ".${BUILD_NUMBER}")
  endif()
  set(SWIFT_RUNTIME_VERSION 6.3.0${BUILD_NUMBER})
endblock()
