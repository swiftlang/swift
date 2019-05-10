# Detect B&I builds.
set(SWIFT_BNI_OS_BUILD FALSE)
set(SWIFT_BNI_XCODE_BUILD FALSE)
if(DEFINED ENV{RC_XBS})
  if(NOT DEFINED ENV{RC_XCODE} OR NOT "$ENV{RC_XCODE}")
    set(SWIFT_BNI_OS_BUILD TRUE)
  else()
    set(SWIFT_BNI_XCODE_BUILD TRUE)
  endif()
endif()

configure_file(CMakeConfig.h.in ${CMAKE_CURRENT_BINARY_DIR}/CMakeConfig.h
               ESCAPE_QUOTES @ONLY)
