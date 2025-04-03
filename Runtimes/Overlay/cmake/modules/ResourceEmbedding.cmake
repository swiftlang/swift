# FIXME: it appears that `CMAKE_MT` evaluates to an empty string which prevents
# the use of the variable. This aliases `MT` to `CMAKE_MT` and tries to fallback
# to known spellings for the tool.
if(WIN32 AND BUILD_SHARED_LIBS)
  find_program(MT HINTS ${CMAKE_MT} NAMES mt llvm-mt REQUIRED)
endif()

function(embed_manifest target)
  get_target_property(_EM_TARGET_TYPE ${target} TYPE)
  if(NOT "${_EM_TARGET_TYPE}" MATCHES "SHARED_LIBRARY|EXECUTABLE")
    return()
  endif()

  get_target_property(_EM_BINARY_DIR ${target} BINARY_DIR)
  get_target_property(_EM_NAME ${target} NAME)

  # Evaluate variables
  file(CONFIGURE
    OUTPUT ${_EM_BINARY_DIR}/${_EM_NAME}-${PROJECT_VERSION}.1.manifest.in
    CONTENT [[<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<assembly manifestversion="1.0" xmlns="urn:schemas-microsoft-com:asm.v1">
  <assemblyIdentity
    name="$<TARGET_NAME:@target@>"
    processorArchitecture="@CMAKE_SYSTEM_PROCESSOR@"
    type="win32"
    version="@PROJECT_VERSION@" />
  <file name="$<TARGET_FILE_NAME:@target@>" />
</assembly>]])
  # Evaluate generator expression
  file(GENERATE
    OUTPUT ${_EM_BINARY_DIR}/${_EM_NAME}-${PROJECT_VERSION}.1.manifest
    INPUT ${_EM_BINARY_DIR}/${_EM_NAME}-${PROJECT_VERSION}.1.manifest.in)

  if(WIN32)
    add_custom_command(TARGET ${target} POST_BUILD
      COMMAND "${MT}" -nologo -manifest "${_EM_BINARY_DIR}/${_EM_NAME}-${PROJECT_VERSION}.1.manifest" "-outputresource:$<TARGET_FILE:${target}>;#1")
  endif()
endfunction()
