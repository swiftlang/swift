function(generate_plist project_name project_version target)
  set(PLIST_INFO_PLIST "Info.plist")
  set(PLIST_INFO_NAME "${project_name}")

  # Underscores aren't permitted in the bundle identifier.
  string(REPLACE "_" "" PLIST_INFO_UTI "com.apple.dt.runtime.${PLIST_INFO_NAME}")
  set(PLIST_INFO_VERSION "${project_version}")
  set(PLIST_INFO_BUILD_VERSION "${project_version}")

  set(PLIST_INFO_PLIST_OUT "${PLIST_INFO_PLIST}")
  set(PLIST_INFO_PLIST_IN "${PROJECT_SOURCE_DIR}/${PLIST_INFO_PLIST}.in")

  if(APPLE)
    target_link_options(${target} PRIVATE
      "SHELL:-Xlinker -sectcreate -Xlinker __TEXT -Xlinker __info_plist -Xlinker ${CMAKE_CURRENT_BINARY_DIR}/${PLIST_INFO_PLIST_OUT}")
  endif()

  configure_file(
      "${PLIST_INFO_PLIST_IN}"
      "${PLIST_INFO_PLIST_OUT}"
      @ONLY
      NEWLINE_STYLE UNIX)

  set_property(TARGET ${target} APPEND PROPERTY LINK_DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${PLIST_INFO_PLIST_OUT}")

  # If Application Extensions are enabled, pass the linker flag marking
  # the dylib as safe.
  if (CXX_SUPPORTS_FAPPLICATION_EXTENSION AND (NOT DISABLE_APPLICATION_EXTENSION))
    list(APPEND link_flags "-Wl,-application_extension")
  endif()
endfunction()

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
