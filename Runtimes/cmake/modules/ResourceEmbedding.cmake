
if(WIN32)
  option(SWIFT_ASSEMBLY_VERSION "Assembly Version" "${PROJECT_VERSION}")
  option(SWIFT_PUBLIC_KEY_TOKEN "Code Signing Identity" "0000000000000000")
endif()

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

function(embed_version_info target)
  if(NOT WIN32)
    return()
  endif()

  get_target_property(_EM_TARGET_TYPE ${target} TYPE)
  if(NOT "${_EM_TARGET_TYPE}" MATCHES "SHARED_LIBRARY|EXECUTABLE")
    return()
  endif()

  cmake_parse_arguments(PARSE_ARGV 1 _EVI
    ""
    "FILE_DESCRIPTION;INTERNAL_NAME;ORIGINAL_FILENAME;PRODUCT_NAME"
    "")

  if(NOT _EVI_INTERNAL_NAME)
    set(_EVI_INTERNAL_NAME "${target}")
  endif()
  if(NOT _EVI_FILE_DESCRIPTION)
    set(_EVI_FILE_DESCRIPTION "${_EVI_INTERNAL_NAME}")
  endif()
  if(NOT _EVI_PRODUCT_NAME)
    set(_EVI_PRODUCT_NAME "Swift")
  endif()
  if(NOT _EVI_ORIGINAL_FILENAME)
    set(_EVI_ORIGINAL_FILENAME "$<TARGET_FILE_NAME:${target}>")
  endif()

  get_target_property(_EVI_BINARY_DIR ${target} BINARY_DIR)
  get_target_property(_EVI_NAME ${target} NAME)

  # Pad the project version to a four-part `MAJOR.MINOR.PATCH.TWEAK`
  string(REGEX MATCHALL "[0-9]+" version_parts "${SWIFT_ASSEMBLY_VERSION}")
  list(LENGTH version_parts version_part_count)
  while(version_part_count LESS 4)
    list(APPEND version_parts "0")
    math(EXPR version_part_count "${version_part_count} + 1")
  endwhile()
  list(GET version_parts 0 _EVI_VERSION_MAJOR)
  list(GET version_parts 1 _EVI_VERSION_MINOR)
  list(GET version_parts 2 _EVI_VERSION_PATCH)
  list(GET version_parts 3 _EVI_VERSION_TWEAK)
  set(_EVI_VERSION_STRING "${_EVI_VERSION_MAJOR}.${_EVI_VERSION_MINOR}.${_EVI_VERSION_PATCH}.${_EVI_VERSION_TWEAK}")

  file(CONFIGURE
    OUTPUT ${_EVI_BINARY_DIR}/${_EVI_NAME}.rc.in
    CONTENT [[
#include <winuser.h>

1 VERSIONINFO
FILEVERSION    @_EVI_VERSION_MAJOR@,@_EVI_VERSION_MINOR@,@_EVI_VERSION_PATCH@,@_EVI_VERSION_TWEAK@
PRODUCTVERSION @_EVI_VERSION_MAJOR@,@_EVI_VERSION_MINOR@,@_EVI_VERSION_PATCH@,@_EVI_VERSION_TWEAK@
BEGIN
  BLOCK "StringFileInfo"
  BEGIN
    // U.S. English (LangID 0x0409), UTF-8 (CP 65001)
    BLOCK "0409FDE9"
    BEGIN
      VALUE "CompanyName",      "Swift Open Source Project"
      VALUE "FileDescription",  "@_EVI_FILE_DESCRIPTION@"
      VALUE "FileVersion",      "@_EVI_VERSION_STRING@"
      VALUE "InternalName",     "@_EVI_INTERNAL_NAME@"
      VALUE "LegalCopyright",   "Copyright (c) Apple Inc. and the Swift project authors. Licensed under Apache License v2.0 with Runtime Library Exception."
      VALUE "OriginalFilename", "@_EVI_ORIGINAL_FILENAME@"
      VALUE "ProductName",      "@_EVI_PRODUCT_NAME@"
      VALUE "ProductVersion",   "@_EVI_VERSION_STRING@"
    END
  END

  BLOCK "VarFileInfo"
  BEGIN
    // Translation must match the StringFileInfo BLOCK key above.
    VALUE "Translation", 0x0409, 0xFDE9
  END
END
    ]])
  file(GENERATE
    OUTPUT ${_EVI_BINARY_DIR}/${_EVI_NAME}.rc
    INPUT ${_EVI_BINARY_DIR}/${_EVI_NAME}.rc.in)
  target_sources(${target} PRIVATE
    ${_EVI_BINARY_DIR}/${_EVI_NAME}.rc)
endfunction()

function(embed_manifest target)
  if(NOT WIN32)
    return()
  endif()

  get_target_property(_EM_TARGET_TYPE ${target} TYPE)
  if(NOT "${_EM_TARGET_TYPE}" MATCHES "SHARED_LIBRARY|EXECUTABLE")
    return()
  endif()

  cmake_parse_arguments(PARSE_ARGV 1 _EM
    ""
    "FILE_DESCRIPTION;INTERNAL_NAME;ORIGINAL_FILENAME;PRODUCT_NAME"
    "")

  if(NOT _EM_INTERNAL_NAME)
    set(_EM_INTERNAL_NAME "${target}")
  endif()
  if(NOT _EM_FILE_DESCRIPTION)
    set(_EM_FILE_DESCRIPTION "${_EM_INTERNAL_NAME}")
  endif()
  if(NOT _EM_PRODUCT_NAME)
    set(_EM_PRODUCT_NAME "Swift")
  endif()
  if(NOT _EM_ORIGINAL_FILENAME)
    set(_EM_ORIGINAL_FILENAME "$<TARGET_FILE_NAME:${target}>")
  endif()

  get_target_property(_EM_BINARY_DIR ${target} BINARY_DIR)
  get_target_property(_EM_NAME ${target} NAME)

  # Pad the project version to a four-part `MAJOR.MINOR.PATCH.TWEAK`
  string(REGEX MATCHALL "[0-9]+" version_parts "${SWIFT_ASSEMBLY_VERSION}")
  list(LENGTH version_parts version_part_count)
  while(version_part_count LESS 4)
    list(APPEND version_parts "0")
    math(EXPR version_part_count "${version_part_count} + 1")
  endwhile()
  list(GET version_parts 0 _EM_VERSION_MAJOR)
  list(GET version_parts 1 _EM_VERSION_MINOR)
  list(GET version_parts 2 _EM_VERSION_PATCH)
  list(GET version_parts 3 _EM_VERSION_TWEAK)
  set(_EM_VERSION_STRING "${_EM_VERSION_MAJOR}.${_EM_VERSION_MINOR}.${_EM_VERSION_PATCH}.${_EM_VERSION_TWEAK}")

  string(TOLOWER "${SWIFT_PUBLIC_KEY_TOKEN}" _EM_PUBLIC_KEY_TOKEN)

  # Evaluate variables
  file(CONFIGURE
    OUTPUT ${_EM_BINARY_DIR}/${_EM_NAME}-${_EM_VERSION_STRING}.1.manifest.in
    CONTENT [[<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<assembly manifestVersion="1.0" xmlns="urn:schemas-microsoft-com:asm.v1">
  <assemblyIdentity
    name="$<TARGET_NAME:@target@>"
    processorArchitecture="@CMAKE_SYSTEM_PROCESSOR@"
    publicKeyToken="@_EM_PUBLIC_KEY_TOKEN@"
    type="win32"
    version="@_EM_VERSION_STRING@" />
  <file name="$<TARGET_FILE_NAME:@target@>" />
</assembly>]])
  # Evaluate generator expression
  file(GENERATE
    OUTPUT ${_EM_BINARY_DIR}/${_EM_NAME}-${_EM_VERSION_STRING}.1.manifest
    INPUT ${_EM_BINARY_DIR}/${_EM_NAME}-${_EM_VERSION_STRING}.1.manifest.in)

  file(CONFIGURE
    OUTPUT ${_EM_BINARY_DIR}/${_EM_NAME}.rc.in
    CONTENT [[
#include <winuser.h>

1 VERSIONINFO
FILEVERSION    @_EM_VERSION_MAJOR@,@_EM_VERSION_MINOR@,@_EM_VERSION_PATCH@,@_EM_VERSION_TWEAK@
PRODUCTVERSION @_EM_VERSION_MAJOR@,@_EM_VERSION_MINOR@,@_EM_VERSION_PATCH@,@_EM_VERSION_TWEAK@
BEGIN
  BLOCK "StringFileInfo"
  BEGIN
    // U.S. English (LangID 0x0409), UTF-8 (CP 65001)
    BLOCK "0409FDE9"
    BEGIN
      VALUE "CompanyName",      "Swift Open Source Project"
      VALUE "FileDescription",  "@_EM_FILE_DESCRIPTION@"
      VALUE "FileVersion",      "@_EM_VERSION_STRING@"
      VALUE "InternalName",     "@_EM_INTERNAL_NAME@"
      VALUE "LegalCopyright",   "Copyright (c) Apple Inc. and the Swift project authors. Licensed under Apache License v2.0 with Runtime Library Exception."
      VALUE "OriginalFilename", "@_EM_ORIGINAL_FILENAME@"
      VALUE "ProductName",      "@_EM_PRODUCT_NAME@"
      VALUE "ProductVersion",   "@_EM_VERSION_STRING@"
    END
  END

  BLOCK "VarFileInfo"
  BEGIN
    // Translation must match the StringFileInfo BLOCK key above.
    VALUE "Translation", 0x0409, 0xFDE9
  END
END

LANGUAGE 0, 0
1 RT_MANIFEST "@_EM_NAME@-@_EM_VERSION_STRING@.1.manifest"
    ]])
  file(GENERATE
    OUTPUT ${_EM_BINARY_DIR}/${_EM_NAME}.rc
    INPUT ${_EM_BINARY_DIR}/${_EM_NAME}.rc.in)
  target_sources(${target} PRIVATE
    ${_EM_BINARY_DIR}/${_EM_NAME}.rc)
  target_link_options(${target} PRIVATE
    $<$<PLATFORM_ID:Windows>:LINKER:/MANIFEST:NO>)
endfunction()
