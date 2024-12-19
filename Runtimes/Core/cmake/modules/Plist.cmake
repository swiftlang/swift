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
