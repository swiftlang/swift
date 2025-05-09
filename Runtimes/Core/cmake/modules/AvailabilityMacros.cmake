configure_file("${SwiftCore_SWIFTC_SOURCE_DIR}/utils/availability-macros.def"
               "${CMAKE_CURRENT_BINARY_DIR}/availability-macros.def"
               COPYONLY)
file(STRINGS "${CMAKE_CURRENT_BINARY_DIR}/availability-macros.def" availability_defs)
list(FILTER availability_defs EXCLUDE REGEX "^\\s*(#.*)?$")
foreach(def ${availability_defs})
  add_compile_options("$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -define-availability -Xfrontend \"${def}\">")

  if("${def}" MATCHES "SwiftStdlib .*")
    # For each SwiftStdlib x.y, also define SwiftStdlibTargetOS x.y, which,
    # will expand to the current `-target` platform if the macro defines a
    # newer platform as its availability.
    #
    # There is a setting, SwiftCore_ENABLE_STRICT_AVAILABILITY, which if set
    # ON will cause us to use the "proper" availability instead.
    string(REPLACE "SwiftStdlib" "SwiftStdlibCurrentOS" current "${def}")
    if(NOT SwiftCore_ENABLE_STRICT_AVAILABILITY AND SwiftCore_SWIFT_AVAILABILITY_PLATFORM)
      string(REGEX MATCH "${SwiftCore_SWIFT_AVAILABILITY_PLATFORM} ([0-9]+(\.[0-9]+)+)" platform_version "${def}")
      string(REGEX MATCH "[0-9]+(\.[0-9]+)+" version "${platform_version}")
      if(NOT version STREQUAL "9999" AND version VERSION_GREATER CMAKE_OSX_DEPLOYMENT_TARGET)
        string(REGEX REPLACE ":.*" ":${SwiftCore_SWIFT_AVAILABILITY_PLATFORM} ${CMAKE_OSX_DEPLOYMENT_TARGET}" current "${current}")
      endif()
    endif()
    add_compile_options("$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -define-availability -Xfrontend \"${current}\">")
  endif()
endforeach()
