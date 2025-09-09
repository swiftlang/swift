configure_file("${SwiftCore_SWIFTC_SOURCE_DIR}/utils/availability-macros.def"
               "${CMAKE_CURRENT_BINARY_DIR}/availability-macros.def"
               COPYONLY)
file(STRINGS "${CMAKE_CURRENT_BINARY_DIR}/availability-macros.def" availability_defs)
list(FILTER availability_defs EXCLUDE REGEX "^\\s*(#.*)?$")
foreach(def ${availability_defs})
  list(APPEND availability_definitions "-Xfrontend -define-availability -Xfrontend \"${def}\"")

  if("${def}" MATCHES "SwiftStdlib .*")
    # For each SwiftStdlib x.y, also define StdlibDeploymentTarget x.y, which,
    # will expand to the current `-target` platform if the macro defines a
    # newer platform as its availability.
    #
    # There is a setting, SwiftCore_ENABLE_STRICT_AVAILABILITY, which if set
    # ON will cause us to use the "proper" availability instead.
    string(REPLACE "SwiftStdlib" "StdlibDeploymentTarget" current "${def}")
    if(NOT SwiftCore_ENABLE_STRICT_AVAILABILITY AND SwiftCore_SWIFT_AVAILABILITY_PLATFORM)
      if("${SwiftCore_SWIFT_AVAILABILITY_PLATFORM}" STREQUAL "macOS" AND "${SwiftCore_VARIANT_AVAILABILITY_PLATFORM}" STREQUAL "iOS")
        string(REGEX MATCH "iOS ([0-9]+(\.[0-9]+)+)" ios_platform_version "${def}")
        string(REGEX MATCH "[0-9]+(\.[0-9]+)+" ios_version "${ios_platform_version}")
        string(REGEX MATCH "macOS ([0-9]+(\.[0-9]+)+)" macos_platform_version "${def}")
        string(REGEX MATCH "[0-9]+(\.[0-9]+)+" macos_version "${macos_platform_version}")
        if((NOT macos_version STREQUAL "9999" OR NOT ios_version STREQUAL "9999") AND (macos_version VERSION_GREATER CMAKE_OSX_DEPLOYMENT_TARGET OR ios_version VERSION_GREATER SwiftCore_VARIANT_DEPLOYMENT_VERSION))
          string(REGEX REPLACE ":.*" ": macOS ${CMAKE_OSX_DEPLOYMENT_VERSION}, iOS ${SwiftCore_VARIANT_DEPLOYMENT_VERSION}" current "${current}")
        endif()
      else()
        string(REGEX MATCH "${SwiftCore_SWIFT_AVAILABILITY_PLATFORM} ([0-9]+(\.[0-9]+)+)" platform_version "${def}")
        string(REGEX MATCH "[0-9]+(\.[0-9]+)+" version "${platform_version}")
        if(NOT version STREQUAL "9999" AND version VERSION_GREATER CMAKE_OSX_DEPLOYMENT_TARGET)
          string(REGEX REPLACE ":.*" ":${SwiftCore_SWIFT_AVAILABILITY_PLATFORM} ${CMAKE_OSX_DEPLOYMENT_TARGET}" current "${current}")
        endif()
      endif()
    endif()
    list(APPEND availability_definitions "-Xfrontend -define-availability -Xfrontend \"${current}\"")
  endif()
endforeach()

list(JOIN availability_definitions "\n" availability_definitions)
file(GENERATE
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/availability-macros.rsp"
  CONTENT "${availability_definitions}")
add_compile_options("$<$<COMPILE_LANGUAGE:Swift>:SHELL:${CMAKE_Swift_RESPONSE_FILE_FLAG}${CMAKE_CURRENT_BINARY_DIR}/availability-macros.rsp>")
