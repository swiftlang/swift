# Install the EmbeddedSwiftPlatform header into "swift".

if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
add_custom_command(
    OUTPUT "${SWIFT_INCLUDE_DIR}/swift/EmbeddedSwiftPlatform.h"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedSwiftPlatform.h"
    COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedSwiftPlatform.h" "${SWIFT_INCLUDE_DIR}/swift")

add_custom_target("copy_embedded_swift_platform_header"
    DEPENDS "${SWIFT_INCLUDE_DIR}/swift/EmbeddedSwiftPlatform.h"
    COMMENT "Copying Embedded Swift platform header to ${SWIFT_INCLUDE_DIR}/swift")

swift_install_in_component(FILES
    "${CMAKE_CURRENT_SOURCE_DIR}/swift/EmbeddedSwiftPlatform.h"
    DESTINATION "include/swift"
    COMPONENT stdlib)

add_dependencies(embedded-libraries
    "copy_embedded_swift_platform_header")
endif()
