add_swift_host_library(swiftOption STATIC
  Options.cpp
  SanitizerOptions.cpp)
add_dependencies(swiftOption
  SwiftOptions)
target_link_libraries(swiftOption PRIVATE
  swiftBasic)

set(features_file_src "${CMAKE_CURRENT_SOURCE_DIR}/features.json")
set(features_file_dest "${CMAKE_BINARY_DIR}/share/swift/features.json")

add_custom_command(
  OUTPUT
    ${features_file_dest}
  COMMAND
    ${CMAKE_COMMAND} -E copy ${features_file_src} ${features_file_dest}
  DEPENDS
    ${features_file_src}
)

add_custom_target(swift-features-file DEPENDS ${features_file_dest})

add_dependencies(swiftOption swift-features-file)

swift_install_in_component(
  FILES
    ${features_file_dest}
  DESTINATION
    "share/swift"
  COMPONENT
    compiler
)

