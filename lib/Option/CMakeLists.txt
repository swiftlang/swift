add_swift_host_library(swiftOption STATIC
  Options.cpp
  SanitizerOptions.cpp)
add_dependencies(swiftOption
  SwiftOptionDefinitions)
target_link_libraries(swiftOption PRIVATE
  swiftBasic)

set(features_merger "${SWIFT_SOURCE_DIR}/utils/merge-features.py")
set(features_file_swift_src "${CMAKE_CURRENT_SOURCE_DIR}/features.json")
set(features_file_clang_src "${LLVM_BINARY_DIR}/share/clang/features.json")
set(features_file_dest "${CMAKE_BINARY_DIR}/share/swift/features.json")

add_custom_command(
  OUTPUT
    ${features_file_dest}
  COMMAND
    $<TARGET_FILE:Python3::Interpreter> ${features_merger} -f ${features_file_swift_src} -p \"\" -f ${features_file_clang_src} -p clang- > ${features_file_dest}
  DEPENDS
    ${features_merger}
    ${features_file_swift_src}
    ${features_file_clang_src}
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

