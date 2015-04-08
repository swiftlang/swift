add_swift_executable(swift
  driver.cpp
  autolink_extract_main.cpp
  frontend_main.cpp
  update_main.cpp
  LINK_LIBRARIES
    swiftIDE
    swiftDriver swiftIRGen swiftSIL swiftSILGen swiftSILPasses
    swiftImmediate
    swiftSerialization
    swiftPrintAsObjC
    swiftFrontend
    swiftClangImporter
    swiftOption
)

target_link_libraries(swift edit)

add_custom_command(TARGET swift POST_BUILD
    COMMAND "${CMAKE_COMMAND}" "-E" "create_symlink" "swift" "swiftc"
    WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

add_custom_command(TARGET swift POST_BUILD
    COMMAND "${CMAKE_COMMAND}" "-E" "create_symlink" "swift" "swift-autolink-extract"
    WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift clang-headers)
endif()

# Platforms that have a REPL need extra libraries to be linked into the 'swift'
# binary.
include(SwiftDarwin)
if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  # Link in CoreFoundation on Darwin.
  find_library(CORE_FOUNDATION NAMES CoreFoundation)
  target_link_libraries(swift ${CORE_FOUNDATION})

  # Link in arclite on Darwin.
  get_default_toolchain_dir(toolchain_dir)
  set(SWIFT_REPL_ARCLITE "${toolchain_dir}/usr/lib/arc/libarclite_${SWIFT_HOST_VARIANT}.a")
  set_property(TARGET swift APPEND_STRING PROPERTY
      LINK_FLAGS " -lobjc -Wl,-force_load,\"${SWIFT_REPL_ARCLITE}\"")
endif()

swift_install_in_component(compiler
    TARGETS swift
    RUNTIME DESTINATION "bin")
swift_install_in_component(compiler
    FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swiftc"
    DESTINATION "bin")

if(SWIFT_HOST_VARIANT STREQUAL "LINUX")
# No need for this on other platforms.
  swift_install_in_component(compiler
      FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-autolink-extract"
      DESTINATION "bin")
endif()
