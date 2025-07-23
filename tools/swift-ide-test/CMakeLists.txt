# Early swift-driver is built adjacent to the compiler (swift build dir)
if(CMAKE_BUILD_TYPE STREQUAL "Release" OR CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo")
  set(driver_package_configuration_dir "release")
else()
  set(driver_package_configuration_dir "debug")
endif()

get_target_property(LIB_SCAN_SOURCE_DIR libSwiftScan SOURCE_DIR)

set(LIB_SCAN_EXPORTED_SYMBOL_FILE
    ${LIB_SCAN_SOURCE_DIR}/libSwiftScan.exports)

message(STATUS "libSwiftScan: ${LIB_SCAN_SOURCE_DIR}")
message(STATUS "swift-driver: ${CMAKE_BINARY_DIR}/../earlyswiftdriver-${SWIFT_HOST_VARIANT}-${SWIFT_HOST_VARIANT_ARCH}/${driver_package_configuration_dir}/cmake/modules/")
message(STATUS "swift-tools-support-core: ${CMAKE_BINARY_DIR}/../earlyswiftdriver-${SWIFT_HOST_VARIANT}-${SWIFT_HOST_VARIANT_ARCH}/${driver_package_configuration_dir}/dependencies/swift-tools-support-core/cmake/modules/")

set(SwiftDriver_DIR
    "${CMAKE_BINARY_DIR}/../earlyswiftdriver-${SWIFT_HOST_VARIANT}-${SWIFT_HOST_VARIANT_ARCH}/${driver_package_configuration_dir}/cmake/modules/")
set(TSC_DIR    "${CMAKE_BINARY_DIR}/../earlyswiftdriver-${SWIFT_HOST_VARIANT}-${SWIFT_HOST_VARIANT_ARCH}/${driver_package_configuration_dir}/dependencies/swift-tools-support-core/cmake/modules/")
find_package(TSC CONFIG)
find_package(SwiftDriver CONFIG)


add_swift_host_tool(swift-ide-test
  swift-ide-test.cpp
  ModuleAPIDiff.cpp
  XMLValidator.cpp
  SWIFT_COMPONENT tools
  THINLTO_LD64_ADD_FLTO_CODEGEN_ONLY
  HAS_SWIFT_MODULES
)

set_property(
  TARGET swift-ide-test
  APPEND PROPERTY INSTALL_RPATH "${CMAKE_BINARY_DIR}/../earlyswiftdriver-${SWIFT_HOST_VARIANT}-${SWIFT_HOST_VARIANT_ARCH}/${driver_package_configuration_dir}/lib/"
)
set_property(
  TARGET swift-ide-test
  APPEND PROPERTY INSTALL_RPATH "${CMAKE_BINARY_DIR}/../earlyswiftdriver-${SWIFT_HOST_VARIANT}-${SWIFT_HOST_VARIANT_ARCH}/${driver_package_configuration_dir}/dependencies/swift-tools-support-core/lib/"
)

target_link_libraries(swift-ide-test
                      PRIVATE
                        swiftAST
                        swiftDriver
                        SwiftDriver
                        swiftFrontend
                        swiftIDE
                        swiftIDETool
                        swiftCompilerModules
                        libSwiftScanDependencies)
target_sources(swift-ide-test
  PRIVATE
  $<TARGET_OBJECTS:libSwiftScan>)

# This is required because swift-driver in "integrated" mode depends on libSwiftScan symbols
# that have to be re-exported from the `libSwiftScan` library.
add_llvm_symbol_exports(swift-ide-test ${LIB_SCAN_EXPORTED_SYMBOL_FILE})

# If libxml2 is available, make it available for swift-ide-test.
if(LLVM_ENABLE_LIBXML2)
  target_link_libraries(swift-ide-test PRIVATE LibXml2::LibXml2)
  target_compile_definitions(swift-ide-test PRIVATE SWIFT_HAVE_LIBXML=1)
endif()

# Create a symlink for swift-api-dump.py in the bin directory
swift_create_post_build_symlink(swift-ide-test
  SOURCE "${SWIFT_SOURCE_DIR}/utils/swift-api-dump.py"
  DESTINATION "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-api-dump.py"
  COMMENT "Creating development symlink for swift-api-dump.py.")

