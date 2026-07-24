add_swift_unittest(swiftScanTests
  ScanFixture.cpp
  PrintTarget.cpp
  ModuleDeps.cpp)

target_link_libraries(swiftScanTests
  PRIVATE
    swiftFrontend
    swiftDependencyScan
    swiftScanImpl)

target_compile_definitions(swiftScanTests PRIVATE
  SWIFTLIB_DIR="${SWIFTLIB_DIR}"
  SWIFTSCAN_NO_EXPORTS)
