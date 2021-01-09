add_swift_unittest(swiftScanTests
  ScanFixture.cpp
  ModuleDeps.cpp)

target_link_libraries(swiftScanTests
  PRIVATE
    swiftDependencyScan
    libSwiftScan)

target_compile_definitions(swiftScanTests PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")
