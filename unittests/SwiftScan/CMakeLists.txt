add_swift_unittest(swiftScanTests
  ScanFixture.cpp
  ModuleDeps.cpp)

target_link_libraries(swiftScanTests
  PRIVATE
    SwiftScanTool)

target_compile_definitions(swiftScanTests PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")
