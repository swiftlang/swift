add_swift_unittest(swiftScanTests
  ScanFixture.cpp
  Features.cpp
  PrintTarget.cpp
  ModuleDeps.cpp)

target_link_libraries(swiftScanTests
  PRIVATE
    swiftFrontend
    swiftDependencyScan
    libSwiftScan)

target_compile_definitions(swiftScanTests PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")
