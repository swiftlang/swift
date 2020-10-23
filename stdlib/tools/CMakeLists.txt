# Keep in sync with test/CMakeLists.txt: swift-reflection-test is
# only used when testing dynamic stdlib.
if(SWIFT_BUILD_DYNAMIC_STDLIB AND SWIFT_INCLUDE_TESTS)
  add_subdirectory(swift-reflection-test)
endif()
