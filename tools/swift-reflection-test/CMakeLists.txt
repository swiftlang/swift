add_swift_target_executable(swift-reflection-test BUILD_WITH_STDLIB
  swift-reflection-test.cpp
  LINK_FAT_LIBRARIES
    swiftReflection)

