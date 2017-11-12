add_swift_target_executable(swift-reflection-test BUILD_WITH_STDLIB
  swift-reflection-test.c
  overrides.c
  LINK_FAT_LIBRARIES
    swiftRemoteMirror)

