
# Only test this if we found a Swift compiler. Currently only enable this test
# if we build on Darwin since we do not have a host toolchain available when
# compiling on the bots for Linux meaning this path would not be tested.
if (CMAKE_Swift_COMPILER AND APPLE)
  add_subdirectory(swift-cmake)
endif()
