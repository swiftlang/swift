add_swift_unittest(libInMemoryFrontendTests
  InMemoryFrontendTests.cpp
)

target_link_libraries(libInMemoryFrontendTests PRIVATE
  libInMemoryFrontend
  swiftDriver
)

target_compile_definitions(libInMemoryFrontendTests PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\"
)

include_directories(
  ${SWIFT_SOURCE_DIR}/tools/libInMemoryFrontend/include
)
