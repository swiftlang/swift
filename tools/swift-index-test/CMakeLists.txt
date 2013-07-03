add_swift_executable(swift-index-test
  swift-index-test.cpp
  DEPENDS swiftParse swiftSema swiftFrontend)

if(HAVE_DARWIN_MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}" )
endif()

install(TARGETS swift-index-test
  RUNTIME DESTINATION bin)

