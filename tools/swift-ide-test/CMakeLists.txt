add_swift_executable(swift-ide-test
  swift-ide-test.cpp
  DEPENDS swiftParse swiftSema swiftFrontend)

if(HAVE_DARWIN_MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}" )
endif()

install(TARGETS swift-ide-test
  RUNTIME DESTINATION bin)

