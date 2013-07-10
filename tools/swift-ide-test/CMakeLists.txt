add_swift_executable(swift-ide-test
  swift-ide-test.cpp
  DEPENDS swiftFrontend swiftIDE
  COMPONENT_DEPENDS bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})

if(HAVE_DARWIN_MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}" )
endif()

install(TARGETS swift-ide-test
  RUNTIME DESTINATION bin)

