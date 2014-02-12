add_swift_executable(swift-ide-test
  swift-ide-test.cpp
  DEPENDS swiftFrontend swiftIDE
  COMPONENT_DEPENDS bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})

if(MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}"
                   -DSWIFT_MODULE_CACHE_PATH="${SWIFT_MODULE_CACHE_PATH}" )
endif()

install(TARGETS swift-ide-test
  RUNTIME DESTINATION bin)

