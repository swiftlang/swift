add_swift_executable(swift-ide-test
  swift-ide-test.cpp
  XMLValidator.cpp
  DEPENDS swiftFrontend swiftIDE
  COMPONENT_DEPENDS bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})

if(MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}" )
endif()

# If libxml2 is available, make it available for swift-ide-test.
if (SWIFT_HAVE_LIBXML)
  include_directories(SYSTEM ${LIBXML2_INCLUDE_DIR})
  target_link_libraries(swift-ide-test ${LIBXML2_LIBRARIES})
  add_definitions( -DSWIFT_HAVE_LIBXML="1" )
endif()

install(TARGETS swift-ide-test
  RUNTIME DESTINATION bin)

