add_swift_executable(swift-ide-test
  swift-ide-test.cpp
  ModuleAPIDiff.cpp
  XMLValidator.cpp
  LINK_LIBRARIES
    swiftDriver
    swiftFrontend
    swiftIDE
  COMPONENT_DEPENDS
    bitreader bitwriter transformutils option support ${LLVM_TARGETS_TO_BUILD})

# If libxml2 is available, make it available for swift-ide-test.
if(SWIFT_HAVE_LIBXML)
  include_directories(SYSTEM ${LIBXML2_INCLUDE_DIR})
  target_link_libraries(swift-ide-test ${LIBXML2_LIBRARIES})
  add_definitions(-DSWIFT_HAVE_LIBXML="1")
endif()

swift_install_in_component(tools
    TARGETS swift-ide-test
    RUNTIME DESTINATION bin)

