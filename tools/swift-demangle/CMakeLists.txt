add_swift_executable(swift-demangle
  swift-demangle.cpp
  DEPENDS swiftSIL
  COMPONENT_DEPENDS bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})

if(HAVE_DARWIN_MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}" )
endif()

install(TARGETS swift-demangle
  RUNTIME DESTINATION bin)
