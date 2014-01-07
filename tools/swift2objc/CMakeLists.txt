if(MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}"
                   -DSWIFT_MODULE_CACHE_PATH="${SWIFT_MODULE_CACHE_PATH}" )
endif()

add_swift_executable(swift2objc
  Swift2ObjC.cpp
  DEPENDS swiftSerialization swiftFrontend swiftClangImporter
  COMPONENT_DEPENDS option bitreader support)

install(TARGETS swift2objc
  RUNTIME DESTINATION bin)
