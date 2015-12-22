set(swift_stubs_objc_sources)

if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  set(swift_stubs_objc_sources
      Availability.mm
      FoundationHelpers.mm
      SwiftNativeNSXXXBase.mm.gyb)
endif()

add_swift_library(swiftStdlibStubs IS_STDLIB IS_STDLIB_CORE
  Assert.cpp
  GlobalObjects.cpp
  LibcShims.cpp
  Stubs.cpp
  ${swift_stubs_objc_sources}
  C_COMPILE_FLAGS ${SWIFT_CORE_CXX_FLAGS}
  INSTALL_IN_COMPONENT stdlib)

