set(swift_stubs_objc_sources)
set(swift_stubs_unicode_normalization_sources)

if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  set(swift_stubs_objc_sources
      Availability.mm
      DispatchShims.mm
      FoundationHelpers.mm
      Reflection.mm
      SwiftNativeNSXXXBase.mm.gyb)
  set(LLVM_OPTIONAL_SOURCES
      UnicodeNormalization.cpp)
else()
  find_package(ICU REQUIRED COMPONENTS uc i18n)
  set(swift_stubs_unicode_normalization_sources
      UnicodeNormalization.cpp)
endif()

add_swift_library(swiftStdlibStubs OBJECT_LIBRARY TARGET_LIBRARY
  Assert.cpp
  CommandLine.cpp
  GlobalObjects.cpp
  LibcShims.cpp
  Stubs.cpp
  UnicodeExtendedGraphemeClusters.cpp.gyb
  ${swift_stubs_objc_sources}
  ${swift_stubs_unicode_normalization_sources}
  C_COMPILE_FLAGS ${SWIFT_RUNTIME_CORE_CXX_FLAGS} -DswiftCore_EXPORTS
  LINK_FLAGS ${SWIFT_RUNTIME_CORE_LINK_FLAGS}
  LINK_LIBRARIES ${swift_stubs_link_libraries}
  INSTALL_IN_COMPONENT stdlib)

