add_library(swiftStdlibStubs OBJECT
  Assert.cpp
  GlobalObjects.cpp
  LibcShims.cpp
  Random.cpp
  Stubs.cpp
  ThreadLocalStorage.cpp
  MathStubs.cpp
  Unicode/UnicodeData.cpp
  Unicode/UnicodeGrapheme.cpp
  Unicode/UnicodeNormalization.cpp
  Unicode/UnicodeScalarProps.cpp
  Unicode/UnicodeWord.cpp)

if(SwiftCore_ENABLE_OBJC_INTEROP)
  gyb_expand(SwiftNativeNSXXXBase.mm.gyb "SwiftNativeNSXXXBase.mm")
  target_sources(swiftStdlibStubs PRIVATE
    # ObjC files
    Availability.mm
    FoundationHelpers.mm
    OptionalBridgingHelper.mm
    Reflection.mm
    SwiftNativeNSObject.mm
    SwiftNativeNSXXXBaseARC.m
    "${CMAKE_CURRENT_BINARY_DIR}/SwiftNativeNSXXXBase.mm")
endif()


target_compile_definitions(swiftStdlibStubs PRIVATE
  $<$<BOOL:${BUILD_SHARED_LIBS}>:-DswiftCore_EXPORTS>
  $<$<BOOL:${SwiftCore_ENABLE_UNICODE_DATA}>:-DSWIFT_STDLIB_ENABLE_UNICODE_DATA>
  $<$<BOOL:${SwiftCore_HAS_LOCALE}>:-DSWIFT_STDLIB_HAS_LOCALE>)

target_link_libraries(swiftStdlibStubs PRIVATE
  swiftShims)

target_include_directories(swiftStdlibStubs PRIVATE
  "${PROJECT_BINARY_DIR}/include"
  # FIXME: pulls in headers from the main compiler build
  # Assert.cpp:
  #   swift/Runtime/Config.h
  #   swift/Runtime/Debug.h
  #   swift/Runtime/Portability.h
  # Stubs.cpp:
  #   swift/Runtime/Debug.h
  # GlobalObjects.cpp:
  #   swift/Basic/type_traits.h
  # Random.cpp:
  #   swift/Runtime/Config.h
  # UnicodeScalarProps.cpp:
  #   swift/Runtime/Debug.h
  # ...
  #
  "${SwiftCore_SWIFTC_SOURCE_DIR}/include")

if(APPLE)
  set_property(SOURCE SwiftNativeNSXXXBaseARC.m
               APPEND_STRING
               PROPERTY COMPILE_FLAGS "-fobjc-arc")
endif()

if(NOT BUILD_SHARED_LIBS)
  install(TARGETS swiftStdlibStubs
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
endif()
