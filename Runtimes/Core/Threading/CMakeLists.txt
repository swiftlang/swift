# TODO: I think we can build the one that actually matters based on the
#       platform we're building, rather than relying just on macro definitions
#       and linking empty objects.
#       https://github.com/swiftlang/swift/issues/77435
add_library(swiftThreading OBJECT
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Threading/C11.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Threading/Linux.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Threading/Pthreads.cpp"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/lib/Threading/Win32.cpp")
target_link_libraries(swiftThreading PRIVATE swiftShims)

# FIXME: We should split out the parts that are needed by the runtime
#        to avoid pulling in headers from the compiler.
target_include_directories(swiftThreading PRIVATE
  "${SwiftCore_SWIFTC_SOURCE_DIR}/include")

if(NOT BUILD_SHARED_LIBS)
  install(TARGETS swiftThreading
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime)
endif()
