# libswiftRemoteMirror.dylib should not have runtime dependencies; it's
# always built as a shared library.
if(SWIFT_BUILD_DYNAMIC_STDLIB)
  add_swift_target_library(swiftRemoteMirror
                           SHARED DONT_EMBED_BITCODE NOSWIFTRT
                           SwiftRemoteMirror.cpp
                           LINK_LIBRARIES
                             swiftReflection
                           C_COMPILE_FLAGS
                             ${SWIFT_RUNTIME_CXX_FLAGS} -DswiftRemoteMirror_EXPORTS
                           LINK_FLAGS
                             ${SWIFT_RUNTIME_LINK_FLAGS}
                           SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
                           INSTALL_IN_COMPONENT
                             swift-remote-mirror)
endif()

# Build a specific version for the host with the host toolchain.  This is going
# to be used by tools (e.g. lldb)
if(SWIFT_INCLUDE_TOOLS)
  if(NOT SWIFT_BUILD_DYNAMIC_STDLIB)
    add_custom_target(swiftRemoteMirror-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR})
  endif()

  if(NOT SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER)
    set(CURRENT_CMAKE_C_COMPILER ${CMAKE_C_COMPILER})
    set(CURRENT_CMAKE_CXX_COMPILER ${CMAKE_CXX_COMPILER})
    set(CMAKE_C_COMPILER ${HOST_CMAKE_C_COMPILER})
    set(CMAKE_CXX_COMPILER ${HOST_CMAKE_CXX_COMPILER})
  endif()

  add_swift_host_library(swiftRemoteMirror STATIC
    SwiftRemoteMirror.cpp)
  target_compile_definitions(swiftRemoteMirror PRIVATE _LIB)
  target_compile_options(swiftRemoteMirror PRIVATE
    ${SWIFT_RUNTIME_CXX_FLAGS})
  set_property(TARGET swiftRemoteMirror APPEND_STRING PROPERTY LINK_FLAGS
    ${SWIFT_RUNTIME_LINK_FLAGS})

  if(NOT SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER)
    set(CMAKE_C_COMPILER ${CURRENT_CMAKE_C_COMPILER})
    set(CMAKE_CXX_COMPILER ${CURRENT_CMAKE_CXX_COMPILER})
  endif()
endif()
