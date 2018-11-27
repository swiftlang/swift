if(XCODE)
  file(GLOB_RECURSE REMOTE_LIB_HEADERS
    ${SWIFT_SOURCE_DIR}/include/swift/Remote/*.h
    ${SWIFT_SOURCE_DIR}/include/swift/Remote/*.def)

  set_source_files_properties(${REMOTE_LIB_HEADERS}
    PROPERTIES
    HEADER_FILE_ONLY true)
  source_group("libRemote Headers" FILES ${REMOTE_LIB_HEADERS})
else()
  set(REMOTE_LIB_HEADERS)
endif()

add_swift_host_library(swiftRemoteAST STATIC
  RemoteAST.cpp
  InProcessMemoryReader.cpp
  ${REMOTE_LIB_HEADERS}
  LINK_LIBRARIES
    swiftSema swiftIRGen)
