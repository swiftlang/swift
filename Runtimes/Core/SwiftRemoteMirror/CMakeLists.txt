
add_library(swiftRemoteMirror
  SwiftRemoteMirror.cpp)
target_compile_definitions(swiftRemoteMirror PUBLIC
  $<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:swiftRemoteMirror_STATIC>)
target_link_libraries(swiftRemoteMirror PRIVATE
  swiftRemoteInspection)
target_include_directories(swiftRemoteMirror PRIVATE
  "${PROJECT_BINARY_DIR}/include"
  "${SwiftCore_SWIFTC_SOURCE_DIR}/include")
target_link_libraries(swiftRemoteMirror PRIVATE
  # FIXME(compnerd) why is this required? These should be transitively applied
  # from `swiftRemoteInspection`
  swiftDemangling
  $<$<BOOL:${SwiftCore_ENABLE_CRASH_REPORTER_CLIENT}>:swiftDemanglingCR>
  swiftLLVMSupport
  swiftShims)

install(TARGETS swiftRemoteMirror
    EXPORT SwiftCoreTargets
    COMPONENT SwiftCore_runtime
    ARCHIVE DESTINATION "${SwiftCore_INSTALL_LIBDIR}"
    LIBRARY DESTINATION "${SwiftCore_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
install(FILES
  ${SwiftCore_SWIFTC_SOURCE_DIR}/include/swift/SwiftRemoteMirror/MemoryReaderInterface.h
  ${SwiftCore_SWIFTC_SOURCE_DIR}/include/swift/SwiftRemoteMirror/Platform.h
  ${SwiftCore_SWIFTC_SOURCE_DIR}/include/swift/SwiftRemoteMirror/SwiftRemoteMirror.h
  ${SwiftCore_SWIFTC_SOURCE_DIR}/include/swift/SwiftRemoteMirror/SwiftRemoteMirrorTypes.h
  ${SwiftCore_SWIFTC_SOURCE_DIR}/include/swift/SwiftRemoteMirror/module.modulemap
  DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/swift/SwiftRemoteMirror")

# Configure plist creation for Darwin platforms.
generate_plist("${CMAKE_PROJECT_NAME}" "${CMAKE_PROJECT_VERSION}" swiftRemoteMirror)
embed_manifest(swiftRemoteMirror)
