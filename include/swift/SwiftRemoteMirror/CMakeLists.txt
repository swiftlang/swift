set(swift_remote_mirror_headers)
list(APPEND swift_remote_mirror_headers
       MemoryReaderInterface.h
       Platform.h
       SwiftRemoteMirror.h
       SwiftRemoteMirrorTypes.h
       module.modulemap)
swift_install_in_component(FILES
                             ${swift_remote_mirror_headers}
                           DESTINATION
                             "include/swift/SwiftRemoteMirror"
                           COMPONENT
                             swift-remote-mirror-headers)

