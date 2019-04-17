set(swift_remote_mirror_headers)
list(APPEND swift_remote_mirror_headers
       MemoryReaderInterface.h
       Platform.h
       SwiftRemoteMirror.h
       SwiftRemoteMirrorTypes.h)
install(FILES ${swift_remote_mirror_headers}
        DESTINATION "include/swift/SwiftRemoteMirror"
        COMPONENT swift-remote-mirror-headers)
