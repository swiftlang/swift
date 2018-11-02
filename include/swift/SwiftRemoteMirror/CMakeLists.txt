set(swift_remote_mirror_headers)
list(APPEND swift_remote_mirror_headers
       MemoryReaderInterface.h
       Platform.h
       SwiftRemoteMirror.h
       SwiftRemoteMirrorTypes.h)
swift_install_in_component("swift-remote-mirror-headers"
                           FILES
                             ${swift_remote_mirror_headers}
                           DESTINATION
                             "include/swift/SwiftRemoteMirror")

