include(SwiftManpage)

manpage(
    swift-pod
    SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/swift.pod"
    PAGE_HEADER "Swift Documentation"
    MAN_FILE_BASENAME swift
    MAN_SECTION 1
    INSTALL_IN_COMPONENT compiler)

