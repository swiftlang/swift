add_swift_host_tool(swiftdt
  Inspector.swift
  main.swift
  stdio.swift
  symbolication.swift
  SWIFT_COMPONENT tools
  COMPILE_FLAGS -I${SWIFT_SOURCE_DIR}/include/swift/SwiftRemoteMirror
  )

add_dependencies(swiftdt swift-stdlib)
target_link_libraries(swiftdt
  swiftRemoteMirror.dylib)
