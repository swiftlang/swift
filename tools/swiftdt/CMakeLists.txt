add_swift_target_executable(swiftdt
  Inspector.swift
  InterpolationExtensions.swift
  main.swift
  RemoteMirrorExtensions.swift
  stdio.swift
  symbolication.swift

  COMPILE_FLAGS
    -I${SWIFT_SOURCE_DIR}/include/swift/SwiftRemoteMirror
  LINK_LIBRARIES
    swiftRemoteMirror
  )
