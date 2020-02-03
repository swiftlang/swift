add_swift_host_tool(swiftdt
  main.swift
  stdio.swift
  symbolication.swift
  SWIFT_COMPONENT tools
  )

add_dependencies(swiftdt swift-stdlib)
