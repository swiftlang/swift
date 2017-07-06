add_swift_host_tool(yaml2json
  yaml2json.cpp
  LINK_LIBRARIES swiftBasic
  LLVM_COMPONENT_DEPENDS support
  )
