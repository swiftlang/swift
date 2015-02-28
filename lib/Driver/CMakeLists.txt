set(swiftDriver_sources
  Action.cpp
  Compilation.cpp
  DependencyGraph.cpp
  Driver.cpp
  FrontendUtil.cpp
  Job.cpp
  OutputFileMap.cpp
  ParseableOutput.cpp
  Tool.cpp
  Tools.cpp
  ToolChain.cpp
  ToolChains.cpp
  Types.cpp
  Util.cpp
  )

add_swift_library(swiftDriver
  ${swiftDriver_sources}
  DEPENDS SwiftOptions
  LINK_LIBRARIES swiftAST swiftBasic swiftFrontend swiftOption)

if(${SWIFT_ENABLE_TARGET_LINUX})
  foreach(f ${swiftDriver_sources})
    set_property(SOURCE ${f} APPEND_STRING PROPERTY COMPILE_FLAGS
        " -DSWIFT_ENABLE_TARGET_LINUX=\"1\"")
  endforeach()
endif()
