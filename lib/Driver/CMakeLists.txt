set(swiftDriver_sources
  Action.cpp
  Compilation.cpp
  DependencyGraph.cpp
  Driver.cpp
  FrontendUtil.cpp
  Job.cpp
  OutputFileMap.cpp
  ParseableOutput.cpp
  ToolChain.cpp
  ToolChains.cpp
  Types.cpp
  )

set(swiftDriver_targetDefines)

add_swift_library(swiftDriver
  ${swiftDriver_sources}
  DEPENDS SwiftOptions
  LINK_LIBRARIES swiftAST swiftBasic swiftFrontend swiftOption)

if(${SWIFT_ENABLE_TARGET_LINUX})
  list(APPEND swiftDriver_targetDefines "LINUX")
endif()

if(${SWIFT_ENABLE_TARGET_FREEBSD})
  list(APPEND swiftDriver_targetDefines "FREEBSD")
endif()

foreach(f ${swiftDriver_sources})
  foreach(d ${swiftDriver_targetDefines})
    set_property(SOURCE ${f} APPEND_STRING PROPERTY COMPILE_FLAGS
        " -DSWIFT_ENABLE_TARGET_${d}=1")
  endforeach()
endforeach()
