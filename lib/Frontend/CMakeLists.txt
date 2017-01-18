if(APPLE)
  set(AppleHostVersionDetection AppleHostVersionDetection.mm)
else()
  set(AppleHostVersionDetection)
endif()

add_swift_library(swiftFrontend STATIC
  CompilerInvocation.cpp
  DiagnosticVerifier.cpp
  Frontend.cpp
  FrontendOptions.cpp
  PrintingDiagnosticConsumer.cpp
  SerializedDiagnosticConsumer.cpp
  ${AppleHostVersionDetection}
  DEPENDS SwiftOptions
  LINK_LIBRARIES
    swiftSIL
    swiftOption
    swiftParse
    swiftSema
    swiftSerialization)

