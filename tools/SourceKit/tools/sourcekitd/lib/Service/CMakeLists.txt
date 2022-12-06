
add_sourcekit_library(sourcekitdService
  Requests.cpp
)
target_link_libraries(sourcekitdService PRIVATE
  sourcekitdAPI
  swiftBasic
  SourceKitSupport
  SourceKitSwiftLang
  swiftCompilerModules)
