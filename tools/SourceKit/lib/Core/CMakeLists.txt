
add_sourcekit_library(SourceKitCore
  Context.cpp
  LangSupport.cpp
  NotificationCenter.cpp
)
target_link_libraries(SourceKitCore PRIVATE
  SourceKitSupport)
