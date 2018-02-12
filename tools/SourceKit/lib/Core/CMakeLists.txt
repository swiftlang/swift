add_sourcekit_library(SourceKitCore
  Context.cpp
  LangSupport.cpp
  NotificationCenter.cpp
  LINK_LIBS SourceKitSupport
  DEPENDS generated_sourcekit_uids
)
