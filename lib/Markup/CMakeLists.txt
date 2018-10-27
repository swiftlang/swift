add_swift_host_library(swiftMarkup STATIC
  AST.cpp
  LineList.cpp
  Markup.cpp
  
  LINK_LIBRARIES
    libcmark_static)

