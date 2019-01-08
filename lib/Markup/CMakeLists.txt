add_swift_host_library(swiftMarkup STATIC
  AST.cpp
  LineList.cpp
  Markup.cpp
  
  LINK_LIBRARIES
    libcmark_static)
target_compile_definitions(swiftMarkup
                           PRIVATE
                             CMARK_STATIC_DEFINE)

