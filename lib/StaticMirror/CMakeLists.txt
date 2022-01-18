

add_swift_host_library(swiftStaticMirror STATIC
   ObjectFileContext.cpp)

 target_link_libraries(swiftStaticMirror PRIVATE
     swiftFrontend
     swiftReflection)
