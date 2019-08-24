//===----------------------------------------------------------------------===//
//                              SourceKit README
//===----------------------------------------------------------------------===//

Welcome to SourceKit! SourceKit is a framework for supporting IDE features like
indexing, syntax-coloring, code-completion, etc. In general it provides the
infrastructure that an IDE needs for excellent language support.

SourceKit currently only supports the Swift language.

//===----------------------------------------------------------------------===//
//                       Linking to the SourceKit C API
//===----------------------------------------------------------------------===//

The stable C API for SourceKit is provided via the sourcekitd.framework which
uses an XPC service for process isolation and the libsourcekitdInProc.dylib
library which is in-process.

libsourcekitdInProc.dylib is more convenient for debugging. To use it either:
  -Link to this library instead of the framework ("-lsourcekitdInProc" instead
   of "-framework sourcekitd")
  -Run the binary that linked to the framework using these environment variables:
 DYLD_INSERT_LIBRARIES=/path/to/libsourcekitdInProc.dylib DYLD_FORCE_FLAT_NAMESPACE=1 <...>

//===----------------------------------------------------------------------===//
