// RUN: %empty-directory(%t)

// Copy the module map and headers into a sandbox.
// RUN: mkdir %t/sandbox
// RUN: cp %S/Inputs/custom-modules/EmbeddedFiles/module.map %t/sandbox
// RUN: cp %S/Inputs/custom-modules/EmbeddedFiles/textual.inc %t/sandbox
// RUN: cp %S/Inputs/custom-modules/EmbeddedFiles/types.h %t/sandbox

// Embed all input files in the explicit module. This makes it self-contained,
// letting us copy it around without bringing along all of its headers.
// RUN: %target-swift-emit-pcm -module-name EmbeddedFiles -Xcc -Xclang -Xcc -fmodules-embed-all-files -o %t/EmbeddedFiles.pcm %t/sandbox/module.map

// Delete the headers from the sandbox.
// RUN: rm %t/sandbox/textual.inc %t/sandbox/types.h

// Verify that ClangImporter still reports that the module is importable.
// Use the `-fmodule-file=NAME=PATH` form of the flag that only loads the
// module when it is requested.
// RUN: %target-swift-frontend -typecheck -verify -Xcc -fmodule-map-file=%t/sandbox/module.map -Xcc -fmodule-file=EmbeddedFiles=%t/EmbeddedFiles.pcm %s

#if canImport(EmbeddedFiles)
  import EmbeddedFiles
#endif

var _ = EmbeddedFilesTy(
    field_number: 0,
    field_name: nil)
