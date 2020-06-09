// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name EmptyExtension -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name EmptyExtension -I %t -pretty-print -output-dir %t
// RUN: %{python} -c 'import os.path; import sys; sys.exit(1 if os.path.exists(sys.argv[1]) else 0)' %t/EmptyExtension@Swift.symbols.json

extension Sequence {
  func foo() {}
}
