// RUN: %target-swift-ide-test -print-module -module-to-print=SuppressibleProtocols -I %swift_src_root/lib/ClangImporter/SwiftBridging -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: struct View : ~Escapable {
// CHECK: struct Noncopyable : ~Copyable {
