// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/mirror_import_overrides_1.h -typecheck -verify %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/mirror_import_overrides_2.h -typecheck -verify %s

// REQUIRES: objc_interop

// rdar://31471034

func foo(widget: Widget) {
  widget.use { context in
    context.operate()
  }
}
