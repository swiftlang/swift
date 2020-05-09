// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/mirror_import_overrides_1.h -typecheck -verify %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/mirror_import_overrides_2.h -typecheck -verify %s

// rdar://31471034

func foo(widget: Widget) {
  widget.use { context in
    context.operate()
  }
}

func allowClassAndInstance(widget: Widget) {
  widget.doClassAndInstanceThing()
  Widget.doClassAndInstanceThing()

  _ = widget.classAndInstanceProp
  _ = Widget.classAndInstanceProp
}
