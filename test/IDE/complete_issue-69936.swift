// RUN: %batch-code-completion

struct Test {
  var currentHeight: Int

  func subscribeToKeyboardEvents() {
    objectAndKeyPath(object: self, keyPath: \.#^COMPLETE^#)
  }
}

func objectAndKeyPath<Root, Input>(object: Root, keyPath: WritableKeyPath<Root, Input>) {}

// COMPLETE: Decl[InstanceVar]/CurrNominal:      currentHeight[#Int#]; name=currentHeight
