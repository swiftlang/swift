func memberRefLift(_ widget: any Widget) {
  func workWithBox(_: some Widget) {
    let box = widget.wrapInBox()  // okay
    // do stuff with `box'
  }

  workWithBox(widget)  // okay
}
