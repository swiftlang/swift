struct Box<T> {}

protocol Widget {
  associatedtype Model: Equatable

  func draw()
  var model: Self.Model { get }
  var friends: [Self] { get }
  func wrapInBox() -> Box<Self>
}

func memberRefExample(_ widget: any Widget) {
  widget.draw()                   // 1: okay
  let model = widget.model        // 2: okay
  let friends = widget.friends    // 3: okay
  let box = widget.wrapInBox()    // 4: error
}
