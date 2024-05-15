// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

struct Box<Wrapped: ~Copyable>: ~Copyable {
  private let _pointer: UnsafeMutablePointer<Wrapped>
  
  init(_ wrapped: consuming Wrapped) {
    _pointer = .allocate(capacity: 1)
    _pointer.initialize(to: wrapped)
  }
  
  deinit {
    _pointer.deinitialize(count: 1)
    _pointer.deallocate()
  }
  
  consuming func move() -> Wrapped {
    let wrapped = _pointer.move()
    _pointer.deallocate()
    discard self
    return wrapped
  }
  
  var wrapped: Wrapped {
    _read { yield _pointer.pointee }
  }
  
  consuming func map(_ transform: (consuming Wrapped)->Wrapped) -> Self {
    _pointer.initialize(to: transform(_pointer.move()))
    return self
  }
}


struct Tree<Element: Comparable>: ~Copyable {
  typealias Link = Box<Node>?
  struct Node: ~Copyable{
    var left: Link = nil
    var element: Element
    var right: Link = nil
  }
  var root: Link = nil
}

extension Tree.Node {
  consuming func insert(_ value: consuming Element) -> Self {
    if value < element {
      switch left {
      case nil:
        .init(left: .init(.init(element: value)), element: element, right: right)
      case let box?:
        .init(left: box.map { (node: consuming Self) -> Self in node.insert(value) }, element: element, right: right)
      }
    } else if element < value {
      switch right {
      case nil:
        .init(left: left, element: element, right: .init(.init(element: value)))
      case let box?:
        .init(left: left, element: element, right: box.map { (node: consuming Self) -> Self in node.insert(value) })
      }
    } else {
      self
    }
  }
}

extension Tree {
  mutating func insert(_ value: consuming Element) {
    self = 
      switch root {
      case nil:
        .init(root: .init(.init(element: value)))
      case let box?:
        .init(root: box.map { (node: consuming Node) -> Node in node.insert(value) })
    }
  }
}

extension Tree.Node {
  func forEach(_ body: (borrowing Element)->Void) {
    switch left {
    case nil: break
    case .some(_borrowing box):
      box.wrapped.forEach(body)
    }
    body(element)
    switch right {
    case nil: break
    case .some(_borrowing box):
      box.wrapped.forEach(body)
    }
  }
}

extension Tree {
  func forEach(_ body: (borrowing Element)->Void) {
    switch root {
    case nil: return
    case .some(_borrowing box): box.wrapped.forEach(body)
    }
  }
}

let engines = [
    "Daisy", "Salty", "Harold", "Cranky",
    "Thomas", "Henry", "James", "Toby",
    "Belle", "Diesel", "Stepney", "Gordon",
    "Captain", "Percy", "Arry", "Bert",
    "Spencer",
]

// CHECK: Arry,Belle,Bert,Captain,Cranky,Daisy,Diesel,Gordon,Harold,Henry,James,Percy,Salty,Spencer,Stepney,Thomas,Toby,

defer { main() }
func main() {
  var tree: Tree<String> = .init()
  for engine in engines {
    tree.insert(engine)
  }
  tree.forEach { print($0, terminator: ",") }
  print("")
}
