// RUN: %target-swift-frontend -emit-sil -verify %s -disable-availability-checking

struct Box<Wrapped: ~Copyable>: ~Copyable {
    private let _pointer: UnsafeMutablePointer<Wrapped>
    
    init(_ element: consuming Wrapped) {
        _pointer = .allocate(capacity: 1)
        print("allocating",_pointer)
        _pointer.initialize(to: element)
    }
        
    deinit {
        print("deallocating",_pointer)
        _pointer.deinitialize(count: 1)
        _pointer.deallocate()
    }
    
    consuming func move() -> Wrapped {
        let wrapped = _pointer.move()
        print("deallocating", _pointer)
        _pointer.deallocate()
        discard self
        return wrapped
    }
}

enum List<Element: ~Copyable>: ~Copyable {
    case empty
    case node(Element, Box<List>)
}

extension List {
    init() { self = .empty }
    
    mutating func push(_ element: consuming Element) {
        self = .node(element, Box(self))
    }
        
    mutating func pop() -> Element {
        switch consume self {
        case .node(let element, let box):
            self = box.move()
            return element
        case .empty:
            fatalError()
        }
    }
    
    var isEmpty: Bool {
        switch self {
        case .empty: true
        case .node: false
        }
    }
}
