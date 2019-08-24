// RUN: not %target-swift-frontend %s -typecheck

public protocol TiView {
    
}

public class TiPresenter<V: TiView> {
    
    private var view: V? = nil
    
    public func create() {
        
    }
    
    public func attchView(view: V) {
        self.view = view
    }
    
    public func detachView() {
        self.view = nil
    }
    
    public func destroy() {
        
    }
}


public class TiViewController<P: TiPresenter<V>, V: TiView>
    /*: UiViewController*/ // should extend UiViewController but this is not problem here
{
    
    lazy var presenter: P = {
        return self.providePresenter()
    }()
    
    public init() {
        presenter.create()
    }
    
    func providePresenter() -> P {
        fatalError("must override")
    }
    
    func provideView() -> V {
        if (self is V) {
            return self as! V
        } else {
            fatalError("UIViewController doesn't implement TiView interface")
        }
    }
}


protocol MyView: TiView {
    func setDataItems(_: [String])
}

class MyPresenter: TiPresenter<MyView> { //ERROR: Using 'MyView' as a concrete type conforming to protocol 'TiView' is not supported
    
}


class MyController: TiViewController<MyPresenter, MyView>, MyView { // ERROR: 'TiViewController' requires that 'MyPresenter' inherit from 'TiPresenter<MyView>'

    
    override func providePresenter() -> MyPresenter {
        return MyPresenter()
    }
    
    internal func setDataItems(_: [String]) {
        //TODO
    }
}

let vc = MyController()
let p = vc.presenter

class TestView: MyView {
    internal func setDataItems(_: [String]) {
        // TODO
    }
}
let view = TestView()

p.attchView(view: view)
