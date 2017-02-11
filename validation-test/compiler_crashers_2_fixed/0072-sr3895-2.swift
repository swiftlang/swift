// RUN: %target-swift-frontend %s -emit-ir

public class TiPresenter<V> {
    
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


public class TiViewController<P: TiPresenter<V>, V>
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


protocol MyView {
    func setDataItems(_: [String])
}

class MyPresenter: TiPresenter<MyView> {
}


class MyController: TiViewController<MyPresenter, MyView>, MyView {

    
    override func providePresenter() -> MyPresenter {
        return MyPresenter()
    }
    
    internal func setDataItems(_: [String]) {
        //TODO
    }
}

let vc = MyController()
let p = vc.presenter
