// RUN: %target-swift-frontend -emit-ir %s

protocol Dismissable {
    func dismiss(completion: @escaping () -> Void)
}

typealias Completion = (Dismissable?) -> Void

protocol Cancelable: AnyObject {
    func cancel()
}

protocol Controller {
    func asyncThing(completion: @escaping ((_ error: Error) -> Void)) -> Cancelable
}

public struct Message: Equatable {
    public static let `default` = Message()
    
    public init() {
    }

    public init(error: Error) {
        self = .default
    }
}

struct PresentAlert {
    let message: Message
}

class Manager {
    private let controller: Controller
    
    init(controller: Controller) {
        self.controller = controller
    }
    
    func present() {
        let _: Completion = { (dismissable: Dismissable?) in
            dismissable?.dismiss { [weak self] in
                guard let sself = self else { return }
                
                sself.controller.asyncThing { error in
                    let backupMessage = Message()
                    let mainMessage = Message(error: error)
                    let finalMessage = mainMessage != Message.default ? mainMessage : backupMessage
                    
                    _ = PresentAlert(message: finalMessage)
                }.cancel()
            }
        }
    }
}
