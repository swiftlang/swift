class StateAnimatorGroup<State> {
    func add<T: StateAnimator>(_ animator: T) where T.State == State {}
    func set(_ state: State) {}
}

extension StateAnimatorGroup: StateAnimator where State == String {}

protocol StateAnimator {
    associatedtype State

    func set(_ state: State)
}