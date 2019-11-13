// RUN: %target-swift-frontend -typecheck %s

protocol AuthenticationFlowStateMachine {
    associatedtype StartState: AuthenticationFlowStateMachineStartState where StartState.StateMachine == Self
    associatedtype NonFinalState: AuthenticationFlowStateMachineNonFinalState where NonFinalState.StateMachine == Self
    associatedtype FlowError: AuthenticationFlowStateMachineFlowError where FlowError.StateMachine == Self
}

protocol AuthenticationFlowStateMachineFlowError: Error {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.FlowError == Self
}

protocol AuthenticationFlowStateMachineStartState {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.StartState == Self
    var nonFinalState: StateMachine.NonFinalState { get }
}

protocol AuthenticationFlowStateMachineNonFinalState {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.NonFinalState == Self
}
