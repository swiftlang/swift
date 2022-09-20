// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/52838

// CHECK: sr10438.(file).AuthenticationFlowStateMachine@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[AuthenticationFlowStateMachine]FlowError.[AuthenticationFlowStateMachineFlowError]StateMachine, Self.[AuthenticationFlowStateMachine]FlowError : AuthenticationFlowStateMachineFlowError, Self.[AuthenticationFlowStateMachine]NonFinalState : AuthenticationFlowStateMachineNonFinalState, Self.[AuthenticationFlowStateMachine]StartState : AuthenticationFlowStateMachineStartState, Self.[AuthenticationFlowStateMachine]FlowError.[AuthenticationFlowStateMachineFlowError]StateMachine == Self.[AuthenticationFlowStateMachine]NonFinalState.[AuthenticationFlowStateMachineNonFinalState]StateMachine, Self.[AuthenticationFlowStateMachine]NonFinalState.[AuthenticationFlowStateMachineNonFinalState]StateMachine == Self.[AuthenticationFlowStateMachine]StartState.[AuthenticationFlowStateMachineStartState]StateMachine>
protocol AuthenticationFlowStateMachine {
    associatedtype StartState: AuthenticationFlowStateMachineStartState where StartState.StateMachine == Self
    associatedtype NonFinalState: AuthenticationFlowStateMachineNonFinalState where NonFinalState.StateMachine == Self
    associatedtype FlowError: AuthenticationFlowStateMachineFlowError where FlowError.StateMachine == Self
}

// CHECK: sr10438.(file).AuthenticationFlowStateMachineFlowError@
// CHECK-NEXT: Requirement signature: <Self where Self : Error, Self == Self.[AuthenticationFlowStateMachineFlowError]StateMachine.[AuthenticationFlowStateMachine]FlowError, Self.[AuthenticationFlowStateMachineFlowError]StateMachine : AuthenticationFlowStateMachine>
protocol AuthenticationFlowStateMachineFlowError: Error {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.FlowError == Self
}

// CHECK: sr10438.(file).AuthenticationFlowStateMachineStartState@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[AuthenticationFlowStateMachineStartState]StateMachine.[AuthenticationFlowStateMachine]StartState, Self.[AuthenticationFlowStateMachineStartState]StateMachine : AuthenticationFlowStateMachine>
protocol AuthenticationFlowStateMachineStartState {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.StartState == Self
    var nonFinalState: StateMachine.NonFinalState { get }
}

// CHECK: sr10438.(file).AuthenticationFlowStateMachineNonFinalState@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[AuthenticationFlowStateMachineNonFinalState]StateMachine.[AuthenticationFlowStateMachine]NonFinalState, Self.[AuthenticationFlowStateMachineNonFinalState]StateMachine : AuthenticationFlowStateMachine>
protocol AuthenticationFlowStateMachineNonFinalState {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.NonFinalState == Self
}
