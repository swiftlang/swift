// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: sr10438.(file).AuthenticationFlowStateMachine@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.FlowError.StateMachine, Self.FlowError : AuthenticationFlowStateMachineFlowError, Self.NonFinalState : AuthenticationFlowStateMachineNonFinalState, Self.StartState : AuthenticationFlowStateMachineStartState, Self.FlowError.StateMachine == Self.NonFinalState.StateMachine, Self.NonFinalState.StateMachine == Self.StartState.StateMachine>
protocol AuthenticationFlowStateMachine {
    associatedtype StartState: AuthenticationFlowStateMachineStartState where StartState.StateMachine == Self
    associatedtype NonFinalState: AuthenticationFlowStateMachineNonFinalState where NonFinalState.StateMachine == Self
    associatedtype FlowError: AuthenticationFlowStateMachineFlowError where FlowError.StateMachine == Self
}

// CHECK: sr10438.(file).AuthenticationFlowStateMachineFlowError@
// CHECK-NEXT: Requirement signature: <Self where Self : Error, Self == Self.StateMachine.FlowError, Self.StateMachine : AuthenticationFlowStateMachine>
protocol AuthenticationFlowStateMachineFlowError: Error {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.FlowError == Self
}

// CHECK: sr10438.(file).AuthenticationFlowStateMachineStartState@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.StateMachine.StartState, Self.StateMachine : AuthenticationFlowStateMachine>
protocol AuthenticationFlowStateMachineStartState {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.StartState == Self
    var nonFinalState: StateMachine.NonFinalState { get }
}

// CHECK: sr10438.(file).AuthenticationFlowStateMachineNonFinalState@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.StateMachine.NonFinalState, Self.StateMachine : AuthenticationFlowStateMachine>
protocol AuthenticationFlowStateMachineNonFinalState {
    associatedtype StateMachine: AuthenticationFlowStateMachine where StateMachine.NonFinalState == Self
}
