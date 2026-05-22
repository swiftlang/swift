// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/isolated_conformance_other.swift -module-name ExternalTypes -o %t/ExternalTypes.swiftmodule
// RUN: %target-swift-frontend -emit-sil -swift-version 5 -strict-concurrency=complete -I %t %s -verify -verify-additional-prefix swift5- -enable-upcoming-feature InferIsolatedConformances -o /dev/null
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -I %t %s -verify -verify-additional-prefix swift6- -enable-upcoming-feature InferIsolatedConformances -o /dev/null

// REQUIRES: concurrency

@preconcurrency import ExternalTypes

////////////////////////
// MARK: Declarations //
////////////////////////

protocol NonSendableProtocol {}
protocol NonSendableKlassProtocol : AnyObject {}
class NonSendableKlass {} // expected-note 4{{class 'NonSendableKlass' does not conform to the 'Sendable' protocol}}

protocol Command: Sendable {}

enum SpecificCommand: Command, @unchecked Sendable {
    case myCommand(any MyObject & MyCommand)
    case text(String)
}

typealias ClientBound = BaseCommand & ClientBoundCommand
typealias ServerBound = BaseCommand & ServerBoundCommand

protocol CommandPerformer: Sendable {
    func performCommand(_ command: any Command) async throws
    func canPerformCommand(_ command: any Command) async -> Bool
}

protocol ServiceItemProvider: Sendable {}

protocol Connection: Actor, ServiceItemProvider, CommandPerformer {
    var isActive: Bool { get }
    func submit(request: String) async throws
    func deactivate() async
}

/////////////////
// MARK: Tests //
/////////////////

actor MyConnection: Connection {
    var isActive: Bool { false }

    func submit(request: String) async throws {}
    func deactivate() async {}

    func canPerformCommand(_ command: any Command) async -> Bool {
        return command is SpecificCommand
    }

    func performCommand(_ command: any Command) async throws {
        guard await canPerformCommand(command) else { return }
        guard let specific = command as? SpecificCommand else { return }

        switch specific {
        case .myCommand(let myCommand):
            var commandToSend: ServerBound?

            if let clientBoundCommand = myCommand as? ClientBound {
                commandToSend = await handleClientBound(clientBoundCommand)
            } else if let serverBoundCommand = myCommand as? ServerBound {
                commandToSend = serverBoundCommand
            }

            if let commandToSend {
                send(commandToSend)
            }

        case .text(let prompt):
            try await submit(request: prompt)
        }
    }

    private func handleClientBound(_ command: any ClientBound) async -> ServerBound? {
        return nil
    }

    private func send(_ command: ServerBound) {}
}

actor CastTest<T, U : NonSendableProtocol, V : AnyObject, V2 : NonSendableKlassProtocol> { // expected-note 8{{}}
  @MainActor var cnx: NonSendableKlass?
  @MainActor var cnx2: T?
  @MainActor var cnx3: U?
  @MainActor var cnx4: V?
  @MainActor var cnx5: V2?

  func f() async -> NonSendableProtocol {
    guard let c = await cnx else { fatalError() } // expected-swift5-warning {{non-Sendable type 'NonSendableKlass?' of property 'cnx' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'NonSendableKlass?' of property 'cnx' cannot exit main actor-isolated context}}
    guard let q = c as? NonSendableProtocol else { fatalError() } // expected-swift5-warning {{casting main actor-isolated 'c' to 'self'-isolated 'any NonSendableProtocol' risks causing data races; this is an error in the Swift 6 language mode}}
    return q // expected-swift5-warning {{assigning main actor-isolated 'q' to 'self'-isolated '$return_value' risks causing data races}}
    // expected-swift5-note @-1 {{'q' could become accessible to 'self'-isolated code despite remaining accessible to main actor-isolated code}}
  }

  func f2() async -> NonSendableProtocol {
    guard let c = await cnx2 else { fatalError() } // expected-swift5-warning {{non-Sendable type 'T?' of property 'cnx2' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'T?' of property 'cnx2' cannot exit main actor-isolated context}}
    guard let q = c as? NonSendableProtocol else { fatalError() } // expected-swift5-warning {{casting main actor-isolated 'c' to 'self'-isolated 'any NonSendableProtocol' risks causing data races; this is an error in the Swift 6 language mode}}
    return q // expected-swift5-warning {{assigning main actor-isolated 'q' to 'self'-isolated '$return_value' risks causing data races}}
    // expected-swift5-note @-1 {{'q' could become accessible to 'self'-isolated code despite remaining accessible to main actor-isolated code}}
  }

  func f3() async -> NonSendableProtocol {
    guard let c = await cnx3 else { fatalError() } // expected-swift5-warning {{non-Sendable type 'U?' of property 'cnx3' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'U?' of property 'cnx3' cannot exit main actor-isolated context}}
    guard let q = c as? NonSendableProtocol else { fatalError() } // expected-warning {{conditional cast from 'U' to 'any NonSendableProtocol' always succeeds}}
    return q // expected-swift5-warning {{assigning main actor-isolated 'q' to 'self'-isolated '$return_value' risks causing data races; this is an error in the Swift 6 language mode}}
    // expected-swift5-note @-1 {{'q' could become accessible to 'self'-isolated code despite remaining accessible to main actor-isolated code}}
  }

  func g() async -> NonSendableProtocol {
    guard let c = await cnx else { fatalError() } // expected-swift5-warning {{non-Sendable type 'NonSendableKlass?' of property 'cnx' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'NonSendableKlass?' of property 'cnx' cannot exit main actor-isolated context}}
    let q = c as! NonSendableProtocol // expected-swift5-warning {{assigning main actor-isolated 'c' to 'self'-isolated 'q' risks causing data races}}
    // expected-swift5-note @-1 {{'c' could become accessible to 'self'-isolated code despite remaining accessible to main actor-isolated code}}
    return q
  }

  func g2() async -> NonSendableProtocol {
    guard let c = await cnx2 else { fatalError() } // expected-swift5-warning {{non-Sendable type 'T?' of property 'cnx2' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'T?' of property 'cnx2' cannot exit main actor-isolated context}}
    let q = c as! NonSendableProtocol // expected-swift5-warning {{assigning main actor-isolated 'c' to 'self'-isolated 'q' risks causing data races}}
    // expected-swift5-note @-1 {{'c' could become accessible to 'self'-isolated code despite remaining accessible to main actor-isolated code}}
    return q
  }

  func g3() async -> NonSendableProtocol {
    guard let c = await cnx3 else { fatalError() } // expected-swift5-warning {{non-Sendable type 'U?' of property 'cnx3' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'U?' of property 'cnx3' cannot exit main actor-isolated context}}
    let q = c as! NonSendableProtocol // expected-warning {{forced cast from 'U' to 'any NonSendableProtocol' always succeeds; did you mean to use 'as'}}
    return q // expected-swift5-warning {{assigning main actor-isolated 'q' to 'self'-isolated '$return_value' risks causing data races; this is an error in the Swift 6 language mode}}
    // expected-swift5-note @-1 {{'q' could become accessible to 'self'-isolated code despite remaining accessible to main actor-isolated code}}
  }


  func h() async -> NonSendableKlassProtocol {
    guard let c = await cnx else { fatalError() } // expected-swift5-warning {{non-Sendable type 'NonSendableKlass?' of property 'cnx' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'NonSendableKlass?' of property 'cnx' cannot exit main actor-isolated context}}
    guard let q = c as? NonSendableKlassProtocol else { fatalError() } // expected-swift5-warning {{casting main actor-isolated 'c' to 'self'-isolated 'any NonSendableKlassProtocol' risks causing data races}}
    return q
  }

  func h2() async -> NonSendableKlassProtocol {
    guard let c = await cnx4 else { fatalError() } // expected-swift5-warning {{non-Sendable type 'V?' of property 'cnx4' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'V?' of property 'cnx4' cannot exit main actor-isolated context}}
    guard let q = c as? NonSendableKlassProtocol else { fatalError() } // expected-swift5-warning {{casting main actor-isolated 'c' to 'self'-isolated 'any NonSendableKlassProtocol' risks causing data races}}
    return q
  }

  func h3() async -> NonSendableKlassProtocol {
    guard let c = await cnx5 else { fatalError() } // expected-swift5-warning {{non-Sendable type 'V2?' of property 'cnx5' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'V2?' of property 'cnx5' cannot exit main actor-isolated context}}
    guard let q = c as? NonSendableKlassProtocol else { fatalError() } // expected-warning {{conditional cast from 'V2' to 'any NonSendableKlassProtocol' always succeeds}}
    return q
  }

  func k() async -> NonSendableKlassProtocol {
    guard let c = await cnx else { fatalError() } // expected-swift5-warning {{non-Sendable type 'NonSendableKlass?' of property 'cnx' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'NonSendableKlass?' of property 'cnx' cannot exit main actor-isolated context}}
    let q = c as! NonSendableKlassProtocol // expected-swift5-warning {{assigning main actor-isolated 'c' to 'self'-isolated 'q' risks causing data races}}
    // expected-swift5-note @-1 {{'c' could become accessible to 'self'-isolated code despite remaining accessible to main actor-isolated code}}
    // expected-swift5-warning @-2 {{casting main actor-isolated 'c' to 'self'-isolated 'any NonSendableKlassProtocol' risks causing data races}}
    return q
  }

  func k2() async -> NonSendableKlassProtocol {
    guard let c = await cnx4 else { fatalError() } // expected-swift5-warning {{non-Sendable type 'V?' of property 'cnx4' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'V?' of property 'cnx4' cannot exit main actor-isolated context}}
    let q = c as! NonSendableKlassProtocol // expected-swift5-warning {{assigning main actor-isolated 'c' to 'self'-isolated 'q' risks causing data races}}
    // expected-swift5-note @-1 {{'c' could become accessible to 'self'-isolated code despite remaining accessible to main actor-isolated code}}
    // expected-swift5-warning @-2 {{casting main actor-isolated 'c' to 'self'-isolated 'any NonSendableKlassProtocol' risks causing data races}}
    return q
  }

  func k3() async -> NonSendableKlassProtocol {
    guard let c = await cnx5 else { fatalError() } // expected-swift5-warning {{non-Sendable type 'V2?' of property 'cnx5' cannot exit main actor-isolated context}}
    // expected-swift6-error @-1 {{non-Sendable type 'V2?' of property 'cnx5' cannot exit main actor-isolated context}}
    let q = c as! NonSendableKlassProtocol // expected-warning {{forced cast from 'V2' to 'any NonSendableKlassProtocol' always succeeds}}
    return q
  }

}
