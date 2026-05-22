// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/ExternalTypes.swift -module-name ExternalTypes -o %t/ExternalTypes.swiftmodule
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -I %t %t/test.swift

// REQUIRES: concurrency

//--- ExternalTypes.swift

// ExternalTypes module - simulates non-Sendable ObjC-like protocols
// from a framework imported with @preconcurrency

public protocol MyObject: AnyObject {}
public protocol MyCommand: AnyObject {}
public protocol ClientBoundCommand: AnyObject {}
public protocol ServerBoundCommand: AnyObject {}
public protocol BaseCommand: AnyObject {}

//--- test.swift

@preconcurrency import ExternalTypes

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

// MARK: - Crashing actor

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
