// RUN: %target-typecheck-verify-swift -solver-scope-threshold=3000

class RequestRecord: Record {
    var urlNoQuery: String
    var method: String?
    var hash: String
    var instance: Int64

    required init(row: Row) { fatalError() }
}

extension Database {
    func record(tmp: RequestRecord) {
        // The formerly slow expression
        let _ = RequestRecord.filter(
                Column("") == tmp.hash
                && Column("") == tmp.instance
                && Column("") == tmp.urlNoQuery
                && Column("") == tmp.method
                && Column("") == tmp.method
                && Column("") == tmp.method
            )
            .fetchOne(self)
    }
}

// The rest was reduced from https://github.com/groue/GRDB.swift

struct Database {}

class Record {}

extension Record: TableRecord { }
extension Record: FetchableRecord { }

protocol TableRecord {}

protocol DatabaseValueConvertible {}

protocol StatementColumnConvertible {}

protocol FetchableRecord {}

struct Row {}

protocol FetchRequest {
    associatedtype RowDecoder
}

extension FetchRequest where Self.RowDecoder : DatabaseValueConvertible {
    func fetchOne(_: Database) -> Self.RowDecoder? { fatalError() }
}

extension FetchRequest where Self.RowDecoder : DatabaseValueConvertible & StatementColumnConvertible {
    func fetchOne(_: Database) -> Self.RowDecoder? { fatalError() }
}

extension FetchRequest where Self.RowDecoder : FetchableRecord {
    func fetchOne(_: Database) -> Self.RowDecoder? { fatalError() }
}

extension FetchRequest where Self.RowDecoder == Row {
    func fetchOne(_: Database) -> Row? { fatalError() }
}

struct QueryInterfaceRequest<RowDecoder>: FetchRequest {
}

extension TableRecord {
    static func filter(_ predicate: some SQLSpecificExpressible) -> QueryInterfaceRequest<Self> {
        fatalError()
    }
}

struct Request {}

struct Column: Sendable {
    init(_ name: String) {}
    init(_ codingKey: some CodingKey) {}
}

protocol SQLExpressible {}

extension Column: SQLSpecificExpressible {}

protocol SQLSpecificExpressible: SQLExpressible {}

struct SQLExpression: SQLSpecificExpressible {}

extension SQLSpecificExpressible {
    static func && (lhs: Self, rhs: some SQLExpressible) -> SQLExpression { fatalError() }
    static func && (lhs: some SQLExpressible, rhs: Self) -> SQLExpression { fatalError() }
    static func && (lhs: Self, rhs: some SQLSpecificExpressible) -> SQLExpression { fatalError() }
    static func == (lhs: Self, rhs: (any SQLExpressible)?) -> SQLExpression { fatalError() }
    static func == (lhs: Self, rhs: Bool) -> SQLExpression { fatalError() }
    static func == (lhs: (any SQLExpressible)?, rhs: Self) -> SQLExpression { fatalError() }
    static func == (lhs: Bool, rhs: Self) -> SQLExpression { fatalError() }
    static func == (lhs: Self, rhs: some SQLSpecificExpressible) -> SQLExpression { fatalError() }
}

struct AssociationAggregate<RowDecoder> {
    static func && (lhs: Self, rhs: Self) -> Self { fatalError()}
    static func && (lhs: Self, rhs: some SQLExpressible) -> Self { fatalError()}
    static func && (lhs: some SQLExpressible, rhs: Self) -> Self { fatalError()}
    static func == (lhs: Self, rhs: Self) -> Self { fatalError() }
    static func == (lhs: Self, rhs: (any SQLExpressible)?) -> Self { fatalError()}
    static func == (lhs: (any SQLExpressible)?, rhs: Self) -> Self { fatalError()}
    static func == (lhs: Self, rhs: Bool) -> Self { fatalError()}
    static func == (lhs: Bool, rhs: Self) -> Self { fatalError()}
}

struct Data {}

extension Data: SQLExpressible {}
extension Int64: SQLExpressible {}
extension String: SQLExpressible {}