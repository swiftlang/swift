// RUN: %target-swift-frontend %s -emit-ir

enum SampleType: UInt8 {
    case Value = 1
    case Array
    case Dictionary
}

protocol ByteConvertible {
    init?(bytes: [UInt8], startOffset: Int)
    func rawBytes() -> [UInt8]
    func bytesNeeded() -> Int
}

protocol TypeRequestable {
    static func containerType() -> SampleType
}

struct Sample<T, C> where T: ByteConvertible, C: TypeRequestable {
    let numberOfRecords: UInt32
    let sizeInBytes: UInt64
    var records: [T]  = [] // problem line

    init(records: [T]) {
        numberOfRecords = 0
        sizeInBytes = 0

        self.records.reserveCapacity(records.count)
        self.records += records
    }
}

extension Sample: ByteConvertible {
    init?(bytes: [UInt8], startOffset: Int = 0) {
        numberOfRecords = 0
        sizeInBytes = 0
        records = []
    }

    func rawBytes() -> [UInt8] {
        return []
    }

    func bytesNeeded() -> Int {
        return 0
    }
}
