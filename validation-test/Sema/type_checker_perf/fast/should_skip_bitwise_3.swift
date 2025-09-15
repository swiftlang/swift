// RUN: %target-swift-frontend -typecheck %s -solver-scope-threshold=15000

// REQUIRES: tools-release,no_asan

// Reduced from https://github.com/leif-ibsen/SwiftHPKE

struct Field25519 {
    static let mask51 = UInt64(0x7ffffffffffff)
    
    var l0: UInt64
    var l1: UInt64
    var l2: UInt64
    var l3: UInt64
    var l4: UInt64

    init(_ b: [UInt8]) {
        self.l0 = UInt64(b[0]) | UInt64(b[1]) << 8 | UInt64(b[2]) << 16 | UInt64(b[3]) << 24 | UInt64(b[4]) << 32 | UInt64(b[5]) << 40 | UInt64(b[6]) << 48 | UInt64(b[7]) << 56
        self.l1 = UInt64(b[6]) | UInt64(b[7]) << 8 | UInt64(b[8]) << 16 | UInt64(b[9]) << 24 | UInt64(b[10]) << 32 | UInt64(b[11]) << 40 | UInt64(b[12]) << 48 | UInt64(b[13]) << 56
        self.l2 = UInt64(b[12]) | UInt64(b[13]) << 8 | UInt64(b[14]) << 16 | UInt64(b[15]) << 24 | UInt64(b[16]) << 32 | UInt64(b[17]) << 40 | UInt64(b[18]) << 48 | UInt64(b[19]) << 56
        self.l3 = UInt64(b[19]) | UInt64(b[20]) << 8 | UInt64(b[21]) << 16 | UInt64(b[22]) << 24 | UInt64(b[23]) << 32 | UInt64(b[24]) << 40 | UInt64(b[25]) << 48 | UInt64(b[26]) << 56
        self.l4 = UInt64(b[24]) | UInt64(b[25]) << 8 | UInt64(b[26]) << 16 | UInt64(b[27]) << 24 | UInt64(b[28]) << 32 | UInt64(b[29]) << 40 | UInt64(b[30]) << 48 | UInt64(b[31]) << 56
    }
}
