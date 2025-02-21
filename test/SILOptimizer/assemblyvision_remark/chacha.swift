// RUN: %target-swiftc_driver -Osize -emit-sil %s -o /dev/null -Xfrontend -verify
// REQUIRES: optimized_stdlib,swift_stdlib_no_asserts
// REQUIRES: swift_in_compiler

// An extraction from the benchmark ChaCha20 that we were not ignoring
// dealloc_stack and other end scope instructions.

enum ChaCha20 { }

extension ChaCha20 {
    @inline(never)
    public static func encrypt<Key: Collection, Nonce: Collection, Bytes: MutableCollection>(bytes: inout Bytes, key: Key, nonce: Nonce, initialCounter: UInt32 = 0) where Bytes.Element == UInt8, Key.Element == UInt8, Nonce.Element == UInt8 {
        print("I am lost...")
    }
}

@inline(never)
func checkResult(_ plaintext: [UInt8]) {
    precondition(plaintext.first! == 6 && plaintext.last! == 254)
    var hash: UInt64 = 0
    for byte in plaintext {
        // rotate
        hash = (hash &<< 8) | (hash &>> (64 - 8))
        hash ^= UInt64(byte)
    }
    precondition(hash == 0xa1bcdb217d8d14e4)
}

@_semantics("optremark.sil-assembly-vision-remark-gen")
public func run_ChaCha(_ N: Int) {
  let key = Array(repeating: UInt8(1), count: 32) // expected-note {{of 'key}}
  let nonce = Array(repeating: UInt8(2), count: 12) // expected-note {{of 'nonce}}

  var checkedtext = Array(repeating: UInt8(0), count: 1024) // expected-note {{of 'checkedtext}}
  ChaCha20.encrypt(bytes: &checkedtext, key: key, nonce: nonce)
  checkResult(checkedtext)


  var plaintext = Array(repeating: UInt8(0), count: 30720) // expected-note {{of 'plaintext}}
  for _ in 1...N {
    ChaCha20.encrypt(bytes: &plaintext, key: key, nonce: nonce)
    print(plaintext.first!) // expected-remark @:11 {{heap allocated ref of type '}}
                            // expected-remark @-1:27 {{release of type '}}
  }
} // expected-remark {{release of type '}}
  // expected-remark @-1 {{release of type '}}
  // expected-remark @-2 {{release of type '}}
  // expected-remark @-3 {{release of type '}}
