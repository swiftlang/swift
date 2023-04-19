// RUN: %empty-directory(%t)

// REQUIRES: asserts

// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-deserialization-safety \
// RUN:   -Xllvm -debug-only=Serialization 2>&1 \
// RUN:   | %FileCheck --check-prefixes=SAFETY-PRIVATE,SAFETY-INTERNAL %s

// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-deserialization-safety \
// RUN:   -Xllvm -debug-only=Serialization \
// RUN:   -enable-testing 2>&1 \
// RUN:   | %FileCheck --check-prefixes=DISABLED %s

/// Don't mark decls as unsafe when private import is enabled.
// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -enable-library-evolution -swift-version 5 \
// RUN:   -enable-deserialization-safety \
// RUN:   -Xllvm -debug-only=Serialization \
// RUN:   -enable-private-imports 2>&1 \
// RUN:   | %FileCheck --check-prefixes=DISABLED %s

/// Don't mark decls as unsafe without library evolution.
// RUN: %target-swift-frontend -emit-module %s \
// RUN:   -enable-deserialization-safety -swift-version 5 \
// RUN:   -Xllvm -debug-only=Serialization 2>&1 \
// RUN:   | %FileCheck --check-prefixes=DISABLED %s

// DISABLED-NOT: Serialization safety

/// Public
// SAFETY-PRIVATE: Serialization safety, safe: 'PublicProto'
public protocol PublicProto {}
public struct ExtendedPublic {
// SAFETY-PRIVATE: Serialization safety, safe: 'ExtendedPublic'
}
extension ExtendedPublic {
// SAFETY-PRIVATE: Serialization safety, safe: 'extension ExtendedPublic'
    public func publicMember() {}
}
extension ExtendedPublic : PublicProto {
// SAFETY-PRIVATE: Serialization safety, safe: 'extension ExtendedPublic'
}

/// Internal
internal protocol InternalProto {}
// SAFETY-INTERNAL: Serialization safety, safe: 'InternalProto'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'InternalProto'
internal struct ExtendedInternal {}
// SAFETY-INTERNAL: Serialization safety, unsafe: 'ExtendedInternal'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'ExtendedInternal'
extension ExtendedInternal {
// SAFETY-INTERNAL: Serialization safety, unsafe: 'extension ExtendedInternal'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'extension ExtendedInternal'
    internal func internalMember() {}
}
extension ExtendedInternal : InternalProto {}
// SAFETY-INTERNAL: Serialization safety, unsafe: 'extension ExtendedInternal'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'extension ExtendedInternal'

/// Private
private protocol PrivateProto {}
// SAFETY-PRIVATE: Serialization safety, safe: 'PrivateProto'
private struct ExtendedPrivate {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'ExtendedPrivate'
extension ExtendedPrivate {
// SAFETY-PRIVATE: Serialization safety, unsafe: 'extension ExtendedPrivate'
    private func privateMember() {}
}
extension ExtendedPrivate : PrivateProto {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'extension ExtendedPrivate'

/// Fileprivate
private protocol FileprivateProto {}
// SAFETY-PRIVATE: Serialization safety, safe: 'FileprivateProto'
private struct ExtendedFileprivate {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'ExtendedFileprivate'
extension ExtendedFileprivate {
// SAFETY-PRIVATE: Serialization safety, unsafe: 'extension ExtendedFileprivate'
    private func fileprivateMember() {}
}
extension ExtendedFileprivate : FileprivateProto {}
// SAFETY-PRIVATE: Serialization safety, unsafe: 'extension ExtendedFileprivate'

/// Back to public
extension ExtendedPublic : InternalProto {
// SAFETY-INTERNAL: Serialization safety, safe: 'extension ExtendedPublic'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'extension ExtendedPublic'
}
extension ExtendedPublic : PrivateProto {
// SAFETY-PRIVATE: Serialization safety, safe: 'extension ExtendedPublic'
}
extension ExtendedPublic : FileprivateProto {
// SAFETY-PRIVATE: Serialization safety, safe: 'extension ExtendedPublic'
}

extension ExtendedPublic {
// SAFETY-INTERNAL: Serialization safety, unsafe: 'extension ExtendedPublic'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'extension ExtendedPublic'
    internal func internalMemberToPublic() {}
}

extension ExtendedPublic {
// SAFETY-PRIVATE: Serialization safety, unsafe: 'extension ExtendedPublic'
    private func privateMemberToPublic() {}
}

extension ExtendedPublic {
// SAFETY-PRIVATE: Serialization safety, unsafe: 'extension ExtendedPublic'
    fileprivate func fileprivateMemberToPublic() {}
}

extension ExtendedInternal {
// SAFETY-PRIVATE: Serialization safety, unsafe: 'extension ExtendedInternal'
    private func privateMemberToInternal() {}
}

extension ExtendedInternal {
// SAFETY-PRIVATE: Serialization safety, unsafe: 'extension ExtendedInternal'
    fileprivate func fileprivateMemberToInternal() {}
}

/// Members are serialized last
// SAFETY-INTERNAL: Serialization safety, unsafe: 'internalMember()'
// SAFETY-PRIVATE: Serialization safety, unsafe: 'privateMember()'
// SAFETY-PRIVATE: Serialization safety, unsafe: 'fileprivateMember()'

// SAFETY-INTERNAL: Serialization safety, unsafe: 'internalMemberToPublic()'
// NO-SAFETY-INTERNAL: Serialization safety, safe: 'internalMemberToPublic()'
// SAFETY-PRIVATE: Serialization safety, unsafe: 'privateMemberToPublic()'
// SAFETY-PRIVATE: Serialization safety, unsafe: 'fileprivateMemberToPublic()'

// SAFETY-PRIVATE: Serialization safety, unsafe: 'privateMemberToInternal()'
// SAFETY-PRIVATE: Serialization safety, unsafe: 'fileprivateMemberToInternal()'
