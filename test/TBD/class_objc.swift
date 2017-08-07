// RUN: %target-swift-frontend -emit-ir -o- -parse-as-library -module-name test -import-objc-header %S/Inputs/objc_class_header.h -validate-tbd-against-ir=all %s

// REQUIRES: objc_interop

import Foundation

public class PublicEmpty: NSObject {}

public class PublicSubPublicEmpty: PublicEmpty {}
internal class InternalSubPublicEmpty: PublicEmpty {}
private class PrivateSubPublicEmpty: PublicEmpty {}


internal class InternalEmpty: NSObject {}

internal class InternalSubInternalEmpty: InternalEmpty {}
private class PrivateSubInternalEmpty: InternalEmpty {}


private class PrivateEmpty: NSObject {}

private class PrivateSubPrivateEmpty: PrivateEmpty {}


public class PublicInheritObjCProtocol: NSObject, ObjCProtocol {}

internal class InternalInheritObjCProtocol: NSObject, ObjCProtocol {}

private class PrivateInheritObjCProtocol: NSObject, ObjCProtocol {}
