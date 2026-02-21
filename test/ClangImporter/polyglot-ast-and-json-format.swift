/// Test -print-polyglot-ast

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Parse header
// RUN: %target-swift-ide-test -source-filename %t/MyClass.m -print-polyglot-ast \
// RUN:   -header-to-print %t/MyClass.h \
// RUN:   --cc-args %target-cc-options %t/MyClass.h \
// RUN:   -isysroot %clang-importer-sdk-path \
// RUN:   > %t/ast.json
// RUN: %validate-json %t/ast.json > /dev/null
// RUN: %FileCheck %t/MyClass.h --input-file %t/ast.json

/// Parse implementation file
// RUN: %target-swift-ide-test -source-filename %t/MyClass.m -print-polyglot-ast \
// RUN:   -header-to-print %t/MyClass.m \
// RUN:   --cc-args %target-cc-options %t/MyClass.m \
// RUN:   -isysroot %clang-importer-sdk-path \
// RUN:   > %t/ast.json
// RUN: %validate-json %t/ast.json > /dev/null
// RUN: %FileCheck %t/MyClass.m --input-file %t/ast.json

// REQUIRES: objc_interop

//--- MyClass.h
// CHECK: {
// CHECK-NEXT:   "formatVersion": 1,
// CHECK-NEXT:   "compilerVersion": "{{.*}}",
// CHECK-NEXT:   "topLevelDecls": [
@import Foundation;
// CHECK-NEXT:     {
// CHECK-NEXT:       "formatKind": "import",
// CHECK-NEXT:       "target": "Foundation"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "formatKind": "unhandled",
// CHECK-NEXT:       "debug": "Unhandled kind",
// CHECK-NEXT:       "fullText": "@import Foundation"
// CHECK-NEXT:     },

#pragma clang assume_nonnull begin

@interface MyClass : NSObject
// CHECK-NEXT:     {
// CHECK-NEXT:       "declKind": "ObjCInterface",
// CHECK-NEXT:       "diagName": "MyClass",
// CHECK-NEXT:       "formatKind": "container",
// CHECK-NEXT:       "className": "MyClass",
// CHECK-NEXT:       "properties": [

@property (nonatomic, strong, readonly) NSString *text;
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCProperty",
// CHECK-NEXT:           "diagName": "MyClass::text",
// CHECK-NEXT:           "formatKind": "property",
// CHECK-NEXT:           "swiftSignature": "open var text: String"
// CHECK-NEXT:         },
@property (nonatomic, readonly) NSInteger length;
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCProperty",
// CHECK-NEXT:           "diagName": "MyClass::length",
// CHECK-NEXT:           "formatKind": "property",
// CHECK-NEXT:           "swiftSignature": "open var length: Int"
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       "methods": [

- (instancetype)initWithText:(NSString *)text length:(NSInteger)length;
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCMethod",
// CHECK-NEXT:           "diagName": "MyClass::initWithText:length:",
// CHECK-NEXT:           "formatKind": "method",
// CHECK-NEXT:           "selector": "initWithText:length:",
// CHECK-NEXT:           "body": null,
// CHECK-NEXT:           "debug": "note: getClassInterface path",
// CHECK-NEXT:           "swiftSignature": "\n    @objc(initWithText:length:)\n    open init(text: String, length: Int) {\n        <#code#>\n    }\n"
// CHECK-NEXT:         },
+ (BOOL)supportsSecureCoding;
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCMethod",
// CHECK-NEXT:           "diagName": "MyClass::supportsSecureCoding",
// CHECK-NEXT:           "formatKind": "method",
// CHECK-NEXT:           "selector": "supportsSecureCoding",
// CHECK-NEXT:           "body": null,
// CHECK-NEXT:           "debug": "note: getClassInterface path",
// CHECK-NEXT:           "swiftSignature": "\n    @objc(supportsSecureCoding)\n    open class func supportsSecureCoding() -> {{.*}} {\n        <#code#>\n    }\n"
// CHECK-NEXT:         }
// CHECK-NEXT:       ]
// CHECK-NEXT:     }

@end

#pragma clang assume_nonnull end
// CHECK-NEXT:   ]
// CHECK-NEXT: }

//--- MyClass.m
// CHECK: {
// CHECK-NEXT:   "formatVersion": 1,
// CHECK-NEXT:   "compilerVersion": "{{.*}}",
// CHECK-NEXT:   "topLevelDecls": [

#import "MyClass.h"
#import <objc/NSObject.h>
// CHECK-NEXT:     {
// CHECK-NEXT:       "formatKind": "import",
// CHECK-NEXT:       "target": "Foundation"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "formatKind": "import",
// CHECK-NEXT:       "target": "ObjectiveC"
// CHECK-NEXT:     },

#pragma clang assume_nonnull begin

static NSString * const kMyClassText = @"text";
// CHECK-NEXT:     {
// CHECK-NEXT:       "formatKind": "unhandled",
// CHECK-NEXT:       "debug": "Unhandled kind",
// CHECK-NEXT:       "fullText": "static NSString * const kMyClassText = @\"text\""
// CHECK-NEXT:     },
static NSString * const kMyClassLength = @"length";
// CHECK-NEXT:     {
// CHECK-NEXT:       "formatKind": "unhandled",
// CHECK-NEXT:       "debug": "Unhandled kind",
// CHECK-NEXT:       "fullText": "static NSString * const kMyClassLength = @\"length\""
// CHECK-NEXT:     },

@interface MyClass ()
// CHECK-NEXT:     {
// CHECK-NEXT:       "declKind": "ObjCCategory",
// CHECK-NEXT:       "diagName": "(anonymous)",
// CHECK-NEXT:       "formatKind": "container",
// CHECK-NEXT:       "className": "MyClass",
// CHECK-NEXT:       "properties": [
@property (nonatomic, strong, readwrite) NSString *text;
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCProperty",
// CHECK-NEXT:           "diagName": "MyClass::text",
// CHECK-NEXT:           "formatKind": "property",
// CHECK-NEXT:           "swiftSignature": "open var text: String"
// CHECK-NEXT:         },
@property (nonatomic, assign, readwrite) NSInteger length;
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCProperty",
// CHECK-NEXT:           "diagName": "MyClass::length",
// CHECK-NEXT:           "formatKind": "property",
// CHECK-NEXT:           "swiftSignature": "open var length: Int"
// CHECK-NEXT:         }
@end
// CHECK-NEXT:       ],
// CHECK-NEXT:       "methods": []
// CHECK-NEXT:     },

@implementation MyClass
// CHECK-NEXT:     {
// CHECK-NEXT:       "declKind": "ObjCImplementation",
// CHECK-NEXT:       "diagName": "MyClass",
// CHECK-NEXT:       "formatKind": "container",
// CHECK-NEXT:       "className": "MyClass",
// CHECK-NEXT:       "properties": [],
// CHECK-NEXT:       "methods": [

- (instancetype)initWithText:(NSString *)text length:(NSInteger)length
{
    self = [super init];
    if (self) {
      _text = text;
      _length = length;
    }
    return self;
}
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCMethod",
// CHECK-NEXT:           "diagName": "MyClass::initWithText:length:",
// CHECK-NEXT:           "formatKind": "method",
// CHECK-NEXT:           "selector": "initWithText:length:",
// CHECK-NEXT:           "body": "{\n    self = [super init];\n    if (self) {\n      _text = text;\n      _length = length;\n    }\n    return self;\n}",
// CHECK-NEXT:           "debug": "note: getClassInterface path",
// CHECK-NEXT:           "swiftSignature": "\n    @objc(initWithText:length:)\n    open init(text: String, length: Int) {\n        <#code#>\n    }\n"
// CHECK-NEXT:         },

+ (BOOL)supportsSecureCoding
{
  return YES;
}
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCMethod",
// CHECK-NEXT:           "diagName": "MyClass::supportsSecureCoding",
// CHECK-NEXT:           "formatKind": "method",
// CHECK-NEXT:           "selector": "supportsSecureCoding",
// CHECK-NEXT:           "body": "{\n  return YES;\n}",
// CHECK-NEXT:           "debug": "note: getClassInterface path",
// CHECK-NEXT:           "swiftSignature": "\n    @objc(supportsSecureCoding)\n    open class func supportsSecureCoding() -> {{.*}} {\n        <#code#>\n    }\n"
// FIXME: check return type of Bool, it now returns Int8 on x86_64
// CHECK-NEXT:         },

- (BOOL)isEqual:(id)object
{
    BOOL result = object == self;
    if (!result && [object isKindOfClass:[MyClass class]]) {
        MyClass *other = (MyClass*)object;
        result = (self.text == other.text || [self.text isEqual:other.text]) &&
                 self.length == other.length;
    }
    return result;
}
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCMethod",
// CHECK-NEXT:           "diagName": "MyClass::isEqual:",
// CHECK-NEXT:           "formatKind": "method",
// CHECK-NEXT:           "selector": "isEqual:",
// CHECK-NEXT:           "body": "{\n    BOOL result = object == self;\n    if (!result && [object isKindOfClass:[MyClass class]]) {\n        MyClass *other = (MyClass*)object;\n        result = (self.text == other.text || [self.text isEqual:other.text]) &&\n                 self.length == other.length;\n    }\n    return result;\n}",
// CHECK-NEXT:           "debug": "note: isOverriding path",
// CHECK-NEXT:           "swiftSignature": "\n    @objc(isEqual:)\n    open func isEqual(_ other: NSObject!) -> {{.*}} {\n        <#code#>\n    }\n"
// CHECK-NEXT:         },

#pragma mark - Description

- (NSString *)description
{
    return @"Static description";
}
// CHECK-NEXT:         {
// CHECK-NEXT:           "declKind": "ObjCMethod",
// CHECK-NEXT:           "diagName": "MyClass::description",
// CHECK-NEXT:           "formatKind": "method",
// CHECK-NEXT:           "selector": "description",
// CHECK-NEXT:           "body": "{\n    return @\"Static description\";\n}",
// CHECK-NEXT:           "debug": "note: findPropertyDecl path",
// CHECK-NEXT:           "swiftSignature": "\n    @objc(description)\n    public let description: String\n"
// CHECK-NEXT:         }

@end
// CHECK-NEXT:       ]
// CHECK-NEXT:     }

#pragma clang assume_nonnull end

// CHECK-NEXT:   ]
// CHECK-NEXT: }
