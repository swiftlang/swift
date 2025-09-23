// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/create_string.swift -module-name StringCreator -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/StringCreator.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -fobjc-arc -c %t/string-to-nsstring.mm -I %t -o %t/swift-stdlib-execution.o
// RUN: %target-build-swift %t/use_foundation.swift %t/create_string.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name StringCreator -Xfrontend -entry-point-function-name -Xfrontend swiftMain -lc++
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution

// RUN: %target-interop-build-clangxx -std=gnu++20 -fobjc-arc -c %t/string-to-nsstring-one-arc-op.mm -I %t -Xclang -emit-llvm -S -o - -O1 |  %FileCheck --check-prefix=CHECKARC %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

//--- use_foundation.swift
import Foundation

//--- create_string.swift
@_expose(Cxx)
public func createString(_ ptr: UnsafePointer<CChar>) -> String {
    return String(cString: ptr)
}

//--- string-to-nsstring-one-arc-op.mm

#include "StringCreator.h"

int main() {
  using namespace swift;
  auto emptyString = String::init();
  NSString *nsStr = emptyString;
}

// CHECKARC: %[[VAL:.*]] = {{(tail )?}}call swiftcc ptr @"$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF"
// CHECKARC: call ptr @llvm.objc.autorelease(ptr %[[VAL]])
// CHECKARC: @llvm.objc.
// CHECKARC-SAME: autorelease(ptr
// CHECKARC-NOT: @llvm.objc.

//--- string-to-nsstring.mm

#include <cassert>
#include <string>
#include "StringCreator.h"

int main() {
  using namespace swift;

  auto emptyString = String::init();

  {
    NSString *nsStr = emptyString;
    assert(std::string(nsStr.UTF8String) == "");
    assert([nsStr isEqualToString:@""]);
  }

  auto aStr = StringCreator::createString("hello");
  {
    NSString *nsStr = aStr;
    assert(std::string(nsStr.UTF8String) == "hello");
    assert([nsStr isEqualToString:@"hello"]);
  }

  {
    NSString *nsStr = @"nsstr";
    auto str = String::init(nsStr);
    NSString *nsStr2 = str;
    assert(std::string(nsStr.UTF8String) == "nsstr");
    assert([nsStr2 isEqualToString:nsStr]);
  }

  NSString *nsStrContainingUnicode = @"👨‍💻👩‍💻åäö";
  {
    swift::String swiftStr = swift::String::init(nsStrContainingUnicode);
    assert(std::string(swiftStr) == "👨‍💻👩‍💻åäö");
  }

  {
    NSString* arabic_ns = @"طاب يومك";
    auto swift_arabic = String::init(arabic_ns);
    auto std_arabic = (std::string)swift_arabic;
    assert(std_arabic == "طاب يومك");
    
    NSString* mixed_ns = @"Hello مرحبا World";
    auto swift_mixed = String::init(mixed_ns);
    auto std_mixed = (std::string)swift_mixed;
    assert(std_mixed == "Hello مرحبا World");
    
    NSString* hebrew_ns = @"שלום עולם";
    auto swift_hebrew = String::init(hebrew_ns);
    auto std_hebrew = (std::string)swift_hebrew;
    assert(std_hebrew == "שלום עולם");
    
    NSString* chinese_ns = @"你好世界";
    auto swift_chinese = String::init(chinese_ns);
    auto std_chinese = (std::string)swift_chinese;
    assert(std_chinese == "你好世界");
  }

  return 0;
}
