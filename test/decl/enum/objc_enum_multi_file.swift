// RUN: not %target-swift-frontend -disable-objc-attr-requires-foundation-module -enable-objc-interop -module-name main %s -primary-file %S/Inputs/objc_enum_multi_file_helper.swift -emit-ir -D NO_RAW_TYPE 2>&1 | %FileCheck -check-prefix=NO_RAW_TYPE %s
// RUN: not %target-swift-frontend -disable-objc-attr-requires-foundation-module -enable-objc-interop -module-name main %s -primary-file %S/Inputs/objc_enum_multi_file_helper.swift -emit-ir -D BAD_RAW_TYPE 2>&1 | %FileCheck -check-prefix=BAD_RAW_TYPE %s
// RUN: not %target-swift-frontend -disable-objc-attr-requires-foundation-module -enable-objc-interop -module-name main %s -primary-file %S/Inputs/objc_enum_multi_file_helper.swift -emit-ir -D NON_INT_RAW_TYPE 2>&1 | %FileCheck -check-prefix=NON_INT_RAW_TYPE %s
// RUN: not %target-swift-frontend -disable-objc-attr-requires-foundation-module -enable-objc-interop -module-name main %s -primary-file %S/Inputs/objc_enum_multi_file_helper.swift -emit-ir -D NON_INT_RAW_VALUE 2>&1 | %FileCheck -check-prefix=NON_INT_RAW_VALUE %s
// RUN: not %target-swift-frontend -disable-objc-attr-requires-foundation-module -enable-objc-interop -module-name main %s -primary-file %S/Inputs/objc_enum_multi_file_helper.swift -emit-ir -D NO_CASES 2>&1 | %FileCheck -check-prefix=NO_CASES %s
// RUN: not %target-swift-frontend -disable-objc-attr-requires-foundation-module -enable-objc-interop -module-name main %s -primary-file %S/Inputs/objc_enum_multi_file_helper.swift -emit-ir -D DUPLICATE_CASES 2>&1 | %FileCheck -check-prefix=DUPLICATE_CASES %s
// Note that the *other* file is the primary file in this test!

#if NO_RAW_TYPE
// NO_RAW_TYPE: :[[@LINE+1]]:12: error: '@objc' enum must declare an integer raw type
@objc enum TheEnum {
  case A
}

#elseif BAD_RAW_TYPE
// BAD_RAW_TYPE: :[[@LINE+1]]:12: error: '@objc' enum raw type 'Array<Int>' is not an integer type
@objc enum TheEnum : Array<Int> {
  case A
}

#elseif NON_INT_RAW_TYPE
// NON_INT_RAW_TYPE: :[[@LINE+1]]:12: error: '@objc' enum raw type 'Float' is not an integer type
@objc enum TheEnum : Float {
  case A = 0.0
}

#elseif NO_CASES
// NO_CASES: :[[@LINE+1]]:12: error: an enum with no cases cannot declare a raw type
@objc enum TheEnum : Int32 {
  static var A: TheEnum! { return nil }
}

#elseif DUPLICATE_CASES
@objc enum TheEnum : Int32 {
  case A
  case B = 0
  // DUPLICATE_CASES: :[[@LINE-1]]:12: error: raw value for enum case is not unique
  // DUPLICATE_CASES: note: raw value previously used here
  // DUPLICATE_CASES: note: raw value implicitly auto-incremented from zero
}

#elseif NON_INT_RAW_VALUE
@objc enum TheEnum : Int32 {
  case A = 0
  case B = "B"
  // NON_INT_RAW_VALUE: :[[@LINE-1]]:12: error: cannot convert value of type 'String' to raw type 'Int32'
}

#else
enum TheEnum: Invalid { // should never be hit
  case A
}

#endif
