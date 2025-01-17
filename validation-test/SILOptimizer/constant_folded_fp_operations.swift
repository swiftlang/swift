// RUN: %empty-directory(%t) 
// RUN: %swift_driver %s >%t/constant_folded_fp_operation_validation.swift

// RUN: %target-swift-frontend -emit-sil -O -suppress-warnings -verify %t/constant_folded_fp_operation_validation.swift 2>&1 | %FileCheck --check-prefix=CHECK-SIL %t/constant_folded_fp_operation_validation.swift
// RUN: %target-build-swift -O -suppress-warnings %t/constant_folded_fp_operation_validation.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test,optimized_stdlib
// REQUIRES: swift_in_compiler

// Note: This code is not the testfile itself but generates the testfile in the %t directory.

createTestFile()

func createTestFile() {
    generateOperandAccessors()

    let cmpOpValidator = FPConstantFoldedComparisonOpsValidator()
    cmpOpValidator.generateOptimizedFuncDecls()
    cmpOpValidator.generateUnoptimizedFuncDecls()
    cmpOpValidator.generateComparisonFuncDecls()
    cmpOpValidator.generateComparisonFuncCalls()

    let arithOpValidator = FPConstantFoldedArithmeticOpsValidator()
    arithOpValidator.generateOptimizedFuncDecls()
    arithOpValidator.generateUnoptimizedFuncDecls()
    arithOpValidator.generateComparisonFuncDecls()
    arithOpValidator.generateComparisonFuncCalls()
}

/////////////////// Protocols ///////////////////

/// Implemented by types that have distinct ways of 
/// being represented in an expression context vs all
/// other contexts. 
protocol Representable {
    func math_name() -> String
    func printable_name() -> String
}

/// An rough estimation of a type that needs to validate
/// optimized floating point operations in Swift.
/// 
/// Any new validator should conform to this protocol
/// and then call the generate* functions to generate
/// corresponding test file code.
protocol FPOptimizedOpsValidator {
    associatedtype FPType: Representable
    associatedtype FPOperation: Representable
    associatedtype FPOperand: Representable

    func generateOptimizedFuncDecls()
    func generateUnoptimizedFuncDecls()
    func generateComparisonFuncDecls()
    func generateComparisonFuncCalls()

    func optimizedFunDeclCheck(fpType: FPType, op: FPOperation, op1: FPOperand, op2: FPOperand) -> String
    func unoptimizedFunDeclCheck(fpType: FPType, op: FPOperation, op1: FPOperand, op2: FPOperand) -> String
    func resultComparisonCheck(fpType: FPType, op: FPOperation, op1: FPOperand, op2: FPOperand) -> String
}

////////////////// Common ///////////////////

// Generates accessors for floating point operands
// commonly used in the tests.
//
// These accessors prevent the unoptimized function versions
// from getting optimized by the mandatory optimization passes
// and returning a constant value.
func generateOperandAccessors() {
    print("""
    @inline(never) @_silgen_name("zero_float") @_optimize(none)
    func zero_float() -> Float {
        return 0.0
    }

    @inline(never) @_silgen_name("one_float") @_optimize(none)
    func one_float() -> Float {
        return 1.0
    }

    @inline(never) @_silgen_name("zero_double") @_optimize(none)
    func zero_double() -> Double {
        return 0.0
    }

    @inline(never) @_silgen_name("one_double") @_optimize(none)
    func one_double() -> Double {
        return 1.0
    }
    """)
}

////////////////// Comparison Operations Validator ///////////////////

struct FPConstantFoldedComparisonOpsValidator: FPOptimizedOpsValidator {
    /////////////////////// TYPES ///////////////////////
    
    /// Type of floating points this validator deals with.
    enum _FpType : CaseIterable, Representable, Equatable {
        case Float
        case Double

        func math_name() -> String {
            switch self {
            case .Float:
                return "Float"
            case .Double:
                return "Double"
            }
        }

        func printable_name() -> String {
            switch self {
            case .Float:
                return "float"
            case .Double:
                return "double"
            }
        }
    }

    /// Type of floating point operations this validator deals with.
    enum _FPOperation : CaseIterable, Representable, Equatable {
        case LessThan
        case GreaterThan
        case LessThanOrEqual
        case GreaterThanOrEqual
        case Equal
        case NotEqual

        func math_name() -> String {
            switch self {
                case .LessThan:
                    return "<"
                case .GreaterThan:
                    return ">"
                case .LessThanOrEqual:
                    return "<="
                case .GreaterThanOrEqual:
                    return ">="
                case .Equal:
                    return "=="
                case .NotEqual:
                    return "!="
            }
        }

        func printable_name() -> String {
            switch self {
                case .LessThan:
                    return "lessThan"
                case .GreaterThan:
                    return "greaterThan"
                case .LessThanOrEqual:
                    return "lessThanOrEqual"
                case .GreaterThanOrEqual:
                    return "greaterThanOrEqual"
                case .Equal:
                    return "equal"
                case .NotEqual:
                    return "notEqual"
            }
        }
    }

    /// Type of floating point operands this validator deals with.
    enum _FPOperand : CaseIterable, Representable, Equatable {
        case Zero
        case One
        case Infinity
        case Nan

        func isSpecial() -> Bool {
            switch self {
                case .Zero:
                    return false
                case .One:
                    return false
                case .Infinity:
                    return true
                case .Nan:
                    return true
            }
        }

        func math_name() -> String {
            switch self {
                case .Zero:
                    return "0.0"
                case .One:
                    return "1.0"
                case .Infinity:
                    return "infinity"
                case .Nan:
                    return "nan"
            }
        }

        func printable_name() -> String {
            switch self {
                case .Zero:
                    return "zero"
                case .One:
                    return "one"
                case .Infinity:
                    return "infinity"
                case .Nan:
                    return "nan"
            }
        }
    }

    private let optPrefix = "opt"
    private let unoptPrefix = "unopt"

    /////////////////////// FPOptimizedOpsValidator Conformances ///////////////////////
    typealias FPType = _FpType
    typealias FPOperation = _FPOperation
    typealias FPOperand = _FPOperand

    func optimizedFunDeclCheck(
        fpType: FPType, 
        op: FPOperation, 
        op1: FPOperand, 
        op2: FPOperand
    ) -> String {
        let funcName = [optPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")

        return """
        // CHECK-SIL-LABEL: sil hidden [noinline] @\(funcName)
        // CHECK-SIL-NEXT: [global: ]
        // CHECK-SIL-NEXT: bb0:
        // CHECK-SIL-NOT: {{.*}}fcmp{{.*}}
        // CHECK-SIL: } // end sil function '\(funcName)' 
        """
    }

    func unoptimizedFunDeclCheck(
        fpType: FPType, 
        op: FPOperation, 
        op1: FPOperand, 
        op2: FPOperand
    ) -> String {
        let funcName = [unoptPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")

        return """
        // CHECK-SIL-LABEL: sil hidden [noinline] [Onone] @\(funcName)
        // CHECK-SIL-NEXT: bb0:
        // CHECK-SIL: {{.*}}fcmp{{.*}}
        // CHECK-SIL: } // end sil function '\(funcName)' 
        """
    }

    func resultComparisonCheck(
        fpType: FPType, 
        op: FPOperation, 
        op1: FPOperand, 
        op2: FPOperand
    ) -> String {
        let comparisonFuncName = ["comparison", fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")

        return """
        precondition(\(comparisonFuncName)(), "\(comparisonFuncName) returned false!")
        """
    }

    func generateOptimizedFuncDecls() {
        for fpType in FPType.allCases {
            for op in FPOperation.allCases {
                for op1 in FPOperand.allCases {
                    for op2 in FPOperand.allCases {
                        if checkIfEqCmpBetweenInfAndNonInf(op: op, op1: op1, op2: op2) {
                            continue
                        }
                        generateFuncDeclWithCheckDirectives(fpType: fpType, op: op, op1: op1, op2: op2, isopt: true)
                    }
                }
            }
        }
    }

    func generateUnoptimizedFuncDecls() {
        for fpType in FPType.allCases {
            for op in FPOperation.allCases {
                for op1 in FPOperand.allCases {
                    for op2 in FPOperand.allCases {
                        if checkIfEqCmpBetweenInfAndNonInf(op: op, op1: op1, op2: op2) {
                            continue
                        }
                        generateFuncDeclWithCheckDirectives(fpType: fpType, op: op, op1: op1, op2: op2, isopt: false)
                    }
                }
            }
        }
    }

    func generateComparisonFuncDecls() {
        for fpType in FPType.allCases {
            for op in FPOperation.allCases {
                for op1 in FPOperand.allCases {
                    for op2 in FPOperand.allCases {
                        if checkIfEqCmpBetweenInfAndNonInf(op: op, op1: op1, op2: op2) {
                            continue
                        }
                        let comparisonFuncName = ["comparison", fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")
                        let optFuncName = [optPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")
                        let unoptFuncName = [unoptPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")
                        
                        print("""
                        @inline(never) @_silgen_name("\(comparisonFuncName)") @_optimize(none)
                        func \(comparisonFuncName)() -> Bool {
                            return \(optFuncName)() == \(unoptFuncName)()
                        }
                                
                        """)
                    }
                }
            }
        }
    }

    func generateComparisonFuncCalls() {
        for fpType in FPType.allCases {
            for op in FPOperation.allCases {
                for op1 in FPOperand.allCases {
                    for op2 in FPOperand.allCases {
                        if checkIfEqCmpBetweenInfAndNonInf(op: op, op1: op1, op2: op2) {
                            continue
                        }
                        let comparison = resultComparisonCheck(fpType: fpType, op: op, op1: op1, op2: op2)

                        print("""
                        \(comparison)

                        """)
                    }
                }
            }
        }
    }

    /////////////////////// Utilities ///////////////////////
    private func generateFuncDeclWithCheckDirectives(
        fpType: FPType, 
        op: FPOperation, 
        op1: FPOperand, 
        op2: FPOperand, 
        isopt: Bool = false
    ) {
        var operand1 = op1.isSpecial() ? fpType.math_name() + "." + op1.math_name() : op1.math_name()
        var operand2 = op2.isSpecial() ? fpType.math_name() + "." + op2.math_name() : op2.math_name()

        if !isopt {
            if !op1.isSpecial() {
                if fpType == .Double {
                    operand1 = op1 == .Zero ? "zero_double()": "one_double()"
                } else {
                    operand1 = op1 == .Zero ? "zero_float()": "one_float()"
                }
            }
            if !op2.isSpecial() {
                if fpType == .Double {
                    operand2 = op2 == .Zero ? "zero_double()": "one_double()"
                } else {
                    operand2 = op2 == .Zero ? "zero_float()": "one_float()"
                }
            }
        }

        let optPrefix = isopt ? optPrefix : unoptPrefix
        let optAttr = isopt ? "" : "@_optimize(none)"
        let funcName = [optPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")
        let checkDirectives = isopt ? 
        optimizedFunDeclCheck(fpType: fpType, op: op, op1: op1, op2: op2) : 
        unoptimizedFunDeclCheck(fpType: fpType, op: op, op1: op1, op2: op2)

        print("""
        @inline(never) @_silgen_name("\(funcName)") \(optAttr)
        func \(funcName)() -> Bool {
            return \(operand1) \(op.math_name()) \(operand2)
        }
        \(checkDirectives)
                
        """)
    }

    // Equality comparisons b/w infinity and non-infinity are not constant folded.
    // In such comparisons, special floating point types - Float80 and Float16, may 
    // come into play and pattern matching against them complicates the constant folding
    // logic more than we'd like.
    private func checkIfEqCmpBetweenInfAndNonInf(op: FPOperation, op1: FPOperand, op2: FPOperand) -> Bool {
        if op == .Equal || op == .NotEqual {
            // If only one of the operands is infinity
            if (op1 == .Infinity || op2 == .Infinity) && !(op1 == .Infinity && op2 == .Infinity) {
                return true
            }
        }
        return false
    }
}

////////////////// Arithmetic Operations Validator ///////////////////

struct FPConstantFoldedArithmeticOpsValidator: FPOptimizedOpsValidator {
    /// Type of floating points this validator deals with.
    enum _FpType : CaseIterable, Representable, Equatable {
        case Float
        case Double

        func math_name() -> String {
            switch self {
            case .Float:
                return "Float"
            case .Double:
                return "Double"
            }
        }

        func printable_name() -> String {
            switch self {
            case .Float:
                return "float"
            case .Double:
                return "double"
            }
        }
    }

    /// Type of floating point operations this validator deals with.
    enum _FPOperation : CaseIterable, Representable, Equatable {
        case Add
        case Subtract
        case Multiply
        case Divide

        func math_name() -> String {
            switch self {
                case .Add:
                    return "+"
                case .Subtract:
                    return "-"
                case .Multiply:
                    return "*"
                case .Divide:
                    return "/"
            }
        }

        func printable_name() -> String {
            switch self {
                case .Add:
                    return "add"
                case .Subtract:
                    return "sub"
                case .Multiply:
                    return "mul"
                case .Divide:
                    return "div"
            }
        }
    }

    /// Type of floating point operands this validator deals with.
    enum _FPOperand : CaseIterable, Representable, Equatable {
        case Zero
        case One

        func math_name() -> String {
            switch self {
                case .Zero:
                    return "0.0"
                case .One:
                    return "1.0"
            }
        }

        func printable_name() -> String {
            switch self {
                case .Zero:
                    return "zero"
                case .One:
                    return "one"
            }
        }
    }

    private let optPrefix = "opt"
    private let unoptPrefix = "unopt"

    /////////////////////// FPOptimizedOpsValidator Conformances ///////////////////////
    typealias FPType = _FpType
    typealias FPOperation = _FPOperation
    typealias FPOperand = _FPOperand

        func optimizedFunDeclCheck(
        fpType: FPType, 
        op: FPOperation, 
        op1: FPOperand, 
        op2: FPOperand
    ) -> String {
        let funcName = [optPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")

        return """
        // CHECK-SIL-LABEL: sil hidden [noinline] @\(funcName)
        // CHECK-SIL-NEXT: [global: ]
        // CHECK-SIL-NEXT: bb0:
        // CHECK-SIL-NOT: {{.*}}f\(op.printable_name()){{.*}}
        // CHECK-SIL: } // end sil function '\(funcName)' 
        """
    }

    func unoptimizedFunDeclCheck(
        fpType: FPType, 
        op: FPOperation, 
        op1: FPOperand, 
        op2: FPOperand
    ) -> String {
        let funcName = [unoptPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")

        return """
        // CHECK-SIL-LABEL: sil hidden [noinline] [Onone] @\(funcName)
        // CHECK-SIL-NEXT: bb0:
        // CHECK-SIL:  {{.*}}f\(op.printable_name()){{.*}}
        // CHECK-SIL: } // end sil function '\(funcName)' 
        """
    }

    func resultComparisonCheck(
        fpType: FPType, 
        op: FPOperation, 
        op1: FPOperand, 
        op2: FPOperand
    ) -> String {
        let comparisonFuncName = ["comparison", fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")

        // 0.0 / 0.0 is NaN and Nan != Nan
        if op == .Divide && op1 == .Zero && op2 == .Zero {
            return """
            precondition(!\(comparisonFuncName)(), "\(comparisonFuncName) returned true. Expected false.")
            """
        }

        return """
        precondition(\(comparisonFuncName)(), "\(comparisonFuncName) returned false. Expected true.")
        """
    }

    func generateOptimizedFuncDecls() {
        for fpType in FPType.allCases {
            for op in FPOperation.allCases {
                for op1 in FPOperand.allCases {
                    for op2 in FPOperand.allCases {
                        generateFuncDeclWithCheckDirectives(fpType: fpType, op: op, op1: op1, op2: op2, isopt: true)
                    }
                }
            }
        }
    }

    func generateUnoptimizedFuncDecls() {
        for fpType in FPType.allCases {
            for op in FPOperation.allCases {
                for op1 in FPOperand.allCases {
                    for op2 in FPOperand.allCases {
                        generateFuncDeclWithCheckDirectives(fpType: fpType, op: op, op1: op1, op2: op2, isopt: false)
                    }
                }
            }
        }
    }

    func generateComparisonFuncDecls() {
        for fpType in FPType.allCases {
            for op in FPOperation.allCases {
                for op1 in FPOperand.allCases {
                    for op2 in FPOperand.allCases {
                        let comparisonFuncName = ["comparison", fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")
                        let optFuncName = [optPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")
                        let unoptFuncName = [unoptPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")
                        
                        print("""
                        @inline(never) @_silgen_name("\(comparisonFuncName)") @_optimize(none)
                        func \(comparisonFuncName)() -> Bool {
                            return \(optFuncName)() == \(unoptFuncName)()
                        }
                                
                        """)
                    }
                }
            }
        }
    }

    func generateComparisonFuncCalls() {
        for fpType in FPType.allCases {
            for op in FPOperation.allCases {
                for op1 in FPOperand.allCases {
                    for op2 in FPOperand.allCases {
                        let comparison = resultComparisonCheck(fpType: fpType, op: op, op1: op1, op2: op2)
                        print("""
                        \(comparison)

                        """)   
                    }
                }
            }
        }
    }

    /////////////////////// Utilities ///////////////////////
    private func generateFuncDeclWithCheckDirectives(
        fpType: FPType, 
        op: FPOperation, 
        op1: FPOperand, 
        op2: FPOperand, 
        isopt: Bool = false
    ) {
        var operand1 = op1.math_name()
        var operand2 = op2.math_name()

        if !isopt {
            if fpType == .Double {
                operand1 = op1 == .Zero ? "zero_double()": "one_double()"
                operand2 = op2 == .Zero ? "zero_double()": "one_double()"
            } else {
                operand1 = op1 == .Zero ? "zero_float()": "one_float()"
                operand2 = op2 == .Zero ? "zero_float()": "one_float()"
            }
        }

        let optPrefix = isopt ? optPrefix : unoptPrefix
        let optAttr = isopt ? "" : "@_optimize(none)"
        let funcName = [optPrefix, fpType.printable_name(), op.printable_name(), op1.printable_name(), op2.printable_name()].joined(separator: "_")
        let checkDirectives = isopt ? 
        optimizedFunDeclCheck(fpType: fpType, op: op, op1: op1, op2: op2) : 
        unoptimizedFunDeclCheck(fpType: fpType, op: op, op1: op1, op2: op2)

        print("""
        @inline(never) @_silgen_name("\(funcName)") \(optAttr)
        func \(funcName)() -> \(fpType.math_name()) {
            return \(operand1) \(op.math_name()) \(operand2)
        }
        \(checkDirectives)
                
        """)
    }
}
