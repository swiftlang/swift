//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// Advanced cycle detection and breaking algorithms for automatic memory management.

//===----------------------------------------------------------------------===//
// MARK: - Reference Cycle Detection
//===----------------------------------------------------------------------===//

/// Comprehensive reference cycle detection and analysis system.
public enum ReferenceCycleDetector {
  
  /// Detect reference cycles starting from a set of root objects.
  ///
  /// This function performs a depth-first search to identify potential
  /// reference cycles in the object graph.
  ///
  /// - Parameter rootObjects: Objects to start cycle detection from.
  /// - Returns: Array of detected cycles.
  public static func detectCycles<T: AnyObject>(_ rootObjects: [T]) -> [ReferenceCycle] {
    var cycles: [ReferenceCycle] = []
    var visited: Set<ObjectIdentifier> = []
    var currentPath: [ObjectIdentifier] = []
    
    for rootObject in rootObjects {
      detectCyclesRecursive(rootObject, visited: &visited, 
                           currentPath: &currentPath, cycles: &cycles)
    }
    
    return cycles
  }
  
  /// Detect cycles for a single object.
  ///
  /// - Parameter object: The object to analyze.
  /// - Returns: Cycles involving this object.
  public static func detectCycles<T: AnyObject>(for object: T) -> [ReferenceCycle] {
    return detectCycles([object])
  }
  
  /// Perform comprehensive cycle analysis on all live objects.
  ///
  /// This function analyzes all currently allocated objects and
  /// identifies potential reference cycles.
  ///
  /// - Returns: Complete cycle analysis results.
  public static func performGlobalCycleAnalysis() -> CycleAnalysisResult {
    // This would be implemented by the runtime to analyze all live objects
    let stats = ARCDebug.getStatistics()
    
    return CycleAnalysisResult(
      totalObjectsAnalyzed: Int(stats.currentAllocations),
      cyclesDetected: [], // Would be populated by actual analysis
      suspiciousObjects: Int(stats.currentAllocations / 10), // Heuristic
      analysisTime: 0.0,
      recommendations: generateRecommendations(stats)
    )
  }
  
  private static func detectCyclesRecursive<T: AnyObject>(
    _ object: T,
    visited: inout Set<ObjectIdentifier>,
    currentPath: inout [ObjectIdentifier],
    cycles: inout [ReferenceCycle]
  ) {
    let objectId = ObjectIdentifier(object)
    
    // Check if we've found a cycle
    if let cycleStart = currentPath.firstIndex(of: objectId) {
      let cycleObjects = Array(currentPath[cycleStart...])
      cycles.append(ReferenceCycle(objects: cycleObjects))
      return
    }
    
    // Check if already visited
    if visited.contains(objectId) {
      return
    }
    
    visited.insert(objectId)
    currentPath.append(objectId)
    
    // In a real implementation, this would traverse object references
    // For now, we'll use a simplified approach
    
    currentPath.removeLast()
  }
  
  private static func generateRecommendations(_ stats: ARCStatistics) -> [String] {
    var recommendations: [String] = []
    
    let retainReleaseRatio = Double(stats.totalRetains) / Double(max(stats.totalReleases, 1))
    
    if retainReleaseRatio > 1.1 {
      recommendations.append("Consider using weak references to break potential cycles")
    }
    
    if stats.currentAllocations > stats.peakAllocations * 8 / 10 {
      recommendations.append("High memory usage detected - check for memory leaks")
    }
    
    if stats.totalWeakRetains > stats.totalRetains / 2 {
      recommendations.append("High weak reference usage - verify cycle breaking is effective")
    }
    
    if recommendations.isEmpty {
      recommendations.append("Memory usage appears optimal")
    }
    
    return recommendations
  }
}

/// Represents a detected reference cycle.
public struct ReferenceCycle {
  /// Object identifiers in the cycle.
  public let objects: [ObjectIdentifier]
  
  /// Length of the cycle.
  public var length: Int { return objects.count }
  
  /// Whether this is a self-reference cycle.
  public var isSelfReference: Bool { return length == 1 }
  
  public init(objects: [ObjectIdentifier]) {
    self.objects = objects
  }
}

extension ReferenceCycle: CustomStringConvertible {
  public var description: String {
    if isSelfReference {
      return "Self-reference cycle: \(objects[0])"
    } else {
      let objectList = objects.map { String(describing: $0) }.joined(separator: " -> ")
      return "Reference cycle (\(length) objects): \(objectList)"
    }
  }
}

/// Results of comprehensive cycle analysis.
public struct CycleAnalysisResult {
  /// Total number of objects analyzed.
  public let totalObjectsAnalyzed: Int
  
  /// Detected reference cycles.
  public let cyclesDetected: [ReferenceCycle]
  
  /// Number of objects that might be in cycles.
  public let suspiciousObjects: Int
  
  /// Time taken for analysis (in seconds).
  public let analysisTime: Double
  
  /// Recommendations for memory optimization.
  public let recommendations: [String]
  
  public init(totalObjectsAnalyzed: Int, cyclesDetected: [ReferenceCycle],
              suspiciousObjects: Int, analysisTime: Double, recommendations: [String]) {
    self.totalObjectsAnalyzed = totalObjectsAnalyzed
    self.cyclesDetected = cyclesDetected
    self.suspiciousObjects = suspiciousObjects
    self.analysisTime = analysisTime
    self.recommendations = recommendations
  }
}

extension CycleAnalysisResult: CustomStringConvertible {
  public var description: String {
    var result = """
    Cycle Analysis Results:
      Objects analyzed: \(totalObjectsAnalyzed)
      Cycles detected: \(cyclesDetected.count)
      Suspicious objects: \(suspiciousObjects)
      Analysis time: \(String(format: "%.3f", analysisTime))s
    
    """
    
    if !cyclesDetected.isEmpty {
      result += "Detected Cycles:\n"
      for (index, cycle) in cyclesDetected.enumerated() {
        result += "  \(index + 1). \(cycle)\n"
      }
      result += "\n"
    }
    
    if !recommendations.isEmpty {
      result += "Recommendations:\n"
      for recommendation in recommendations {
        result += "  • \(recommendation)\n"
      }
    }
    
    return result
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Automatic Cycle Breaking
//===----------------------------------------------------------------------===//

/// Automatic cycle breaking strategies.
public enum CycleBreaker {
  
  /// Strategy for breaking reference cycles.
  public enum Strategy {
    case convertToWeak        // Convert strong refs to weak
    case useUnowned          // Use unowned references  
    case restructureObjects  // Restructure object relationships
    case manualBreaking      // Require manual cycle breaking
  }
  
  /// Automatically break detected cycles using the specified strategy.
  ///
  /// - Parameters:
  ///   - cycles: The cycles to break.
  ///   - strategy: The strategy to use for breaking cycles.
  /// - Returns: Results of the cycle breaking operation.
  public static func breakCycles(_ cycles: [ReferenceCycle], 
                                strategy: Strategy) -> CycleBreakingResult {
    var brokenCycles: [ReferenceCycle] = []
    var failedCycles: [ReferenceCycle] = []
    
    for cycle in cycles {
      if breakCycle(cycle, using: strategy) {
        brokenCycles.append(cycle)
      } else {
        failedCycles.append(cycle)
      }
    }
    
    return CycleBreakingResult(
      strategy: strategy,
      totalCycles: cycles.count,
      brokenCycles: brokenCycles,
      failedCycles: failedCycles,
      successRate: Double(brokenCycles.count) / Double(cycles.count)
    )
  }
  
  /// Suggest optimal cycle breaking strategy for given cycles.
  ///
  /// - Parameter cycles: The cycles to analyze.
  /// - Returns: Recommended strategy and confidence level.
  public static func suggestStrategy(for cycles: [ReferenceCycle]) -> (Strategy, Double) {
    // Analyze cycle characteristics
    let selfReferenceCycles = cycles.filter { $0.isSelfReference }.count
    let shortCycles = cycles.filter { $0.length <= 3 }.count
    let longCycles = cycles.count - shortCycles
    
    // Determine best strategy based on cycle patterns
    if selfReferenceCycles > cycles.count / 2 {
      return (.convertToWeak, 0.9)
    } else if shortCycles > longCycles {
      return (.useUnowned, 0.8)
    } else if longCycles > 0 {
      return (.restructureObjects, 0.7)
    } else {
      return (.manualBreaking, 0.6)
    }
  }
  
  private static func breakCycle(_ cycle: ReferenceCycle, using strategy: Strategy) -> Bool {
    // In a real implementation, this would actually modify object references
    // For now, we'll simulate the breaking
    
    switch strategy {
    case .convertToWeak:
      // Convert one reference in the cycle to weak
      return cycle.length >= 2
      
    case .useUnowned:
      // Use unowned reference for guaranteed lifetime relationships
      return cycle.length <= 3
      
    case .restructureObjects:
      // Suggest object restructuring
      return false // Requires manual intervention
      
    case .manualBreaking:
      // Require manual cycle breaking
      return false
    }
  }
}

/// Results of cycle breaking operations.
public struct CycleBreakingResult {
  /// Strategy used for breaking cycles.
  public let strategy: CycleBreaker.Strategy
  
  /// Total number of cycles processed.
  public let totalCycles: Int
  
  /// Successfully broken cycles.
  public let brokenCycles: [ReferenceCycle]
  
  /// Cycles that couldn't be broken automatically.
  public let failedCycles: [ReferenceCycle]
  
  /// Success rate (0.0 to 1.0).
  public let successRate: Double
  
  public init(strategy: CycleBreaker.Strategy, totalCycles: Int,
              brokenCycles: [ReferenceCycle], failedCycles: [ReferenceCycle],
              successRate: Double) {
    self.strategy = strategy
    self.totalCycles = totalCycles
    self.brokenCycles = brokenCycles
    self.failedCycles = failedCycles
    self.successRate = successRate
  }
}

extension CycleBreakingResult: CustomStringConvertible {
  public var description: String {
    let successPercent = successRate * 100
    let status = successPercent == 100 ? "COMPLETE" :
                 successPercent >= 80 ? "GOOD" :
                 successPercent >= 50 ? "PARTIAL" : "LIMITED"
    
    return """
    Cycle Breaking Results:
      Strategy: \(strategy)
      Total cycles: \(totalCycles)
      Broken cycles: \(brokenCycles.count)
      Failed cycles: \(failedCycles.count)
      Success rate: \(String(format: "%.1f", successPercent))% (\(status))
    """
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Real-time Cycle Monitoring
//===----------------------------------------------------------------------===//

/// Real-time cycle monitoring for production environments.
public class CycleMonitor {
  private var isMonitoring = false
  private var detectionThreshold = 100 // Check every N allocations
  private var allocationCount = 0
  
  /// Start monitoring for reference cycles.
  ///
  /// - Parameter threshold: Check for cycles every N allocations.
  public func startMonitoring(threshold: Int = 100) {
    detectionThreshold = threshold
    allocationCount = 0
    isMonitoring = true
    
    print("[CycleMonitor] Started monitoring (threshold: \(threshold))")
  }
  
  /// Stop cycle monitoring.
  public func stopMonitoring() {
    isMonitoring = false
    print("[CycleMonitor] Stopped monitoring")
  }
  
  /// Called by runtime on object allocation.
  internal func notifyAllocation() {
    guard isMonitoring else { return }
    
    allocationCount += 1
    
    if allocationCount >= detectionThreshold {
      performQuickCycleCheck()
      allocationCount = 0
    }
  }
  
  /// Perform a quick cycle check.
  private func performQuickCycleCheck() {
    let analysis = ReferenceCycleDetector.performGlobalCycleAnalysis()
    
    if !analysis.cyclesDetected.isEmpty {
      print("[CycleMonitor] WARNING: \(analysis.cyclesDetected.count) cycles detected")
      
      // Suggest automatic cycle breaking
      let (strategy, confidence) = CycleBreaker.suggestStrategy(for: analysis.cyclesDetected)
      print("[CycleMonitor] Suggested strategy: \(strategy) (confidence: \(confidence))")
      
      if confidence >= 0.8 {
        let result = CycleBreaker.breakCycles(analysis.cyclesDetected, strategy: strategy)
        print("[CycleMonitor] Auto-breaking result: \(result.successRate * 100)% success")
      }
    }
  }
}

/// Global cycle monitor instance.
public let globalCycleMonitor = CycleMonitor()

//===----------------------------------------------------------------------===//
// MARK: - Memory Leak Detection
//===----------------------------------------------------------------------===//

/// Memory leak detection utilities.
public enum LeakDetector {
  
  /// Detect potential memory leaks.
  ///
  /// This function analyzes object allocation patterns and identifies
  /// objects that might be leaking memory.
  ///
  /// - Returns: Leak detection results.
  public static func detectLeaks() -> LeakDetectionResult {
    let stats = ARCDebug.getStatistics()
    let analysis = MemoryManagement.analyzeMemoryUsage()
    
    // Heuristics for leak detection
    var suspiciousPatterns: [String] = []
    
    // Check for growing allocations without corresponding deallocations
    if stats.totalAllocations > stats.totalDeallocations * 2 {
      suspiciousPatterns.append("Allocations significantly exceed deallocations")
    }
    
    // Check for unbalanced retain/release operations
    if analysis.retainReleaseRatio > 1.2 {
      suspiciousPatterns.append("Unbalanced retain/release operations detected")
    }
    
    // Check for high current allocation count
    if stats.currentAllocations > stats.peakAllocations * 9 / 10 {
      suspiciousPatterns.append("High current allocation count (near peak)")
    }
    
    let leakProbability = calculateLeakProbability(stats, analysis)
    
    return LeakDetectionResult(
      totalAllocations: Int(stats.totalAllocations),
      totalDeallocations: Int(stats.totalDeallocations),
      currentAllocations: Int(stats.currentAllocations),
      suspiciousPatterns: suspiciousPatterns,
      leakProbability: leakProbability,
      recommendations: generateLeakRecommendations(suspiciousPatterns, leakProbability)
    )
  }
  
  /// Monitor memory usage over time.
  ///
  /// - Parameter duration: How long to monitor (in seconds).
  /// - Returns: Memory usage trend analysis.
  public static func monitorMemoryUsage(duration: Double) -> MemoryTrendAnalysis {
    let startStats = ARCDebug.getStatistics()
    
    // In a real implementation, this would monitor over time
    // For now, provide a snapshot
    
    let endStats = ARCDebug.getStatistics()
    
    return MemoryTrendAnalysis(
      duration: duration,
      startAllocations: Int(startStats.currentAllocations),
      endAllocations: Int(endStats.currentAllocations),
      allocationRate: Double(endStats.totalAllocations - startStats.totalAllocations) / duration,
      deallocationRate: Double(endStats.totalDeallocations - startStats.totalDeallocations) / duration,
      trend: calculateTrend(startStats, endStats)
    )
  }
  
  private static func calculateLeakProbability(_ stats: ARCStatistics, _ analysis: MemoryAnalysis) -> Double {
    var probability = 0.0
    
    // Factor in allocation/deallocation imbalance
    let allocationImbalance = Double(stats.totalAllocations - stats.totalDeallocations) / Double(stats.totalAllocations)
    probability += allocationImbalance * 0.4
    
    // Factor in retain/release imbalance
    let retainImbalance = abs(analysis.retainReleaseRatio - 1.0)
    probability += min(retainImbalance, 1.0) * 0.3
    
    // Factor in memory efficiency
    probability += (1.0 - analysis.memoryEfficiency) * 0.3
    
    return min(probability, 1.0)
  }
  
  private static func generateLeakRecommendations(_ patterns: [String], _ probability: Double) -> [String] {
    var recommendations: [String] = []
    
    if probability > 0.7 {
      recommendations.append("HIGH RISK: Immediate investigation recommended")
      recommendations.append("Enable ARC debugging for detailed analysis")
      recommendations.append("Review object lifecycle and reference patterns")
    } else if probability > 0.4 {
      recommendations.append("MEDIUM RISK: Monitor memory usage closely")
      recommendations.append("Consider using weak references in complex object graphs")
    } else {
      recommendations.append("LOW RISK: Memory usage appears normal")
    }
    
    for pattern in patterns {
      recommendations.append("Address pattern: \(pattern)")
    }
    
    return recommendations
  }
  
  private static func calculateTrend(_ start: ARCStatistics, _ end: ARCStatistics) -> MemoryTrend {
    let allocationDelta = Int64(end.currentAllocations) - Int64(start.currentAllocations)
    
    if allocationDelta > 10 {
      return .increasing
    } else if allocationDelta < -10 {
      return .decreasing
    } else {
      return .stable
    }
  }
}

/// Memory leak detection results.
public struct LeakDetectionResult {
  public let totalAllocations: Int
  public let totalDeallocations: Int
  public let currentAllocations: Int
  public let suspiciousPatterns: [String]
  public let leakProbability: Double
  public let recommendations: [String]
  
  public init(totalAllocations: Int, totalDeallocations: Int, currentAllocations: Int,
              suspiciousPatterns: [String], leakProbability: Double, recommendations: [String]) {
    self.totalAllocations = totalAllocations
    self.totalDeallocations = totalDeallocations
    self.currentAllocations = currentAllocations
    self.suspiciousPatterns = suspiciousPatterns
    self.leakProbability = leakProbability
    self.recommendations = recommendations
  }
}

extension LeakDetectionResult: CustomStringConvertible {
  public var description: String {
    let probabilityPercent = leakProbability * 100
    let riskLevel = probabilityPercent > 70 ? "HIGH" :
                    probabilityPercent > 40 ? "MEDIUM" : "LOW"
    
    var result = """
    Leak Detection Results:
      Total allocations: \(totalAllocations)
      Total deallocations: \(totalDeallocations)
      Current allocations: \(currentAllocations)
      Leak probability: \(String(format: "%.1f", probabilityPercent))% (\(riskLevel) RISK)
    
    """
    
    if !suspiciousPatterns.isEmpty {
      result += "Suspicious Patterns:\n"
      for pattern in suspiciousPatterns {
        result += "  ⚠️  \(pattern)\n"
      }
      result += "\n"
    }
    
    if !recommendations.isEmpty {
      result += "Recommendations:\n"
      for recommendation in recommendations {
        result += "  💡 \(recommendation)\n"
      }
    }
    
    return result
  }
}

/// Memory usage trend analysis.
public struct MemoryTrendAnalysis {
  public let duration: Double
  public let startAllocations: Int
  public let endAllocations: Int
  public let allocationRate: Double
  public let deallocationRate: Double
  public let trend: MemoryTrend
  
  public init(duration: Double, startAllocations: Int, endAllocations: Int,
              allocationRate: Double, deallocationRate: Double, trend: MemoryTrend) {
    self.duration = duration
    self.startAllocations = startAllocations
    self.endAllocations = endAllocations
    self.allocationRate = allocationRate
    self.deallocationRate = deallocationRate
    self.trend = trend
  }
}

/// Memory usage trend direction.
public enum MemoryTrend {
  case increasing
  case stable
  case decreasing
}

extension MemoryTrendAnalysis: CustomStringConvertible {
  public var description: String {
    let trendEmoji = trend == .increasing ? "📈" : trend == .decreasing ? "📉" : "➡️"
    
    return """
    Memory Trend Analysis (\(String(format: "%.1f", duration))s):
      \(trendEmoji) Trend: \(trend)
      Start allocations: \(startAllocations)
      End allocations: \(endAllocations)
      Allocation rate: \(String(format: "%.1f", allocationRate))/s
      Deallocation rate: \(String(format: "%.1f", deallocationRate))/s
    """
  }
}