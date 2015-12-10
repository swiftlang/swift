#include "swift/SILAnalysis/TypeExpansionAnalysis.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "typeexpansion-analysis"

using namespace swift;

// The MemoryBehavior Cache must not grow beyond this size.
// We limit the size of the MB cache to 2**12 because we want to limit the
// memory usage of this cache.
static const int TypeExpansionAnalysisMaxCacheSize = 4096;

const ProjectionPathList & 
TypeExpansionAnalysis::getTypeLeafExpansion(SILType B, SILModule *Mod) {
  // Check whether we have the type expansion.
  if (LeafTECache.find(B) != LeafTECache.end()) {
    return LeafTECache.find(B)->second;
  }   

  // Flush the cache if the size of the cache is too large.
  if (LeafTECache.size() > TypeExpansionAnalysisMaxCacheSize) {
    LeafTECache.clear();
  }

  // Need to build the type expansion.
  LeafTECache[B] = ProjectionPathList();
  ProjectionPath::expandTypeIntoLeafProjectionPaths(B, Mod, LeafTECache[B]);
  return LeafTECache[B];
}

const ProjectionPathList & 
TypeExpansionAnalysis::getTypeNodeExpansion(SILType B, SILModule *Mod) {
  // Check whether we have the type expansion.
  if (NodeTECache.find(B) != NodeTECache.end()) {
    return NodeTECache.find(B)->second;
  }   

  // Flush the cache if the size of the cache is too large.
  if (NodeTECache.size() > TypeExpansionAnalysisMaxCacheSize) {
    NodeTECache.clear();
  }

  // Need to build the type expansion.
  NodeTECache[B] = ProjectionPathList();
  ProjectionPath::expandTypeIntoNodeProjectionPaths(B, Mod, NodeTECache[B]);
  return NodeTECache[B];
}

SILAnalysis *swift::createTypeExpansionAnalysis(SILModule *M) {
  return new TypeExpansionAnalysis(M);
}
