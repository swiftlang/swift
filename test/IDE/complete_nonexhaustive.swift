// RUN: %batch-code-completion -enable-experimental-feature NonexhaustiveAttribute

// REQUIRES: swift_feature_NonexhaustiveAttribute

// NONEXHAUSTIVE-DAG: Keyword/None:                       warn; name=warn

@nonexhaustive(#^NONEXHAUSTIVE^#)
public enum E {
}
