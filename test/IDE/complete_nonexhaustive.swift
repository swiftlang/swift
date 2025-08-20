// RUN: %batch-code-completion

// NONEXHAUSTIVE-DAG: Keyword/None:                       warn; name=warn

@nonexhaustive(#^NONEXHAUSTIVE^#)
public enum E {
}
