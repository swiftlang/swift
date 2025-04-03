// RUN: %batch-code-completion

func test(subject: Int) {
  OUTER_IF_1:
  if subject == 1 {
    break #^LABEL_1^#
// LABEL_1: Begin completions, 1 items
// LABEL_1-DAG: Pattern/Local:                      OUTER_IF_1;
  }

  OUTER_SWITCH_1:
  switch subject {
  case var x where x < 2:
    break #^LABEL_2^#
// LABEL_2: Begin completions, 1 items
// LABEL_2-DAG: Pattern/Local:                      OUTER_SWITCH_1;

    INNER_IF_1: if subject == 1 {
      INNER_FOR_1: for 0 ..< 1 {
        break #^LABEL_3^#
// LABEL_3: Begin completions, 3 items
// LABEL_3-DAG: Pattern/Local:                      INNER_FOR_1;
// LABEL_3-DAG: Pattern/Local:                      INNER_IF_1;
// LABEL_3-DAG: Pattern/Local:                      OUTER_SWITCH_1;
      }
      break #^LABEL_4^#
// LABEL_4: Begin completions, 2 items
// LABEL_4-DAG: Pattern/Local:                      INNER_IF_1;
// LABEL_4-DAG: Pattern/Local:                      OUTER_SWITCH_1;
    }

    INNER_IF_2: if subject == 1 {
      INNER_WHILE_1: while i == 1 {
        break #^LABEL_5^#
// LABEL_5: Begin completions, 3 items
// LABEL_5-DAG: Pattern/Local:                      INNER_WHILE_1;
// LABEL_5-DAG: Pattern/Local:                      INNER_IF_2;
// LABEL_5-DAG: Pattern/Local:                      OUTER_SWITCH_1;
      }
    }

    INNER_GUARD_1: guard subject == 1 else {
      INNER_DOCATCH_1: do {
      }
      catch let err {
        continue #^LABEL_6^#
// LABEL_6: Begin completions, 1 items
// LABEL_6-DAG: Pattern/Local:                      INNER_DOCATCH_1;
      }
    }

  }

  OUTER_FOR_1: for subject == 1 {
    break #^LABEL_7^#
// LABEL_7: Begin completions, 1 items
// LABEL_7-DAG: Pattern/Local:                      OUTER_FOR_1;
  }

  // This is illegal, but make sure we don't duplicate in completion.
  DUPLICATE_LABEL: do {
    DUPLICATE_LABEL: do {
      break #^LABEL_8^#
      // LABEL_8:     Begin completions, 1 items
      // LABEL_8-DAG: Pattern/Local:                      DUPLICATE_LABEL;
    }
  }
}

TOP_IF_1: if true {}
TOP_IF_2: if true { break #^TOPLEVEL_1^# }
TOP_IF_3: if true {}
// TOPLEVEL_1: Begin completions, 1 items
// TOPLEVEL_1-DAG: Pattern/Local:                      TOP_IF_2;
