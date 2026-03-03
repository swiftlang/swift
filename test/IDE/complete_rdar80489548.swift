// RUN: %batch-code-completion

// KW_IN: Keyword[in]/None/Flair[CommonKeyword]: in{{; name=.+$}}
// KW_NO_IN-NOT: Keyword[in]

func test(value: [Int]) {
  value.map { #^NOIN_IMMEDIATE?check=KW_IN^# }

  value.map { value#^NOIN_AFTER_EXPR_NOSPCACE?check=KW_NO_IN^# }
  value.map { value #^NOIN_AFTER_EXPR?check=KW_IN^# }
  value.map { value
    #^NOIN_NEWLINE?check=KW_IN^#
  }

  value.map { value in #^IN_AFTER_IN?check=KW_NO_IN^# }
  value.map { value in
    #^IN_NEWLINE?check=KW_NO_IN^#
  }

  #^FUNCBODY_STMT?check=KW_NO_IN^#
  value #^FUNCBODY_POSTFIX?check=KW_NO_IN^#
}

#^GLOBAL_STMT?check=KW_NO_IN^#
value #^GLOBAL_POSTFIX?check=KW_NO_IN^#
