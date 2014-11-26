Program
  { program_stmts =
      [ Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var1=Hello world"
                , unparsed_pos =
                    LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 20 }
                }
          , stmt_pos =
              LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 20 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var2=\"Hello world 2\""
                , unparsed_pos =
                    LP { lpLine = 2 , lpColumn = 1 , lpStartByte = 21 , lpLength = 24 }
                }
          , stmt_pos =
              LP { lpLine = 2 , lpColumn = 1 , lpStartByte = 21 , lpLength = 24 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var3=Hello world 3\""
                , unparsed_pos =
                    LP { lpLine = 3 , lpColumn = 1 , lpStartByte = 46 , lpLength = 24 }
                }
          , stmt_pos =
              LP { lpLine = 3 , lpColumn = 1 , lpStartByte = 46 , lpLength = 24 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var4=\"Hello world 4\"\""
                , unparsed_pos =
                    LP { lpLine = 4 , lpColumn = 1 , lpStartByte = 71 , lpLength = 26 }
                }
          , stmt_pos =
              LP { lpLine = 4 , lpColumn = 1 , lpStartByte = 71 , lpLength = 26 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var5=wtf"
                , unparsed_pos =
                    LP { lpLine = 5 , lpColumn = 1 , lpStartByte = 98 , lpLength = 13 }
                }
          , stmt_pos =
              LP { lpLine = 5 , lpColumn = 1 , lpStartByte = 98 , lpLength = 13 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var6="
                , unparsed_pos =
                    LP
                      { lpLine = 6 , lpColumn = 1 , lpStartByte = 112 , lpLength = 10 }
                }
          , stmt_pos =
              LP
                { lpLine = 6 , lpColumn = 1 , lpStartByte = 112 , lpLength = 10 }
          }
      , DoubleColonComment
          { stmt_comment = "set var7=^("
          , stmt_pos =
              LP
                { lpLine = 7 , lpColumn = 1 , lpStartByte = 123 , lpLength = 13 }
          }
      , DoubleColonComment
          { stmt_comment = "set \"var8=^(\""
          , stmt_pos =
              LP
                { lpLine = 8 , lpColumn = 1 , lpStartByte = 137 , lpLength = 15 }
          }
      , DoubleColonComment
          { stmt_comment = "set /a var9=4"
          , stmt_pos =
              LP
                { lpLine = 9 , lpColumn = 1 , lpStartByte = 153 , lpLength = 15 }
          }
      , DoubleColonComment
          { stmt_comment = "set /A \"var10=6\""
          , stmt_pos =
              LP
                { lpLine = 10 , lpColumn = 1 , lpStartByte = 169 , lpLength = 18 }
          }
      , DoubleColonComment
          { stmt_comment = "set /p var11=Please input"
          , stmt_pos =
              LP
                { lpLine = 11 , lpColumn = 1 , lpStartByte = 188 , lpLength = 27 }
          }
      , DoubleColonComment
          { stmt_comment = "set \"var\""
          , stmt_pos =
              LP
                { lpLine = 12 , lpColumn = 1 , lpStartByte = 216 , lpLength = 11 }
          }
      , DoubleColonComment
          { stmt_comment = "set \""
          , stmt_pos =
              LP
                { lpLine = 13 , lpColumn = 1 , lpStartByte = 228 , lpLength = 7 }
          }
      ]
  , program_pos =
      LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 235 }
  }
