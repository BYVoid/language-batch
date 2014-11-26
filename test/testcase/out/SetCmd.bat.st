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
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "/a  var9=4"
                , unparsed_pos =
                    LP
                      { lpLine = 9 , lpColumn = 1 , lpStartByte = 153 , lpLength = 14 }
                }
          , stmt_pos =
              LP
                { lpLine = 9 , lpColumn = 1 , lpStartByte = 153 , lpLength = 14 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "/A \"var10=6\""
                , unparsed_pos =
                    LP
                      { lpLine = 10 , lpColumn = 1 , lpStartByte = 168 , lpLength = 16 }
                }
          , stmt_pos =
              LP
                { lpLine = 10 , lpColumn = 1 , lpStartByte = 168 , lpLength = 16 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "/p\tvar11=Please input"
                , unparsed_pos =
                    LP
                      { lpLine = 11 , lpColumn = 1 , lpStartByte = 185 , lpLength = 25 }
                }
          , stmt_pos =
              LP
                { lpLine = 11 , lpColumn = 1 , lpStartByte = 185 , lpLength = 25 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var12= Con%var1% cat %var2%"
                , unparsed_pos =
                    LP
                      { lpLine = 12 , lpColumn = 1 , lpStartByte = 211 , lpLength = 31 }
                }
          , stmt_pos =
              LP
                { lpLine = 12 , lpColumn = 1 , lpStartByte = 211 , lpLength = 31 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "var"
                , unparsed_pos =
                    LP
                      { lpLine = 13 , lpColumn = 1 , lpStartByte = 243 , lpLength = 7 }
                }
          , stmt_pos =
              LP
                { lpLine = 13 , lpColumn = 1 , lpStartByte = 243 , lpLength = 7 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var1\""
                , unparsed_pos =
                    LP
                      { lpLine = 14 , lpColumn = 1 , lpStartByte = 251 , lpLength = 10 }
                }
          , stmt_pos =
              LP
                { lpLine = 14 , lpColumn = 1 , lpStartByte = 251 , lpLength = 10 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\""
                , unparsed_pos =
                    LP
                      { lpLine = 15 , lpColumn = 1 , lpStartByte = 262 , lpLength = 5 }
                }
          , stmt_pos =
              LP
                { lpLine = 15 , lpColumn = 1 , lpStartByte = 262 , lpLength = 5 }
          }
      ]
  , program_pos =
      LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 267 }
  }
