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
                { unparsed_string = "/a var5=4"
                , unparsed_pos =
                    LP { lpLine = 5 , lpColumn = 1 , lpStartByte = 98 , lpLength = 13 }
                }
          , stmt_pos =
              LP { lpLine = 5 , lpColumn = 1 , lpStartByte = 98 , lpLength = 13 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "/A \"var6=6\""
                , unparsed_pos =
                    LP
                      { lpLine = 6 , lpColumn = 1 , lpStartByte = 112 , lpLength = 15 }
                }
          , stmt_pos =
              LP
                { lpLine = 6 , lpColumn = 1 , lpStartByte = 112 , lpLength = 15 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "/p var7=Please input"
                , unparsed_pos =
                    LP
                      { lpLine = 7 , lpColumn = 1 , lpStartByte = 128 , lpLength = 24 }
                }
          , stmt_pos =
              LP
                { lpLine = 7 , lpColumn = 1 , lpStartByte = 128 , lpLength = 24 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\"var\""
                , unparsed_pos =
                    LP { lpLine = 8 , lpColumn = 1 , lpStartByte = 153 , lpLength = 9 }
                }
          , stmt_pos =
              LP { lpLine = 8 , lpColumn = 1 , lpStartByte = 153 , lpLength = 9 }
          }
      , Set
          { stmt_set =
              Unparsed
                { unparsed_string = "\""
                , unparsed_pos =
                    LP { lpLine = 9 , lpColumn = 1 , lpStartByte = 163 , lpLength = 5 }
                }
          , stmt_pos =
              LP { lpLine = 9 , lpColumn = 1 , lpStartByte = 163 , lpLength = 5 }
          }
      ]
  , program_pos =
      LP { lpLine = 1 , lpColumn = 1 , lpStartByte = 0 , lpLength = 168 }
  }
